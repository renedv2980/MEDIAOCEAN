*          DATA SET NEMED25    AT LEVEL 069 AS OF 05/01/02                      
*PHASE T31E25A                                                                  
         TITLE 'T31E25 - NETWORK CALENDAR'                                      
T31E25   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CAPR**                                                       
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING T31E25+4096,R6                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          USE W/S AREA 2 FOR LOCAL W/S                 
         USING CALD,R7                                                          
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         MVI   MAXLINES,62                                                      
         SPACE 1                                                                
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         ST    R1,RELO                                                          
         SPACE 1                                                                
         BAS   RE,PRODSTAK                                                      
         BAS   RE,SETGROUP                                                      
         SPACE 1                                                                
         L     R2,=A(SORTPOOL)                                                  
         A     R2,RELO                                                          
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(40,(R2))                           
         EJECT                                                                  
*              INITIALIZE NETBLOCK AND CONTROL IO                               
         SPACE 3                                                                
         MVI   NBACTOPT,C'Y'       GET ACTUAL AND                               
         MVI   NBESTOPT,C'A'                                                    
         MVI   NBDATA,C'U'         PROCESS UNITS                                
*                                                                               
         CLI   TIMEOPT,C'Y'        .IF TIMEOPT = Y                              
         BNE   *+14                                                             
         CLC   NBSELPRD,=C'POL'    .AND IF PRD = POL                            
         BE    *+8                 .THEN DON'T DO SPLITS                        
*                                                                               
         OI    NBSPLOPT,X'80'      TURN ON SPLIT OPTION                         
         MVI   NBSELUOP,C'A'       ACTUAL SKED-OMIT PRE-EMPTS/MISSING           
         SPACE 1                                                                
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK   PROCESS DATE                             
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GOTDATE                                                          
         B     PROCDAT                                                          
         SPACE 1                                                                
GOTDATE  XC    PERTYPE,PERTYPE                                                  
         MVI   PERTYPE,C'M'        GET MONTHS IN LIST                           
         LA    R4,MAXMONTS                                                      
         ST    R4,NUMMONS          MAX NUMBER OF MONTHS                         
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE  GET LIST                   
         SPACE 1                                                                
***** IF MONS > MAXMONT THEN 1ST DATE IN MONLIST IS THE START DATE.             
*****  FOR CLIENT=ALL NBCMPSTR GETS RECALCULATED EACH CLIENT. SO                
*****  ALWAYS MOVE IT IN HERE, JUST TO BE SAFE.                                 
         SPACE 1                                                                
GETUNIT  MVC   NBCMPSTR(2),MONLIST                                              
         SPACE 1                                                                
         NETGO NSNETIO,DMCB,NETBLOCK    NOW DO UNIT RECORDS                     
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST     LAST ONE                                     
         BE    LASTONE                                                          
         CLI   NBMODE,NBPROCUN     IF A UNIT                                    
         BE    GOTUNIT                                                          
         B     GETUNIT                                                          
         SPACE 1                                                                
GOTUNIT  BAS   RE,FILTGRUP         CHECK PRODUCT GROUP                          
         BNE   GETUNIT                                                          
         BAS   RE,PUTUNIT          PROCESS UNIT                                 
         B     GETUNIT                                                          
         SPACE 1                                                                
LASTONE  BAS   RE,GETSORT                                                       
         XIT1                                                                   
         SPACE 1                                                                
PROCERR  DC    H'0'                                                             
         EJECT                                                                  
*              BUILD A STACK OF PRODUCT NAMES/CODES                             
         SPACE 3                                                                
PRODSTAK NTR1                                                                   
         SPACE 1                                                                
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT RECORDS                  
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRDHDR,R4                                                        
         MVC   PKEYAM(3),NBACTAM                                                
         SPACE 1                                                                
PS2      AI    PKEYPRD+2,1                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PS4                                                              
         LA    R4,IO                                                            
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         ZIC   R1,PCODE+1          USE CODE TO DISPLACE INTO POOL               
         BCTR  R1,0                                                             
         MH    R1,=H'24'                                                        
         L     R2,=A(PRDPOOL)                                                   
         A     R2,RELO                                                          
         AR    R2,R1                                                            
         MVC   0(1,R2),PCODE+1     SAVE CODE                                    
         MVC   1(3,R2),PKEYPRD     NMEMONIC                                     
         MVC   4(20,R2),PNAME      AND NAME                                     
         LA    R4,KEY                                                           
         B     PS2                                                              
         SPACE 1                                                                
PS4      LA    R1,254              SET UP FE AS UNA                             
         BCTR  R1,0                                                             
         MH    R1,=H'24'                                                        
         L     R2,=A(PRDPOOL)                                                   
         A     R2,RELO                                                          
         AR    R2,R1                                                            
         MVI   0(R2),X'FE'                                                      
         MVC   1(23,R2),=CL23'UNAUNALLOCATED'                                   
         CLI   NOPRDOPT,C'N'                                                    
         BNE   *+10                                                             
         MVC   1(23,R2),SPACES                                                  
         SPACE 1                                                                
         NETGO NVSETUNT,DMCB       RESET TO UNIT FILE                           
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP PRODUCT GROUP                                  
         SPACE 3                                                                
SETGROUP NTR1                                                                   
         CLI   SPLPRO+1,C'='                                                    
         BNE   XIT                                                              
         SPACE 1                                                                
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT RECORDS                  
         USING PRDHDR,R4                                                        
         LA    R4,KEY                                                           
         L     R2,=A(GROUPLST)                                                  
         A     R2,RELO                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),NBACTAM                                                 
         SPACE 1                                                                
SG2      LA    R4,KEY                                                           
         AI    PKEYPRD+2,1         SKIP TO NEXT PRODUCT                         
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   SG4                                                              
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         MVC   0(1,R2),PCODE+1     PRODUCT CODE                                 
         MVC   1(3,R2),PGRP1       FIND MATCH OF PRODUCT GROUP SCHEME           
         CLC   SPLPRO(1),1(R2)     MAY BE FIRST                                 
         BE    SG3                                                              
         MVC   1(3,R2),PGRP2       OR SECOND                                    
         CLC   SPLPRO(1),1(R2)                                                  
         BE    SG3                                                              
         MVC   1(3,R2),PGRP3       OR THIRD                                     
         CLC   SPLPRO(1),1(R2)                                                  
         BE    SG3                                                              
         XC    1(3,R2),1(R2)                                                    
         SPACE 1                                                                
SG3      LA    R2,4(R2)                                                         
         B     SG2                                                              
         SPACE 1                                                                
SG4      MVI   0(R2),X'FF'                                                      
         LA    R4,KEY              GET SCHEME RECORD                            
         XC    KEY,KEY                                                          
         USING PRGKEY,R4                                                        
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD(3),NBACTAM                                              
         MVC   PRGKID,SPLPRO                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 READ                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         LA    R2,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING PRGEL01,R2                                                       
         MVC   BREAK,PRGBK1        DIG OUT BREAK NAME AND LENGTH                
         MVC   BRLEN,PRGBK1LN                                                   
         CLI   PRGBK2LN,0                                                       
         BE    XITSG                                                            
         MVC   BREAK,PRGBK2                                                     
         AC    BRLEN,PRGBK2LN                                                   
         SPACE 1                                                                
XITSG    NETGO NVSETUNT,DMCB       RESET TO READ UNIT FILE                      
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ESTABLISH PRODUCT GROUP FOR SPOT                      
*              AND TO FILTER ON GROUP WHERE APPLICABLE                          
         SPACE 3                                                                
FILTGRUP NTR1                                                                   
         CLI   SPLPRO+1,C'='                                                    
         BNE   YES                                                              
         CLI   NBSPLPRN,0          UNALLOCATED MISSES                           
         BE    NO                                                               
         CLI   NBSPLPRN,X'FF'                                                   
         BE    NO                                                               
         L     R2,=A(GROUPLST)                                                  
         A     R2,RELO                                                          
         SPACE 1                                                                
FG2      CLC   NBSPLPRN,0(R2)                                                   
         BE    FG6                                                              
         SPACE 1                                                                
FG4      LA    R2,4(R2)                                                         
         B     FG2                                                              
         SPACE 1                                                                
FG6      MVC   SINGROUP,1(R2)                                                   
         CLC   SPLPRO+2(3),=C'ALL'                                              
         BE    YES                                                              
         UNPK  WORK(5),2(3,R2)                                                  
         LA    R2,WORK                                                          
         LA    R1,SPLPRO+2                                                      
         LA    R0,3                                                             
         SPACE 1                                                                
FG8      CLI   0(R1),C'A'          FILTER SPECIFIC GROUP                        
         BL    FG10                                                             
         CLC   0(1,R1),0(R2)                                                    
         BNE   NO                                                               
         SPACE 1                                                                
FG10     LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,FG8                                                           
         B     YES                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         B     *+6                                                              
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              PUT SPOT DETAILS TO A SORT RECORD                                
         SPACE 3                                                                
PUTUNIT  NTR1                                                                   
         OC    NBACTUN,NBACTUN                                                  
         BZ    XIT                                                              
         XC    SRTREC,SRTREC                                                    
         LA    R1,MONLIST          FIND MONTH THIS UNIT BELONGS TO              
         SPACE 1                                                                
MONLOOP  CLC   NBACTDAT(2),2(R1)                                                
         BNH   GOTMON                                                           
         LA    R1,4(R1)                                                         
         B     MONLOOP                                                          
         SPACE 1                                                                
GOTMON   MVC   SRTMONTH,0(R1)                                                   
         MVC   SRTDP,NBDPNAM                                                    
         MVC   SRTDATE,NBACTDAT                                                 
         MVC   SRTNET,NBACTNET                                                  
         MVC   SRTPROG,NBPROGNM                                                 
         OC    SRTPROG,SPACES                                                   
         MVC   SRTBRAND,NBPRD                                                   
         MVC   SRTIME,NBTIME                                                    
         MVC   SRTSPTLN,NBLEN                                                   
         CLI   NBSPLPRN,0                                                       
         BE    GOTMON2                                                          
         CLI   NBSPLPRN,X'FF'                                                   
         BE    GOTMON2                                                          
         MVC   SRTBRAND,NBSPLPRN                                                
         SPACE 1                                                                
GOTMON2  MVC   SRTUNITS,NBACTUN+1  DEFAULT IS TO SHOW ACTUAL DEMOS              
         MVC   SRTGRPS,NDACTDEM+2                                               
         CLI   SPLDOPT,C'E'        OPTION TO SHOW ESTIMATED DEMOS               
         BNE   GOTMON4                                                          
         MVC   SRTGRPS,NDESTDEM+2                                               
         SPACE 1                                                                
GOTMON4  CLI   SRTUNITS,0                                                       
         BE    XIT                                                              
         CLC   SRTNET(3),=C'ABC'                                                
         BNE   *+14                                                             
         MVC   SRTNET(4),=X'00000001'                                           
         B     PU2                                                              
         CLC   SRTNET(3),=C'CBS'                                                
         BNE   *+14                                                             
         MVC   SRTNET(4),=X'00000002'                                           
         B     PU2                                                              
         CLC   SRTNET(3),=C'NBC'                                                
         BNE   *+14                                                             
         MVC   SRTNET(4),=X'00000003'                                           
         B     PU2                                                              
         CLC   SRTNET(3),=C'FOX'                                                
         BNE   *+14                                                             
         MVC   SRTNET(4),=X'00000004'                                           
         B     PU2                                                              
         CLC   SRTNET(3),=C'FBC'                                                
         BE    *-16                                                             
         SPACE 1                                                                
PU2      CLC   SPLDPT(3),=C'TOG'   OPTION TO HAVE DAYPARTS TOGETHER             
         BNE   *+10                                                             
         MVC   SRTDP,SPACES                                                     
         CLI   SRTBRAND,0                                                       
         BNE   *+8                                                              
         MVI   SRTBRAND,X'FE'                                                   
         CLI   SRTBRAND,X'FF'                                                   
         BNE   *+8                                                              
         MVI   SRTBRAND,X'FE'                                                   
         MVC   SRTKPRD,SINGROUP    OPTION TO RUN BY PRODUCT GROUP               
         CLI   SPLPRO+1,C'='                                                    
         BE    PU4                                                              
         XC    SRTKPRD,SRTKPRD                                                  
         CLC   SPLPRO(3),=C'ALL'   OPTION TO SHOW BRANDS SEPARATELY             
         BNE   PU4                                                              
         ZIC   R1,SRTBRAND                                                      
         BCTR  R1,0                                                             
         MH    R1,=H'24'                                                        
         L     R2,=A(PRDPOOL)                                                   
         A     R2,RELO                                                          
         AR    R2,R1                                                            
         MVC   SRTKPRD,1(R2)                                                    
         SPACE 1                                                                
PU4      CLC   SPLNET(3),=C'ALL'   OPTION TO SHOW NETWORKS SEPARATELY           
         BNE   PU6                                                              
         MVC   SRTKNET,SRTNET                                                   
         SPACE 1                                                                
PU6      CLC   SPLEST(3),=C'ALL'   OPTION TO SHOW ESTIMATES SEPARATELY          
         BNE   PU8                                                              
         LA    R4,KEY                                                           
         MVC   SRTKEST,NBACTEST                                                 
         SPACE 1                                                                
PU8      GOTO1 SORTER,DMCB,=C'PUT',SRTREC                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL CALENDAR PRINTING                             
         SPACE 3                                                                
GETSORT  NTR1                                                                   
         XC    SRTREC,SRTREC                                                    
         XC    NETLIST,NETLIST                                                  
         SPACE 1                                                                
GS2      MVC   LSORT,SRTKEY                                                     
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,DMCB+4                                                        
         LTR   R2,R2               EOF                                          
         BNZ   GS4                                                              
         BAS   RE,PRINTCAL         PRINT LAST CALENDAR                          
         B     XIT                                                              
         SPACE 1                                                                
GS4      DS    0H                                                               
         MVC   SRTREC,0(R2)                                                     
*                                                                               
         CLC   SRTNET,=X'00000001'                                              
         BNE   *+14                                                             
         MVC   SRTNET,=C'ABC '                                                  
         B     GS4A                                                             
         CLC   SRTNET,=X'00000002'                                              
         BNE   *+14                                                             
         MVC   SRTNET,=C'CBS '                                                  
         B     GS4A                                                             
         CLC   SRTNET,=X'00000003'                                              
         BNE   *+14                                                             
         MVC   SRTNET,=C'NBC '                                                  
         B     GS4A                                                             
         CLC   SRTNET,=X'00000004'                                              
         BNE   GS4A                                                             
         MVC   SRTNET,=C'FOX '                                                  
GS4A     OC    LSORT,LSORT         IS IT FIRST TIME                             
         BZ    GS4C                                                             
         CLC   SRTKEY(22),LSORT    CHANGE MONTH/DAYPART/DAY                     
         BNE   GS4C                                                             
         LA    R3,NETLIST          IS NETWRK ALLREADY IN NETLIST                
         LA    R0,5                                                             
         CLC   0(4,R3),SRTNET                                                   
         BE    GS4C                YES/CONTINUE                                 
         LA    R3,4(R3)                                                         
         BCT   R0,*-14                                                          
         LA    R3,NETLIST          NO/SET IT IN                                 
         LA    R0,4                   NETS OVER 4 = OTHER                       
NETLOOP  OC    0(4,R3),0(R3)                                                    
         BNZ   *+14                                                             
         MVC   0(4,R3),SRTNET                                                   
         B     GS4C                                                             
         LA    R3,4(R3)                                                         
         BCT   R0,NETLOOP                                                       
         MVI   ASTERIX,C'Y'                                                     
         MVC   0(4,R3),=C'OTH '                                                 
         MVC   SRTNET,=C'OTH '                                                  
         ZIC   R1,SRTUNITS                                                      
         A     R1,OTHUNITS                                                      
         ST    R1,OTHUNITS                                                      
*                                                                               
GS4C     DS    0H                                                               
         CLC   SRTKEY(20),LSORT    CHANGE OF MONTH/DAYPART                      
         BE    GS8                                                              
         OC    LSORT,LSORT                                                      
         BZ    GS6                                                              
         BAS   RE,PRINTCAL                                                      
         SPACE 1                                                                
GS6      BAS   RE,CALINIT          INITIALIZE FOR NEW MONTH/DAYPART             
         SPACE 1                                                                
GS8      CLC   SRTKEY(22),LSORT    CHANGE OF DAY                                
         BE    GS10                                                             
         XC    ABCLIST,ABCLIST     RESET COUNTERS                               
         XC    CBSLIST,CBSLIST                                                  
         XC    NBCLIST,NBCLIST                                                  
         XC    FOXLIST,FOXLIST                                                  
         XC    XXXLIST,XXXLIST                                                  
         XC    OTHLIST,OTHLIST                                                  
         MVI   COUNT,0                                                          
         MVI   ASTERIX,0                                                        
         XC    OTHUNITS,OTHUNITS                                                
         XC    NETCOUNT,NETCOUNT                                                
         XC    NETLIST,NETLIST                                                  
         LA    R3,NETLIST                                                       
         MVC   0(4,R3),SRTNET                                                   
         SPACE 1                                                                
GS10     BAS   RE,CALPOST                                                       
         B     GS2                                                              
         EJECT                                                                  
*              INITIALIZE FOR NEW MONTH/DAYPART                                 
         SPACE 3                                                                
CALINIT  NTR1                                                                   
         MVI   MAXLINES,100        I'M CONTROLLING THE HEADLINES                
         MVC   MYHEAD,SPACES                                                    
         MVC   MYHEAD(8),SRTDP                                                  
         CLC   SRTDP,SPACES                                                     
         BE    *+8                                                              
         MVI   MYHEAD+9,C'-'                                                    
         MVC   PRDHEAD,SPACES                                                   
         CLC   SPLPRO(3),=C'POL'                                                
         BE    CIB                                                              
         CLI   SPLPRO+1,C'='       HANDLE PRODUCT GROUP                         
         BNE   CIAA                                                             
         MVC   PRDHEAD(12),BREAK                                                
         MVC   PRDHEAD+13(1),SPLPRO                                             
         UNPK  WORK(5),SRTKPRD+1(3)                                             
         ZIC   R1,BRLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     CIA                                                              
         MVC   PRDHEAD+14(0),WORK                                               
         SPACE 1                                                                
CIAA     ZIC   R1,SRTBRAND                                                      
         BCTR  R1,0                                                             
         MH    R1,=H'24'                                                        
         L     R2,=A(PRDPOOL)                                                   
         A     R2,RELO                                                          
         AR    R2,R1                                                            
         MVC   PRDHEAD,4(R2)                                                    
         SPACE 1                                                                
CIA      OC    PRDHEAD,SPACES                                                   
         GOTO1 CENTER,DMCB,PRDHEAD,20                                           
         SPACE 1                                                                
CIB      MVC   ESTHEAD,SPACES                                                   
         CLC   SPLEST(3),=C'ALL'                                                
         BNE   CID                                                              
         MVC   ESTHEAD(8),=C'ESTIMATE'                                          
         EDIT  (1,SRTKEST),(3,DMCB),ALIGN=LEFT                                  
         MVC   ESTHEAD+9(3),DMCB                                                
         SPACE 1                                                                
CID      GOTO1 DATCON,DMCB,(2,SRTMONTH),(0,THISTART)                            
         GOTO1 DATCON,DMCB,(2,SRTMONTH+2),(0,THISEND)                           
         MVC   DUB,THISTART                                                     
         CLI   NBUSER+3,C'C'       CALENDAR OR BRODCAST MON                     
         BE    CI1                                                              
         GOTO1 ADDAY,DMCB,THISTART,DUB,7                                        
         SPACE 1                                                                
CI1      GOTO1 DATCON,DMCB,(0,DUB),(6,MYBROAD)                                  
         GOTO1 GETDAY,DMCB,THISTART,DUB                                         
         CLI   DMCB,1                                                           
         BE    CI2                                                              
         ZIC   R2,DMCB                                                          
         LA    R3,1                                                             
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,THISTART,DUB,(R3)                                     
         MVC   THISTART,DUB                                                     
         SPACE 1                                                                
CI2      MVC   MYHEAD+11(6),MYBROAD                                             
         GOTO1 SQUASHER,DMCB,MYHEAD,20                                          
         GOTO1 CENTER,DMCB,MYHEAD,20                                            
         GOTO1 UNDERLIN,DMCB,(20,MYHEAD),MYUNDER                                
         MVC   DAYLIST(132),SPACES 42X7=294                                     
         MVC   DAYLIST+132(132),SPACES                                          
         MVC   DAYLIST+264(30),SPACES                                           
         SR    R3,R3               COUNT DAYS                                   
         MVC   WORK,THISTART                                                    
         LA    R2,DAYLIST                                                       
         SPACE 1                                                                
CI4      GOTO1 DATCON,DMCB,WORK,(2,0(R2))                                       
         GOTO1 DATCON,DMCB,WORK,(4,2(R2))                                       
         CLC   2(3,R2),MYBROAD                                                  
         BNE   *+10                                                             
         MVC   2(3,R2),SPACES                                                   
         LA    R3,1(R3)                                                         
         CLC   WORK(6),THISEND                                                  
         BE    CI6                                                              
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         MVC   WORK(6),WORK+6                                                   
         LA    R2,7(R2)                                                         
         B     CI4                                                              
         SPACE 1                                                                
CI6      STC   R3,NDAYS            SET UP SOFT ROWS                             
         MVC   MYROWS,=CL100'     T M         B'                                
         LA    R4,MYROWS+17                                                     
         CLI   NDAYS,36            CAN HAVE 6 WEEKS                             
         BL    CI8                                                              
         MVI   0(R4),C' '                                                       
         SH    R4,=H'2'                                                         
         SPACE 1                                                                
CI7      CH    R3,=H'7'                                                         
         BNH   CI10                                                             
         MVI   0(R4),C'M'                                                       
         MVI   8(R4),C'B'                                                       
         LA    R4,8(R4)                                                         
         SH    R3,=H'7'                                                         
         B     CI7                                                              
         SPACE 1                                                                
CI8      CH    R3,=H'7'                                                         
         BNH   CI10                                                             
         MVI   0(R4),C'M'                                                       
         MVI   10(R4),C'B'                                                      
         LA    R4,10(R4)                                                        
         SH    R3,=H'7'                                                         
         B     CI8                                                              
         SPACE 1                                                                
CI10     L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         L     R1,BOXAWIDE                                                      
         USING WIDED,R1                                                         
         MVC   XP(165),MYDAYS                                                   
         DROP  R1                                                               
         MVI   SPACING,2                                                        
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,DAYLIST                                                       
         L     R2,=A(SLOTS)        CLEAR THE SLOTS                              
         A     R2,RELO                                                          
         LA    R4,42                                                            
         SPACE 1                                                                
CI12     MVI   0(R2),C' '                                                       
         MVC   1(197,R2),0(R2)                                                  
         MVC   17(5,R2),2(R3)      MOVE DATE INTO TOP RIGHT OF SLOT             
         A     R2,LSLOT                                                         
         LA    R3,7(R3)                                                         
         BCT   R4,CI12                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL POSTING TO DAY SLOTS - FIND DAY               
         SPACE 3                                                                
CALPOST  NTR1                                                                   
         CLC   SPLPRO(3),=C'POL'                                                
         BE    CPB                                                              
         CLI   SPLPRO+1,C'='                                                    
         BE    CPB                                                              
         CLI   SPLDEMH+5,0                                                      
         BNE   CPB                                                              
         CLI   TIMEOPT,C'Y'                                                     
         BE    CPB                                                              
         BAS   RE,ONEBRAND         SPECIAL ROUTINE FOR SINGLE BRAND             
         B     CPD                                                              
         SPACE 1                                                                
CPB      BAS   RE,CALMERGE         MERGE INTO DAY LISTS                         
         BAS   RE,CALSLOT          AND EDIT INTO MYSLOT                         
         SPACE 1                                                                
CPD      L     R2,=A(SLOTS)                                                     
         A     R2,RELO                                                          
         LA    R3,DAYLIST                                                       
         SPACE 1                                                                
CP2      CLC   0(2,R3),SRTDATE     LOCATE CORRECT DAY                           
         BE    CP4                                                              
         A     R2,LSLOT                                                         
         LA    R3,7(R3)                                                         
         B     CP2                                                              
         SPACE 1                                                                
CP4      MVC   22(176,R2),MYSLOT                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MERGE INTO DAYLISTS                                   
         SPACE 3                                                                
CALMERGE NTR1                                                                   
         LA    R2,NETLIST                                                       
         LA    R3,ABCLIST                                                       
         LA    R4,NETCOUNT                                                      
         LA    R5,5                                                             
         SPACE 1                                                                
CM2      CLC   0(4,R2),SRTNET                                                   
         BE    CM4                                                              
         LA    R2,4(R2)                                                         
         LA    R3,160(R3)                                                       
         LA    R4,1(R4)                                                         
         BCT   R5,CM2                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
CM4      CLC   16(16,R3),=CL16'VARIOUS'                                         
         BNE   CM5                                                              
         BAS   RE,BMERGE           ALREADY VARIOUS - POST HERE                  
         B     XIT                                                              
         SPACE 1                                                                
CM5      CLC   LSORT(42),SRTKEY    IS THIS A DIFFERENT PROGRAM                  
         BE    CM6                                                              
         CLI   0(R4),4             4 ALREADY - GOING INTO OTHERS                
         BE    CM6                                                              
         CLC   =C'OTH ',SRTNET     DO NOT COUNT OTH/NOT PRINTED                 
         BE    CM6                                                              
         AI    0(R4),1                                                          
         AI    COUNT,1                                                          
         SPACE 1                                                                
CM6      ZIC   R1,0(R4)            DISPLACE TO SLOT WITHIN NETWORK              
         BCTR  R1,0                0,1,2 FOR PROGRAMS, 3 FOR OTHERS             
         SLL   R1,5                (X32)                                        
         AR    R3,R1                                                            
         MVC   16(16,R3),SRTPROG                                                
         CLI   0(R4),4                                                          
         BL    *+10                                                             
         MVC   16(16,R3),=CL16'OTHERS'                                          
         CLI   TIMEOPT,C'Y'                                                     
         BNE   CM8                                                              
         MVC   5(4,R3),SRTIME                                                   
         MVC   9(1,R3),SRTSPTLN                                                 
CM8      BAS   RE,BMERGE                                                        
         SPACE 1                                                                
         SR    R3,R1                                                            
         LA    R3,128(R3)          4 FOR VARIOUS (TOTAL)                        
         MVC   16(16,R3),=CL16'VARIOUS'                                         
         BAS   RE,BMERGE                                                        
         B     XIT                                                              
         EJECT                                                                  
*              NOW FILL MYSLOT FROM LISTS                                       
         SPACE 3                                                                
CALSLOT  NTR1                                                                   
         MVI   SLOTMAX,4           CAN FIT 4 INTO MYSLOT                        
         CLI   NDAYS,36            UNLESS ITS A 6 WEEK MONTH                    
         BL    CS2                                                              
         MVI   SLOTMAX,3           WHEN ALL I CAN HANDLE IS 3                   
         SPACE 1                                                                
CS2      CLC   COUNT,SLOTMAX                                                    
         BNH   CS4                                                              
         MVC   PRECOUNT,COUNT                                                   
         BAS   RE,DAYTRIM          TRIM DOWN UNTIL ITS OK                       
         CLC   PRECOUNT,COUNT                                                   
         BNE   CS2                                                              
         SPACE 1                                                                
CS3      MVI   NETCOUNT+3,0                                                     
         XC    OTHLIST,OTHLIST                                                  
         MVC   COUNT,SLOTMAX                                                    
         SPACE 1                                                                
CS4      DS    0H                                                               
         LA    R2,MYSLOT                                                        
         LA    R3,ABCLIST                                                       
         LA    R4,NETCOUNT                                                      
         LA    R5,NETLIST                                                       
         LA    R0,4                                                             
         MVC   MYSLOT(132),SPACES                                               
         MVC   MYSLOT+132(44),SPACES                                            
         SPACE 1                                                                
CS6      BAS   RE,CS8                                                           
         LA    R5,4(R5)                                                         
         LA    R3,160(R3)                                                       
         LA    R4,1(R4)                                                         
         BCT   R0,CS6                                                           
         CLI   ASTERIX,C'Y'                                                     
         BNE   CSX                                                              
         A     R2,=F'-22'                                                       
         MVI   0(R2),C'*'                                                       
         EDIT  (B4,OTHUNITS),(1,1(R2)),ALIGN=LEFT                               
CSX      B     XIT                                                              
         SPACE 1                                                                
CS8      NTR1                                                                   
         ZIC   R0,0(R4)            COUNT FOR THIS NETWORK                       
         LTR   R0,R0                                                            
         BZ    XIT                                                              
         MVC   0(4,R2),0(R5)       SHOW NETWORK ON FIRST ACTIVE LINE            
         SPACE 1                                                                
CS10     MVC   4(16,R2),16(R3)     SHOW PROGRAM                                 
         CLI   TIMEOPT,C'Y'                                                     
         BNE   CS12                                                             
         CLC   =C'VARIOUS',4(R2)                                                
         BNE   CS10A                                                            
         MVC   UNITLIST,0(R3)                                                   
         ZIC   R4,UNITLIST+1       SHOW AS VARIOUS (NN)                         
         ZIC   R5,UNITLIST+5                                                    
         AR    R5,R4                                                            
         ZIC   R4,UNITLIST+9                                                    
         AR    R5,R4                                                            
         ZIC   R4,UNITLIST+13                                                   
         AR    R5,R4                                                            
*        ZIC   R4,UNITLIST+17                                                   
*        AR    R5,R4                                                            
         MVI   12(R2),C'('                                                      
         LR    R4,R0               EDIT AFFECTS R0/R0=BCT CONTROL               
         EDIT  (R5),(2,13(R2)),ALIGN=LEFT                                       
         LR    R0,R4                                                            
         CLI   14(R2),X'40'                                                     
         BH    *+12                                                             
         MVI   14(R2),C')'                                                      
         B     CS12+4                                                           
         MVI   15(R2),C')'                                                      
         B     CS12+4                                                           
CS10A    CLC   =C'OTHERS',4(R2)                                                 
         BE    *+8                                                              
         BAS   RE,TIMERTN                                                       
         B     *+8                                                              
CS12     BAS   RE,EDLIST           EDIT BRANDS                                  
         LA    R2,44(R2)                                                        
         CLC   COUNT,SLOTMAX                                                    
         BE    *+8                                                              
         LA    R2,22(R2)           WE CAN AFFORD A SPACE LINE                   
         LA    R3,32(R3)                                                        
         BCT   R0,CS10                                                          
         XIT1  REGS=(R2)                                                        
*                                                                               
TIMERTN  NTR1                                                                   
         CLI   9(R3),30         SPTLEN=30/LEAVE BLANK                           
         BNE   *+12                                                             
         S     R2,=F'4'                                                         
         B     TIM5                                                             
         EDIT  (B1,9(R3)),(2,26(R2))                                            
TIM5     MVI   30(R2),C'('                                                      
         GOTO1 UNTIME,DMCB,5(R3),31(R2)                                         
         LA    R1,31(R2)                                                        
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'40'                                                      
         BH    *-8                                                              
         MVI   0(R1),C')'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO TRIM DOWN LISTS, COUNTS                               
         SPACE 3                                                                
DAYTRIM  NTR1                                                                   
         LA    R2,4                SET N TO 4                                   
         SPACE 1                                                                
DT2      LA    R3,ABCLIST          LOOK FOR NETWORK WITH COUNT OF N             
         LA    R4,NETCOUNT                                                      
         LA    R0,4                                                             
         SPACE 1                                                                
DT4      ZIC   R1,0(R4)                                                         
         CR    R1,R2                                                            
         BE    DT6                                                              
         LA    R3,160(R3)                                                       
         LA    R4,1(R4)                                                         
         BCT   R0,DT4                                                           
         BCT   R2,DT2              LOWER N AND TRY AGAIN                        
         DC    H'0'                                                             
         SPACE 1                                                                
DT6      MVC   0(32,R3),128(R3)    GOT A HIT - MOVE IN TOTALS TO FIRST          
         XC    32(128,R3),32(R3)   POSITION AND CLEAR THE REST                  
         MVI   0(R4),1             MAKE THIS COUNT=1                            
         ZIC   R1,COUNT            ADJUST OVERALL COUNT                         
         SR    R1,R2                                                            
         LA    R1,1(R1)                                                         
         STC   R1,COUNT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              SPECIAL ROUTINE TO HANDLE ONE BRAND                              
         SPACE 3                                                                
ONEBRAND NTR1                                                                   
         CLI   COUNT,0             CLEAR FIRST TIME                             
         BNE   OB2                                                              
         MVC   MYSLOT(132),SPACES                                               
         MVC   MYSLOT+132(44),SPACES                                            
         SPACE 1                                                                
OB2      MVI   SLOTMAX,7           MAX 7                                        
         CLI   NDAYS,36                                                         
         BL    *+8                                                              
         MVI   SLOTMAX,5           OR 5 WITH 6-WEEK CAL                         
         CLC   LSORT(40),SRTKEY    NEW PROGRAM                                  
         BE    OB8                                                              
         AI    COUNT,1                                                          
         CLC   COUNT,SLOTMAX                                                    
         BE    OB4                                                              
*        BH    OB6                                                              
         BH    OB8                                                              
         MVC   PROGUNIT,SRTUNITS   NEW PROGRAM                                  
         ZIC   R2,COUNT                                                         
         BCTR  R2,0                                                             
         MH    R2,=H'22'                                                        
         LA    R2,MYSLOT(R2)                                                    
         MVC   4(16,R2),SRTPROG                                                 
         CLC   LSORT(24),SRTKEY                                                 
         BE    *+10                                                             
         MVC   0(4,R2),SRTNET                                                   
         B     OB10                                                             
         SPACE 1                                                                
OB4      ZIC   R2,COUNT            INITIALIZE LAST SLOT                         
         BCTR  R2,0                                                             
         MH    R2,=H'22'                                                        
         LA    R2,MYSLOT(R2)                                                    
         MVC   PROGUNIT,SRTUNITS                                                
         B     OB10                                                             
         SPACE 1                                                                
OB6      MVC   COUNT,SLOTMAX       OVER MAX                                     
         SPACE 1                                                                
OB8      ZIC   R2,COUNT                                                         
         BCTR  R2,0                                                             
         MH    R2,=H'22'                                                        
         LA    R2,MYSLOT(R2)                                                    
         ZIC   R1,PROGUNIT                                                      
         ZIC   R0,SRTUNITS                                                      
         AR    R1,R0                                                            
         STC   R1,PROGUNIT                                                      
         SPACE 1                                                                
OB10     MVC   4(16,R2),SRTPROG                                                 
         CLC   COUNT,SLOTMAX                                                    
         BNH   *+16                                                             
         MVC   0(22,R2),SPACES                                                  
         MVC   0(10,R2),=C'VAR OTHERS'                                          
         LA    R2,19(R2)           FIND END OF PROGRAM NAME                     
         SPACE 1                                                                
OB12     CLI   0(R2),C' '                                                       
         BNE   OB14                                                             
         BCT   R2,OB12                                                          
         SPACE 1                                                                
OB14     ZIC   R1,PROGUNIT                                                      
         CH    R1,=H'1'            IF MORE THAN 1 UNIT                          
         BE    XIT                                                              
         MVI   1(R2),C'='          FLOAT IN NUMBER                              
         EDIT  (R1),(2,2(R2)),ALIGN=LEFT                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MERGE TO BRAND LIST                                   
         SPACE 3                                                                
BMERGE   NTR1                                                                   
         LA    R4,3                R3=A(LIST)                                   
         MVC   BYTE,SRTBRAND                                                    
         CLI   SRTBRAND+1,0                                                     
         BE    BM2                                                              
         MVI   BYTE,X'FD'          CODE FOR P/B                                 
         SPACE 1                                                                
BM2      CLC   0(1,R3),BYTE                                                     
         BE    BM6                                                              
         CLI   0(R3),0                                                          
         BE    BM4                                                              
         LA    R3,4(R3)                                                         
         BCT   R4,BM2                                                           
         MVI   0(R3),X'FF'         MORE THAN 3 - FF=VARIOUS                     
         B     BM6                                                              
         SPACE 1                                                                
BM4      MVC   0(1,R3),BYTE        RECORD BRAND                                 
         SPACE 1                                                                
BM6      ZIC   R1,1(R3)            ADD IN UNITS                                 
         ZIC   R0,SRTUNITS                                                      
         AR    R1,R0                                                            
         STC   R1,1(R3)                                                         
         LH    R1,2(R3)                                                         
         AH    R1,SRTGRPS                                                       
         STH   R1,2(R3)                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EDIT OUT BRAND LIST                                   
         SPACE 3                                                                
EDLIST   NTR1                                                                   
         MVC   UNITLIST,0(R3)                                                   
         MVI   OPTION,C'N'         TRY TO FIT NAMES                             
         BAS   RE,MR31                                                          
         CLI   OPTION,C'Y'                                                      
         BE    XIT                 WONT FIT                                     
         MVI   OPTION,C'C'         NOW TRY CODES                                
         BAS   RE,MR31                                                          
         CLI   OPTION,C'Y'                                                      
         BE    XIT                 STILL WONT FIT                               
         ZIC   R0,UNITLIST+1       SO ADD UP UNITS                              
         ZIC   R1,UNITLIST+5       AND SHOW AS NNN=VARIOUS                      
         AR    R1,R0                                                            
         ZIC   R0,UNITLIST+9                                                    
         AR    R1,R0                                                            
         ZIC   R0,UNITLIST+13                                                   
         AR    R1,R0                                                            
         MVC   26(18,R2),SPACES                                                 
         EDIT  (R1),(3,26(R2)),ALIGN=LEFT                                       
         AR    R2,R0                                                            
         CLI   SPLDEMH+5,0         OPTIONALLY                                   
         BE    MR30                                                             
         MVI   26(R2),C'/'                                                      
         LH    R1,UNITLIST+2       ADD UP POINTS                                
         AH    R1,UNITLIST+6                                                    
         AH    R1,UNITLIST+10                                                   
         AH    R1,UNITLIST+14                                                   
         LA    R2,1(R2)                                                         
         EDIT  (R1),(5,26(R2)),1,ALIGN=LEFT                                     
         AR    R2,R0                                                            
         SPACE 1                                                                
MR30     MVC   26(8,R2),=C'=VARIOUS'                                            
         B     XIT                                                              
         SPACE 1                                                                
MR31     NTR1                                                                   
         MVC   BLOCK(132),SPACES                                                
         LA    R3,UNITLIST                                                      
         LA    R4,BLOCK                                                         
         LA    R5,4                                                             
         SPACE 1                                                                
MR32     CLI   0(R3),0                                                          
         BE    MR36                                                             
         CLI   1(R3),1             SHOW UNITS UNLESS 1                          
***      BE    MR34                                                             
         EDIT  (1,1(R3)),(3,0(R4)),ALIGN=LEFT                                   
         AR    R4,R0                                                            
         CLI   SPLDEMH+5,0         OPTION TO SHOW POINTS AS WELL                
         BE    MR33                                                             
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         EDIT  (2,2(R3)),(5,0(R4)),1,ALIGN=LEFT                                 
         AR    R4,R0                                                            
         SPACE 1                                                                
MR33     MVI   0(R4),C'='                                                       
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
MR34     MVC   0(3,R4),=C'OTH'                                                  
         CLI   0(R3),X'FF'                                                      
         BE    MR36                                                             
         MVC   0(3,R4),=C'P/B'                                                  
         CLI   0(R3),X'FD'                                                      
         BE    MR36                                                             
         L     RE,=A(PRDPOOL)      POSITION INTO POOL                           
         A     RE,RELO                                                          
         ZIC   R1,0(R3)                                                         
         BCTR  R1,R0                                                            
         MH    R1,=H'24'                                                        
         AR    RE,R1                                                            
         MVC   0(3,R4),1(RE)       SHOW CODE                                    
         CLI   OPTION,C'N'                                                      
         BNE   *+10                                                             
         MVC   0(20,R4),4(RE)      OR, OPTIONALLY, NAME                         
         OC    0(20,R4),SPACES                                                  
*                                                                               
         CLC   0(20,R4),SPACES     IF NAME IS SPACES                            
         BNE   MR35                                                             
         CLI   NOPRDOPT,C'N'       AND DON'T PRINT PRODS                        
         BNE   MR35                                                             
         BCTR  R4,0                PXZ                                          
         MVI   0(R4),X'40'         PXZ                                          
         LA    R4,1(R4)            PXZ                                          
*                                                                               
MR35     LA    R3,4(R3)                                                         
         LA    R4,25(R4)                                                        
         BCT   R5,MR32                                                          
         SPACE 1                                                                
MR36     GOTO1 SQUASHER,DMCB,BLOCK,132                                          
         CLI   DMCB+7,18           WILL IF FIT                                  
         BH    XIT                 NO                                           
         MVI   OPTION,C'Y'         YES                                          
         MVC   26(18,R2),BLOCK                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A CALENDAR                                      
         SPACE 3                                                                
PRINTCAL NTR1                                                                   
         L     R2,=A(SLOTS)                                                     
         A     R2,RELO                                                          
         ZIC   R3,NDAYS                                                         
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         SPACE 1                                                                
PC2      BAS   RE,PC4              DO EACH WEEK                                 
         L     R1,LSLOT                                                         
         MH    R1,=H'7'                                                         
         AR    R2,R1                                                            
         SH    R3,=H'7'                                                         
         BP    PC2                                                              
         B     PCX                                                              
         SPACE 1                                                                
PC4      NTR1                                                                   
         CH    R3,=H'7'            PRINT A WEEK                                 
         BL    *+8                                                              
         LA    R3,7                                                             
         LA    R4,9                9 LINES                                      
         CLI   NDAYS,36                                                         
         BL    PC6                                                              
         LA    R4,7                OR 7 LINES FOR 6-WEEK                        
         SPACE 1                                                                
PC6      LR    R5,R2 ********                                                   
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         L     R1,BOXAWIDE                                                      
         USING WIDED,R1                                                         
         LA    R9,XP1+2            USE R9 CAREFULLY. DONT REFERENCE             
         DROP  R1                                                               
         LR    R0,R3                NETBLOCK BEFORE XITING                      
         SPACE 1                                                                
PC8      MVC   0(22,R9),0(R5)      PICK OFF ONE LINE FROM EACH DAY              
         CLI   0(R9),C'*'                                                       
         BNE   *+8                                                              
         MVI   ASTX,C'Y'                                                        
         A     R5,LSLOT                                                         
         LA    R9,23(R9)                                                        
         BCT   R0,PC8                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         LA    R2,22(R2)                                                        
         BCT   R4,PC6                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE                                                                  
PCX      DS    0H                                                               
         CLI   ASTX,C'Y'                                                        
         BNE   XIT                                                              
         MVI   ASTX,0                                                           
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         L     R1,BOXAWIDE                                                      
         USING WIDED,R1                                                         
         LA    R9,XP1+2            USE R9 CAREFULLY. DONT REFERENCE             
         DROP  R1                                                               
         MVC   0(27,R9),=C'* = UNITS ON OTHER NETWORKS'                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         MVC   0(200,R2),MYBOX                                                  
         MVC   200(200,R2),MYBOX+200                                            
         MVI   BOXSTAT,0                                                        
         L     R3,BOXAWIDE                                                      
         USING WIDED,R3                                                         
         MVC   XHEAD4+2(20),SPLCLIN                                             
         MVI   XHEAD5,0                                                         
         MVC   XHEAD5+2(12),ESTHEAD                                             
         MVC   XHEAD1+72(20),MYHEAD                                             
         MVC   XHEAD2+72(20),MYUNDER                                            
         MVC   XHEAD4+72(20),PRDHEAD                                            
         CLI   SPLDEMH+5,0                                                      
         BE    XIT                                                              
         MVC   XHEAD5+128(8),=C'TARGET -'                                       
         MVC   XHEAD5+137(8),SPLDEM                                             
         DROP  R3                                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R2),DATADISP,ELCODE                                             
         EJECT                                                                  
*              LTORG                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              TABLES CONSTANTS ETC                                             
         SPACE 3                                                                
NETLIST  DS    CL24                                                             
RELOC    DC    A(*)                                                             
LSLOT    DC    F'198'              22X9                                         
MYBOX    DC    C'Y'                                                             
         DC    AL1(1)                                                           
         DC    6X'00'                                                           
         SPACE 1                                                                
MYCOLS   DC    C' L                      C                   '                  
         DC    C'   C                      C                 '                  
         DC    C'     C                      C               '                  
         DC    C'       C                      R             '                  
         DC    88C' '                                                           
         SPACE 1                                                                
MYROWS   DC    CL100' '                                                         
         DC    28X'00'                                                          
         SPACE 1                                                                
MYDAYS   DC    C'          MONDAY                TUESDAY     '                  
         DC    C'          WEDNESDAY               THURSDAY  '                  
         DC    C'              FRIDAY                SATURDAY'                  
         DC    C'                SUNDAY                      '                  
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,49,A),FORMAT=BI,WORK=1 '                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=68'                                    
         EJECT                                                                  
*              DSECT FOR CALENDAR                                               
         SPACE 3                                                                
CALD     DSECT                                                                  
         SPACE 1                                                                
*              NETDEMOD HERE                                                    
*              DEDBLOCK                                                         
         PRINT OFF                                                              
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
*                                  LOCAL STORAGE                                
TIMEOPT  DS    CL1        *        *(FROM EDIT MODULE)                          
NOPRDOPT DS    CL1        *        *(FROM EDIT MODULE)                          
*                                                                               
PERTYPE  DS    CL3                 PERIOD TYPE, CONTROLS                        
MAXMONTS EQU   16                                                               
NUMMONS  DS    F                                                                
MONLIST  DS    CL(4*MAXMONTS)                                                   
         SPACE 3                                                                
         DS    0D                                                               
SRTREC   DS    0CL68               SORT RECORD HERE                             
SRTKEY   DS    0CL49               KEY START                                    
SRTKPRD  DS    CL3                                                              
SRTKEST  DS    CL1                                                              
SRTKNET  DS    CL4                                                              
SRTMONTH DS    CL4                 START/ END MONTH                             
SRTDP    DS    CL8                 DAYPART                                      
SRTDATE  DS    CL2                 DAY NUMBER (0-34)                            
SRTNET   DS    CL4                 NETWORK                                      
SRTPROG  DS    CL16                PROGRAM NAME                                 
SRTBRAND DS    CL2                 BRAND CODE                                   
SRTIME   DS    CL4                 START-END TIME                               
SRTSPTLN DS    CL1                 SPOT LENGTH                                  
         DS    CL1                 SPARE                                        
SRTGRPS  DS    CL2                                                              
SRTUNITS DS    CL1                 NUMBER OF UNITS                              
         DS    CL13                SPARE                                        
         SPACE 2                                                                
RELO     DS    A                                                                
COUNT    DS    CL1                                                              
PRECOUNT DS    CL1                                                              
UNITLIST DS    CL16                                                             
ALAST    DS    A                                                                
MYHEAD   DS    CL20                                                             
MYUNDER  DS    CL20                                                             
MYBROAD  DS    CL6                                                              
THISTART DS    CL6                                                              
THISEND  DS    CL6                                                              
NDAYS    DS    CL1                                                              
DAYLIST  DS    42CL7                                                            
LSORT    DS    CL64                                                             
MYSLOT   DS    CL176                                                            
ABCLIST  DS    CL160                                                            
NBCLIST  DS    CL160                                                            
CBSLIST  DS    CL160                                                            
FOXLIST  DS    CL160                                                            
XXXLIST  DS    CL160                                                            
OTHLIST  DS    CL160                                                            
NETCOUNT DS    CL6                                                              
SLOTMAX  DS    CL1                                                              
PRDHEAD  DS    CL20                                                             
ESTHEAD  DS    CL12                                                             
PROGUNIT DS    CL1                                                              
SINGROUP DS    CL3                                                              
BREAK    DS    CL12                                                             
BRLEN    DS    CL1                                                              
ASTERIX  DS    CL1                                                              
ASTX     DS    CL1                                                              
OTHUNITS DS    F                                                                
         SPACE 1                                                                
*                                                                               
*              SPGENPRD                                                         
*              SPGENPRG                                                         
*              DDBIGBOX                                                         
*              DDWIDED                                                          
*              NETINCLS                                                         
         PRINT OFF                                                              
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF5D                                                       
         EJECT                                                                  
*              OTHER STORAGE FOR CALENDAR                                       
         SPACE 3                                                                
T31E25   CSECT                                                                  
         ENTRY SORTPOOL                                                         
         ENTRY SLOTS                                                            
         ENTRY PRDPOOL                                                          
         ENTRY GROUPLST                                                         
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'*GROUPS*'                                                    
GROUPLST DC    1000X'00'                                                        
         DS    0D                                                               
         DC    CL8'*SLOTS**'                                                    
SLOTS    DC    42CL198' '                                                       
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'*PRODS**'                                                    
PRDPOOL  DC    255CL24' '                                                       
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'**SORT**'                                                    
SORTPOOL DC    41000X'00'                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069NEMED25   05/01/02'                                      
         END                                                                    
