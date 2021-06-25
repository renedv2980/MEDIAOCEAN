*          DATA SET RERMP1D    AT LEVEL 006 AS OF 05/01/02                      
*          DATA SET RERMP1D    AT LEVEL 061 AS OF 03/19/96                      
*PHASE T8101DA,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'T8101D - RERMP1D - DEMO TAPE CREATE'                            
*                                                                               
***********************************************************************         
*                                                                     *         
*- RERMP1D -- DEMO TAPE CREATE                                        *         
*                                                                     *         
*  MOD LOG:                                                           *         
*  --------                                                           *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         SPACE 2                                                                
T8101D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8101D**,RR=RE                                                 
*                                                                               
         L     RC,0(R1)            ESTABLISH GENCON WORKAREA                    
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA             ESTABLISH SCREEN                             
         USING CONHEADH-64,RA                                                   
*                                                                               
         L     R8,ASPOOLD          ESTABLISH SPOOL WORKAREA                     
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD            ESTABLISH SYSTEM WORKAREA                    
         USING SYSD,R9                                                          
*                                                                               
         L     R7,ACOMFACS         ESTABLLISH COMMON SUBROUTINES                
         USING COMFACSD,R7                                                      
*                                                                               
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMODE                                                       
         B     EXXMOD                                                           
         SPACE 1                                                                
RP2      CLI   MODE,VALKEY                                                      
         BNE   EXXMOD                                                           
         BAS   RE,VALREQ                                                        
         B     EXXMOD                                                           
*                                                                               
EXXMOD   XIT1                                                                   
         EJECT                                                                  
*                                                                               
*-- EDIT THE REQUEST SCREEN                                                     
*                                                                               
VALREQ   NTR1                                                                   
         SPACE 1                                                                
         LA    R2,TITSTAH          STATION FIELD                                
         XC    WORK,WORK                                                        
         GOTO1 VALISTA             VALIDATE STATION                             
         CLI   WORK+40,X'40'                                                    
         BNH   *+10                                                             
         MVC   CSTAT+4(1),WORK+40                                               
*                                                                               
*        VALIDATE START DATE                                                    
*                                                                               
         LA    R2,TITSDTH          POINT TO START DATE                          
         XC    STRTOPT,STRTOPT     INIT START DATE OPTIONS                      
         XC    STRTOPTC,STRTOPTC   INIT START DATE OPTIONS                      
*                                                                               
         CLI   5(R2),0             OKAY IF NO START DATE ENTERED                
         BE    VREND                                                            
*                                                                               
         CLC   8(3,R2),=C'ALL'     OKAY IF 'ALL' ENTERED                        
         BE    VREND                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK   VALIDATE ENTERED DATE               
*                                                                               
         CLI   DMCB+3,0                                                         
         BNE   VRSTRT1                                                          
         B     DATERR                                                           
*                                                                               
VRSTRT1  GOTO1 DATCON,DMCB,(0,WORK),(3,STRTOPT)  SAVE START DATE                
         GOTO1 DATCON,DMCB,(0,WORK),(2,STRTOPTC) SAVE START DATE                
*                                                                               
*        VALIDATE END DATE                                                      
*                                                                               
VREND    LA    R2,TITEDTH          POINT TO END DATE                            
         MVC   ENDOPT,=X'FFFFFF'   INIT INTERNAL END DATE                       
*                                                                               
         CLI   5(R2),0             OKAY IF NO END DATE ENTERED                  
         BE    VRENDX                                                           
*                                                                               
         CLC   8(3,R2),=C'ALL'     IKAY IF 'ALL' ENTERED                        
         BE    VRENDX                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK  VALIDATE ENTERED DATE                
         CLI   DMCB+3,0                                                         
         BNE   VREND1                                                           
         B     DATERR                                                           
*                                                                               
VREND1   GOTO1 DATCON,DMCB,(0,WORK),(3,ENDOPT) SAVE END DATE                    
*                                                                               
VRENDX   CLC   STRTOPT,ENDOPT      CHECK START LESS THEN END                    
         BH    DATERR                                                           
*                                                                               
         B     EXXMOD                                                           
*                                                                               
DATERR   MVC   RERROR,=AL2(INVDATE) ERROR - INVALID DATE                        
         GOTO1 MYERROR                                                          
         EJECT                                                                  
*                                                                               
*-- EDIT THE REQUEST SCREEN                                                     
*                                                                               
REPMODE  NTR1                                                                   
         OPEN  (FILOUTA,(OUTPUT))                                               
*                                                                               
         L     R5,AIO1                                                          
         USING REINVREC,R5                                                      
*                                                                               
         BAS   RE,BLDHEAD          HEADER RECORD                                
         BAS   RE,BLDDPT           DAYPART RECORD                               
*                                                                               
RPM40    BAS   RE,GTINV            READ INVENTORY RECORD                        
         CLI   KEY,X'FF'                                                        
         BE    RPMEX                                                            
*                                                                               
         CLI   KEY+24,0                                                         
         BNE   RPM100                                                           
         BAS   RE,BLDINVH          INV HEADER RECORD                            
         B     RPM40                                                            
*                                                                               
RPM100   CLI   KEY+24,X'FF'                                                     
         BNE   RPM200                                                           
         BAS   RE,BLDCOMM          COMMENT RECORD                               
         B     RPM40                                                            
*                                                                               
RPM200   BAS   RE,BLDSRVY          SURVEY RECORD                                
         BAS   RE,BLDOVR           OVERRIDE RECORD                              
         B     RPM40                                                            
*                                                                               
RPMEX    BAS   RE,BLDTRL           TRAILER                                      
         CLOSE FILOUTA                                                          
         B     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*--READ AND FILTER INVENTORY RECORDS                                            
*                                                                               
GTINV    NTR1                                                                   
         LA    R4,KEY                                                           
         USING REINVREC,R4                                                      
*                                                                               
         CLI   KEYSAVE,X'12'       FIRST TIME                                   
         BE    GTINV200            NO GET NEXT RECORD                           
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,CSTAT                                                   
         MVC   RINVKINV,=CL4'0000'                                              
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE     CHECK UP TO STATION                          
         BE    *+12                                                             
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
         XC    KEY+21(6),KEY+21                                                 
         MVC   KEY+21(3),STRTOPT   MOVE DATE IN                                 
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE     CHECK UP TO STATION                          
         BE    GTINV300                                                         
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
* GET NEXT RECORD IF A HEADER CHECK IF IT FITS THE REQUEST                      
*                                                                               
GTINV200 GOTO1 SEQ                                                              
         CLC   KEY(17),KEYSAVE     CHECK UP TO STATION                          
         BE    *+12                                                             
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
         CLI   RINVKSRC,0          IS RECORD A HEADER                           
         BE    GTINV300                                                         
*                                                                               
         GOTO1 GETREC                                                           
         B     EXXMOD                                                           
*                                                                               
GTINV300 CLC   RINVKSTD,ENDOPT     IS INV START > REQ END                       
         BH    GTINV400            SKIP TO NEXT INV NUMBER                      
*                                                                               
         L     R4,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         CLC   RINVKSTD,STRTOPT    IS INV START >= REQ START                    
         BNL   EXXMOD              RECORD CAN BE PROCESSED                      
*                                                                               
         CLC   RINVPEFF+2,STRTOPTC IS INV END >= REQ START                      
         BNL   EXXMOD              RECORD CAN BE PROCESSED                      
*                                                                               
*  CURRENT HEADERS DATE OUT OF RANGE SKIP READ TO NEXT DATE                     
*                                                                               
         LA    R4,KEY                                                           
         ICM   RE,7,RINVKSTD                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,7,RINVKSTD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE     CHECK UP TO STATION                          
         BE    *+12                                                             
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
         CLI   RINVKSRC,0                                                       
         BE    GTINV300                                                         
         DC    H'0'                                                             
*                                                                               
*  CURRENT HEADER DOES NOT QUALIFY SKIP READ TO INV NUMBER                      
*                                                                               
GTINV400 MVC   FULL,RINVKINV                                                    
         ICM   R5,15,RINVKINV                                                   
         A     R5,=F'1'                                                         
         STCM  R5,15,RINVKINV                                                   
*                                                                               
         XC    KEY+21(6),KEY+21                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE     CHECK UP TO STATION                          
         BE    *+12                                                             
GTINV420 MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
         XC    KEY+21(6),KEY+21                                                 
         MVC   KEY+21(3),STRTOPT   MOVE DATE IN                                 
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE     CHECK UP TO STATION                          
         BE    GTINV300                                                         
         MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
*--BUILD THE HEADER RECORD                                                      
*                                                                               
BLDHEAD  NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING HDREC,R3                                                         
         XC    0(250,R3),0(R3)                                                  
*                                                                               
         LA    RE,28                                                            
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   HDID,=CL2'H1'                                                    
         MVC   HDSRC,=CL4'DDS '                                                 
         MVC   HDVER,=CL2'01'                                                   
         MVC   HDREP,AGENCY                                                     
         GOTO1 DATCON,DMCB,(5,DUB),(0,HDSVDAT)                                  
         TIME  DEC                                                              
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,HDSVTIM,4,=C'TOG'                               
         BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*--BUILD THE DAYPART RECORDS                                                    
*                                                                               
BLDDPT   NTR1                                                                   
         LA    R5,KEY                                                           
         USING RRDPRECD,R5                                                      
*                                                                               
*  READ THE DAYPART RECORDS                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVI   RRDPKTYP,X'3C'                                                   
         MVC   RRDPKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         B     BDP100                                                           
*                                                                               
BDP80    GOTO1 SEQ                                                              
*                                                                               
BDP100   CLC   KEY(26),KEYSAVE                                                  
         BNE   BDPEX                                                            
         MVC   AIO,AIO1                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING DPTREC,R3                                                        
         XC    0(250,R3),0(R3)                                                  
*                                                                               
         LA    RE,27                                                            
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   DPID,=CL2'D1'                                                    
         MVC   DPCODE,RRDPKDPT                                                  
         MVC   DPSNAME,RRDPSNAM                                                 
         MVC   DPLNAME,RRDPLNAM                                                 
         OI    DPSNAME+3,X'40'                                                  
         OI    DPLNAME+15,X'40'                                                 
         BAS   RE,WRITREC                                                       
         B     BDP80                                                            
*                                                                               
BDPEX    B     EXXMOD                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
*                                                                               
*--BUILD THE INVENTORY RECORD                                                   
*                                                                               
BLDINVH  NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING IMREC,R3                                                         
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    RE,431                                                           
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   IMID,=CL2'I1'                                                    
         MVC   IMSTA,RINVKSTA                                                   
         MVC   IMINVN,RINVKINV                                                  
         MVC   IMDYPTS,RINVDP                                                   
         MVC   IMFLTER,RINVPFLT                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(0,IMEFFS)                              
         OC    IMEFFE,SPACES                                                    
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    BDI100                                                           
         LA    R4,RINVPEFF+2                                                    
         GOTO1 DATCON,DMCB,(2,(R4)),(0,IMEFFE)                                  
*                                                                               
*  DAY/TIME LOGIC                                                               
*                                                                               
BDI100   XC    WORK,WORK                                                        
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'04',AIO),0                          
         CLI   12(R1),0                                                         
         BE    BDI300              DO AVAIL MOVE                                
*                                                                               
*  FILL DAY/TIME FIELDS FROM REGULART DAY/TIME ELEMENTS                         
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,12(R1)                                                        
         LA    R6,IMDAY1                                                        
BDI200   GOTO1 UNDAY,DMCB,2(R4),WORK              DAY                           
         MVC   0(11,6),WORK                                                     
         GOTO1 UNTIME,DMCB,3(R4),(0,11(R6))       TIME                          
         OC    0(22,R6),SPACES     BLANK FILL THE FIELDS                        
         LA    R6,22(R6)                                                        
*        SEE IF MORE AVAIL ADY TIME ELEMS EXIST                                 
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),2                                                          
         BNE   BDI400                                                           
         B     BDI200                                                           
*                                                                               
*  FILL DAY/TIME FIELDS FROM AVAIL INFO                                         
*                                                                               
BDI300   L     R4,12(R1)                                                        
         LA    R6,IMDAY1                                                        
BDI320   MVC   IMDAY1(22),2(R4)                                                 
         LA    R6,22(R6)                                                        
*        SEE IF MORE AVAIL ADY TIME ELEMS EXIST                                 
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),4                                                          
         BNE   BDI400                                                           
         B     BDI320                                                           
*                                                                               
*  PROGRAM NAME LOGIC                                                           
*                                                                               
BDI400   GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'03',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,12(R1)                                                        
         LA    R6,IMPROG1                                                       
BDI440   ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     BDI460                                                           
         MVC   0(0,R6),2(R4)                                                    
*                                                                               
BDI460   OC    0(27,R6),SPACES                                                  
         LA    R6,27(R6)                                                        
*        SEE IF MORE PROG NAME ELEMENTS EXIST                                   
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),3                                                          
         BNE   BDIEX                                                            
         B     BDI440                                                           
*                                                                               
BDIEX    BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
*                                                                               
*--BUILD THE SURVEY RECORD                                                      
*                                                                               
BLDSRVY  NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING SRVREC,R3                                                        
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    RE,641                                                           
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   SRVID,=C'S1'                                                     
         MVC   SRVSTA,RINVKSTA                                                  
         MVC   SRVINV,RINVKINV                                                  
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(0,SRVSEFF)                             
         MVC   SRVBOOK,RINVKBK                                                  
*                                                                               
         LA    RE,TYPTAB                                                        
BLDSR40  CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RINVKSRC,0(RE)                                                   
         BE    BLDSR50                                                          
         LA    RE,3(RE)                                                         
         B     BLDSR40                                                          
BLDSR50  MVC   SRVSRC,1(RE)        SOURCE                                       
         MVC   SRVLOOK,2(RE)       LOOKUP TYPE                                  
*                                                                               
*  FILL DAY/TIME FIELDS FROM REGULART DAY/TIME ELEMENTS                         
*                                                                               
BLDSR100 GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'CE',AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,12(R1)                                                        
         LA    R6,SRVDAY1                                                       
BLDSR140 GOTO1 UNDAY,DMCB,2(R4),WORK              DAY                           
         MVC   0(11,6),WORK                                                     
         GOTO1 UNTIME,DMCB,3(R4),(0,11(R6))       TIME                          
         OC    0(22,R6),SPACES     BLANK FILL THE FIELDS                        
         LA    R6,22(R6)                                                        
*        SEE IF MORE AVAIL ADY TIME ELEMS EXIST                                 
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),X'CE'                                                      
         BE    BLDSR140                                                         
*                                                                               
* MOVE THE DEMOS OUT                                                            
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         ST    R7,DBCOMFCS                                                      
         MVC   DBAREC,AIO                                                       
         L     R1,DBAREC                                                        
         LA    R1,34(R1)           POINT TO FIRST ELEMENT POSITION              
         ST    R1,DBAQUART                                                      
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
*                                                                               
*  MOVE OUT THE STATION LEVEL DEMOS                                             
*                                                                               
         SPACE 1                                                                
         GOTO1 CDEMOUT,DMCB,(C'L',STADEMS),DBLOCK,AIO2                          
         SPACE 1                                                                
*                                                                               
*  DO THE RATINGS                                                               
*                                                                               
         L     RE,AIO2                                                          
         LA    RF,33                                                            
         LA    R1,STPRAT                                                        
*                                                                               
BLDSR300 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR300                                                      
*                                                                               
*  DO THE IMPS                                                                  
*                                                                               
         LA    RF,34                                                            
         LA    R1,STPIMP                                                        
*                                                                               
BLDSR350 MVC   0(4,R1),0(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   RF,BLDSR350                                                      
*                                                                               
*  DO THE VARIOUS DEMOS                                                         
*                                                                               
         LA    RF,9                                                             
         LA    R1,STHLDRTG                                                      
*                                                                               
BLDSR400 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR400                                                      
*                                                                               
*  MOVE OUT THE MARKET LEVEL DEMOS                                              
*                                                                               
         SPACE 1                                                                
         GOTO1 CDEMOUT,DMCB,(C'L',MKTDEMS),DBLOCK,AIO2                          
         SPACE 1                                                                
*                                                                               
*  DO THE RATINGS                                                               
*                                                                               
         L     RE,AIO2                                                          
         LA    RF,33                                                            
         LA    R1,MKPRAT                                                        
*                                                                               
BLDSR500 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR500                                                      
*                                                                               
*  DO THE IMPS                                                                  
*                                                                               
         LA    RF,34                                                            
         LA    R1,MKPIMP                                                        
*                                                                               
BLDSR550 MVC   0(4,R1),0(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   RF,BLDSR550                                                      
*                                                                               
*  DO THE VARIOUS DEMOS                                                         
*                                                                               
         LA    RF,9                                                             
         LA    R1,MKHLDRTG                                                      
*                                                                               
BLDSR600 MVC   0(2,R1),2(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,BLDSR600                                                      
*                                                                               
* SET MARKET SHARE VALUES TO 100                                                
*                                                                               
         MVC   MKHLDSHR,=F'1000'                                                
         MVC   MKMTASHR,=F'1000'                                                
         MVC   MKMTBSHR,=F'1000'                                                
*                                                                               
         BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         DROP  R3,R5                                                            
TYPTAB   DC    CL3'AAA'                                                         
         DC    CL3'BAP'                                                         
         DC    CL3'CAT'                                                         
         DC    CL3'DAS'                                                         
         DC    CL3'EAE'                                                         
         DC    CL3'NNA'                                                         
         DC    CL3'ONP'                                                         
         DC    CL3'PNM'                                                         
         DC    CL3'QNS'                                                         
         DC    CL3'RNE'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*--BUILD THE OVERRIDE RECORD                                                    
*                                                                               
BLDOVR   NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING OVRREC,R3                                                        
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         LA    RE,33                                                            
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   OVRID,=C'O1'                                                     
         MVC   OVRSTA,RINVKSTA                                                  
         MVC   OVRINV,RINVKINV                                                  
         PRINT GEN                                                              
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(0,OVRSEFF)                             
         PRINT NOGEN                                                            
*                                                                               
*  SET THE DBLOCK FOR DEMOCON                                                   
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         ST    R7,DBCOMFCS                                                      
         MVI   DBSELMED,C'U'                                                    
*                                                                               
*  GET OVERRIDE ELEMENTS                                                        
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'DE',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDOVEX                                                          
*                                                                               
         L     R4,12(R1)                                                        
*                                                                               
BLDOV440 GOTO1 DEMOCON,DMCB,(0,3(R4)),(6,OVRCAT),DBLOCK                         
         MVI   OVRVLTYP,C'I'                                                    
         CLI   4(R4),C'R'                                                       
         BNE   *+8                                                              
         MVI   OVRVLTYP,C'R'                                                    
         MVC   OVRDEMO,8(R4)                                                    
*                                                                               
         BAS   RE,WRITREC                                                       
*        SEE IF MORE OVERRIDE ELEMENTS EXIST                                    
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),X'DE'                                                      
         BE    BLDOV440                                                         
*                                                                               
BLDOVEX  B     EXXMOD                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
*                                                                               
*--BUILD THE COMMENT RECORD                                                     
*                                                                               
BLDCOMM  NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING CMREC,R3                                                         
         L     R5,AIO                                                           
         USING REINVREC,R5                                                      
*                                                                               
         MVC   CMID,=CL2'C1'                                                    
         MVC   CMSTA,RINVKSTA                                                   
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(0,CMEFFS)                              
         EDIT  (2,RINVKTXT),(3,CMTXTN),FILL=0                                   
*  GET FILTER INFORMATION                                                       
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   BLDCM50                                                          
*                                                                               
         L     R6,12(R1)                                                        
         USING RINVFEL,R6                                                       
*                                                                               
         MVC   CMBOOK,RINVFBK                                                   
*  GET BOOK TYPE                                                                
         LA    R1,SVCLST                                                        
BLDCM20  CLC   RINVFBKT,3(R1)                                                   
         BE    BLDCM30                                                          
         LA    R1,L'SVCLST(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   BLDCM20                                                          
         DC    H'0'                                                             
BLDCM30  MVC   CMBKTYP,1(R1)                                                    
         CLI   1(R1),C' '                                                       
         BNE   BLDCM50                                                          
         MVI   CMBKTYP,C'A'                                                     
*                                                                               
*  MOVE TEXT OUT                                                                
*                                                                               
BLDCM50  LA    R4,CMTXT1                                                        
         L     R6,AIO                                                           
         LA    R6,34(R6)           POINT R6 TO FIRST ELEMENT                    
         USING RINVTEL,R6                                                       
*                                                                               
         LA    RF,27               RECORD LENGTH                                
*                                                                               
BLDCM100 CLI   0(R6),1                                                          
         BNE   BLDCMEX                                                          
         ZIC   R1,RINVTLEN                                                      
         SH    R1,=H'7'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),RINVTEXT                                                 
         OC    0(60,R4),SPACES     SPACE FILL                                   
         LA    R4,60(R4)                                                        
         LA    RF,60(RF)                                                        
*                                                                               
*  GET NEXT ELEMENT                                                             
*                                                                               
         ZIC   RE,RINVTLEN                                                      
         AR    R6,RE                                                            
         B     BLDCM100                                                         
*                                                                               
BLDCMEX  STCM  RF,3,RECLEN                                                      
         BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         DROP  R3,R5,R6                                                         
*                                                                               
*--BUILD THE TRAILER RECORD                                                     
*                                                                               
BLDTRL   NTR1                                                                   
         L     R3,AIO3                                                          
         LA    R3,4(R3)                                                         
         USING TRREC,R3                                                         
*                                                                               
         LA    RE,17                                                            
         STCM  RE,3,RECLEN                                                      
*                                                                               
         MVC   TRID,=CL2'T1'                                                    
         MVC   TRSTA,CSTAT                                                      
         EDIT  (4,TAPECNT),(6,TRRECCNT),FILL=0                                  
*                                                                               
BLDTREX  BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
WRITREC  NTR1                                                                   
         L     RE,TAPECNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,TAPECNT                                                       
*                                                                               
         L     R4,AIO3                                                          
         XC    0(4,R4),0(R4)                                                    
         MVC   0(2,R4),RECLEN                                                   
         PUT   FILOUTA,(R4)                                                     
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'REC',AIO3,C'DUMP',550,=C'1D'                  
         B     EXXMOD                                                           
*                                                                               
QTRTAB   DC    XL1'03',C'01'                                                    
         DC    XL1'06',C'02'                                                    
         DC    XL1'09',C'03'                                                    
         DC    XL1'0C',C'04'                                                    
         BAS   RE,WRITREC                                                       
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
RELO     DS    A                                                                
REPFILE  DC    CL8'REPFILE'                                                     
*                                                                               
STADEMS  DC    X'00D97A'           CH2-11                                       
         DC    X'00D97B'           CH6-11                                       
         DC    X'00D941'           WW18+                                        
         DC    X'00D97F'           A2+ (V2+)                                    
         DC    X'00D919'           W12-17                                       
         DC    X'00D91C'           W12-24                                       
         DC    X'00D92D'           W18+                                         
         DC    X'00D929'           W18-34                                       
         DC    X'00D92A'           W18-49                                       
         DC    X'00D947'           W21-49                                       
         DC    X'00D92F'           W25-49                                       
         DC    X'00D930'           W25-54                                       
         DC    X'00D931'           W25-64                                       
         DC    X'00D935'           W35-64                                       
         DC    X'00D95F'           M18+                                         
         DC    X'00D95B'           M18-34                                       
         DC    X'00D95C'           M18-49                                       
         DC    X'00D973'           M21-49                                       
         DC    X'00D961'           M25-49                                       
         DC    X'00D962'           M25-54                                       
         DC    X'00D963'           M25-64                                       
         DC    X'00D967'           M35-64                                       
         DC    X'00D97D'           TN12-17                                      
         DC    X'00D980'           A12-24 (V12-24)                              
         DC    X'00D981'           A12-34 (V12-34)                              
         DC    X'00D991'           A18+                                         
         DC    X'00D98D'           A1834                                        
         DC    X'00D98E'           A1849                                        
         DC    X'00D9BF'           A2149                                        
         DC    X'00D993'           A25-49                                       
         DC    X'00D994'           A25-54                                       
         DC    X'00D995'           A25-64                                       
         DC    X'00D999'           A35-64                                       
*                                                                               
         DC    X'00E37A'           CH2-11                                       
         DC    X'00E37B'           CH6-11                                       
         DC    X'00E341'           WW18+                                        
         DC    X'00E37F'           A2+ (V2+)                                    
         DC    X'00E319'           W12-17                                       
         DC    X'00E31C'           W12-24                                       
         DC    X'00E32D'           W18+                                         
         DC    X'00E329'           W18-34                                       
         DC    X'00E32A'           W18-49                                       
         DC    X'00E347'           W21-49                                       
         DC    X'00E32F'           W25-49                                       
         DC    X'00E330'           W25-54                                       
         DC    X'00E331'           W25-64                                       
         DC    X'00E335'           W35-64                                       
         DC    X'00E35F'           M18+                                         
         DC    X'00E35B'           M18-34                                       
         DC    X'00E35C'           M18-49                                       
         DC    X'00E373'           M21-49                                       
         DC    X'00E361'           M25-49                                       
         DC    X'00E362'           M25-54                                       
         DC    X'00E363'           M25-64                                       
         DC    X'00E367'           M35-64                                       
         DC    X'00E37D'           TN12-17                                      
         DC    X'00E380'           A12-24 (V12-24)                              
         DC    X'00E381'           A12-34 (V12-34)                              
         DC    X'00E391'           A18+                                         
         DC    X'00E38D'           A1834                                        
         DC    X'00E38E'           A1849                                        
         DC    X'00E3BF'           A2149                                        
         DC    X'00E393'           A25-49                                       
         DC    X'00E394'           A25-54                                       
         DC    X'00E395'           A25-64                                       
         DC    X'00E399'           A35-64                                       
*                                                                               
         DC    X'00E301'           THOMES                                       
         DC    X'00D901'           RHOMES                                       
         DC    X'00E201'           SHOMES                                       
         DC    X'00D701'           PHOMES                                       
         DC    X'00D902'           RMETRO-A                                     
         DC    X'00E202'           SMETRO-A                                     
         DC    X'00D702'           PMETRO-A                                     
         DC    X'00D903'           RMETRO-B                                     
         DC    X'00E203'           SMETRO-B                                     
         DC    X'00D703'           PMETRO-B                                     
         DC    X'FF'                                                            
*                                                                               
MKTDEMS  DC    X'00D77A'           CH2-11                                       
         DC    X'00D77B'           CH6-11                                       
         DC    X'00D741'           WW18+                                        
         DC    X'00D77F'           A2+ (V2+)                                    
         DC    X'00D719'           W12-17                                       
         DC    X'00D71C'           W12-24                                       
         DC    X'00D72D'           W18+                                         
         DC    X'00D729'           W18-34                                       
         DC    X'00D72A'           W18-49                                       
         DC    X'00D747'           W21-49                                       
         DC    X'00D72F'           W25-49                                       
         DC    X'00D730'           W25-54                                       
         DC    X'00D731'           W25-64                                       
         DC    X'00D735'           W35-64                                       
         DC    X'00D75F'           M18+                                         
         DC    X'00D75B'           M18-34                                       
         DC    X'00D75C'           M18-49                                       
         DC    X'00D773'           M21-49                                       
         DC    X'00D761'           M25-49                                       
         DC    X'00D762'           M25-54                                       
         DC    X'00D763'           M25-64                                       
         DC    X'00D767'           M35-64                                       
         DC    X'00D77D'           TN12-17                                      
         DC    X'00D780'           A12-24 (V12-24)                              
         DC    X'00D781'           A12-34 (V12-34)                              
         DC    X'00D791'           A18+                                         
         DC    X'00D78D'           A1834                                        
         DC    X'00D78E'           A1849                                        
         DC    X'00D7BF'           A2149                                        
         DC    X'00D793'           A25-49                                       
         DC    X'00D794'           A25-54                                       
         DC    X'00D795'           A25-64                                       
         DC    X'00D799'           A35-64                                       
*                                                                               
         DC    X'00D87A'           CH2-11                                       
         DC    X'00D87B'           CH6-11                                       
         DC    X'00D841'           WW18+                                        
         DC    X'00D87F'           A2+ (V2+)                                    
         DC    X'00D819'           W12-17                                       
         DC    X'00D81C'           W12-24                                       
         DC    X'00D82D'           W18+                                         
         DC    X'00D829'           W18-34                                       
         DC    X'00D82A'           W18-49                                       
         DC    X'00D847'           W21-49                                       
         DC    X'00D82F'           W25-49                                       
         DC    X'00D830'           W25-54                                       
         DC    X'00D831'           W25-64                                       
         DC    X'00D835'           W35-64                                       
         DC    X'00D85F'           M18+                                         
         DC    X'00D85B'           M18-34                                       
         DC    X'00D85C'           M18-49                                       
         DC    X'00D873'           M21-49                                       
         DC    X'00D861'           M25-49                                       
         DC    X'00D862'           M25-54                                       
         DC    X'00D863'           M25-64                                       
         DC    X'00D867'           M35-64                                       
         DC    X'00D87D'           TN12-17                                      
         DC    X'00D880'           A12-24 (V12-24)                              
         DC    X'00D881'           A12-34 (V12-34)                              
         DC    X'00D891'           A18+                                         
         DC    X'00D88D'           A1834                                        
         DC    X'00D88E'           A1849                                        
         DC    X'00D8BF'           A2149                                        
         DC    X'00D893'           A25-49                                       
         DC    X'00D894'           A25-54                                       
         DC    X'00D895'           A25-64                                       
         DC    X'00D899'           A35-64                                       
*                                                                               
         DC    X'00D801'           THOMES                                       
         DC    X'00D701'           RHOMES                                       
         DC    X'00D701'           SHOMES                                       
         DC    X'00D701'           PHOMES                                       
         DC    X'00D702'           RMETRO-A                                     
         DC    X'00D702'           SMETRO-A                                     
         DC    X'00D702'           PMETRO-A                                     
         DC    X'00D703'           RMETRO-B                                     
         DC    X'00D703'           SMETRO-B                                     
         DC    X'00D703'           PMETRO-B                                     
         DC    X'FF'                                                            
         EJECT                                                                  
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         SPACE 2                                                                
       ++INCLUDE RESVCTAB                                                       
*                                                                               
*                                                                               
*  LAYOUT FOR TAPE HEADER                                                       
THDREC   DSECT                                                                  
THDLEN   DS    CL2                 LENGTH = 1028                                
THDID    DS    CL4                 TAPE ID = TSSU                               
THDVER   DS    CL2                 VERSION NUMBER =1                            
THDREP   DS    CL2                 REP CODE                                     
THDBASE  DS    CL2                 BASE YEAR                                    
THDDATE  DS    CL3                 TAPE DATE YYMMDD                             
*                                                                               
THDTAPE  DS    CL1                 TAPE NUMBER 1=FIRST TAPE                     
THDTIME  DS    CL4                 TIME MILITARY                                
THDSVC   DS    CL1                 SERVICE                                      
THDCMNT  DS    CL1                 COMMENTS Y,N                                 
THDSTBK  DS    CL2                 START BOOK                                   
THDENDBK DS    CL2                 END BOOK                                     
THDNOSTN DS    CL2                 # STATIONS SAVED                             
THDSTNS  DS    200CL5              STATIONS                                     
*                                                                               
*  STATION FILE - MASTER INVENTORY RECORD                                       
RSSU     DSECT                                                                  
RSSULNTH DS    CL2                 LENGTH                                       
RSSUCNTL DS    CL4                 CONTROL                                      
*                                                                               
*                                                                               
RSSUKEY  DS    0CL36                                                            
RSSUKID  DS    CL2                 KEY ID (X'0050')                             
         DS    CL18                SPARE                                        
RSSUKREP DS    CL2                 REP                                          
RSSUKSTA DS    CL6                 STATION                                      
RSSUKSSU DS    CL4                 INV #                                        
         DS    CL1                 SPARE                                        
RSSUKDUP DS    CL1                 DATE CONTROL                                 
RSSUKREC DS    CL1                 RECORD TYPE (X'01')                          
         DS    CL1                 SPARE                                        
*                                                                               
*  SSU DESCRIPTION ELEMENT                                                      
RSSUDELM  DS   CL1                 X'1C' ELEM CODE/WORD LENGTH                  
RSSUDSPO  DS   CL1                 SPECIAL OPTIONS (=X'FF)                      
RSSUDTYP  DS   CL4                 TYPE (SPACES)                                
RSSUDFDT  DS   CL2                 FIRST USE DATE (OPTIONAL)                    
RSSUDLDT  DS   CL2                 LASST USE DATE (OPTIONAL)                    
          DS   CL2                 SPARE                                        
RSSUDDPT  DS   CL8                 DAYPARTS (FOR MINI-PLANNER)                  
          DS   CL4                 SPARE                                        
*                                                                               
*  DAY-TIME ELEMENTS                                                            
RSSUTELM  DS   CL1                 X'24' ELEM CODE/WORD LENGTH                  
          DS   CL1                 SPARE                                        
RSSUTDY1  DS   CL1                 DAYS (BIT 0=MON,7=OUT/WEEK)                  
RSSUTDY2  DS   CL1                 START DAY BITS 0-3,END DAY BITS 4-7          
RSSUTSTM  DS   CL2                 START TIME MILITARY                          
RSSUTETM  DS   CL2                 END TIME MILITARY                            
*                                                                               
*  PROGRAMMING ELEMENTS                                                         
RSSUPELM  DS   CL1                 X'30' ELEM CODE/WORD LENGTH                  
RSSUPPRO  DS   C                   PROGRAMMING (22 MAX)                         
*                                                                               
*  AVAIL/SALES PLANNER DAY/TIME OVERRIDE PRINT ELEMENTS                         
RSSUAELM  DS   CL1                 X'54' ELEM CODE/WORD LENGTH                  
          DS   CL1                 SPARE                                        
RSSUADY1  DS   CL1                 DAYS (BIT 0=MON,7=OUT/WEEK)                  
RSSUADY2  DS   CL1                 START DAY BITS 0-3,END DAY BITS 4-7          
RSSUASTM  DS   CL2                 START TIME MILITARY                          
RSSUAETM  DS   CL2                 END TIME MILITARY                            
          SPACE 3                                                               
*                                                                               
*  STATION FILE - DEMOGRAPHIC TRACK RECORD                                      
RSDM     DSECT                                                                  
RSDMLNTH DS    CL2                 LENGTH                                       
RSDMCNTL DS    CL4                 CONTROL                                      
*                                                                               
*                                                                               
RSDMKEY  DS    0CL36                                                            
RSDMKID  DS    CL2                 KEY ID (X'0050')                             
         DS    CL18                SPARE                                        
RSDMKREP DS    CL2                 REP                                          
RSDMKSTA DS    CL6                 STATION                                      
RSDMKSSU DS    CL4                 INV #                                        
         DS    CL1                 SPARE                                        
RSDMKDUP DS    CL1                 DATE CONTROL                                 
RSDMKREC DS    CL1                 N=NSI,A=ARB                                  
RSDMKBK  DS    CL1                 BOOK YM                                      
*****************************************                                       
RSDMTREC DSECT                                                                  
RSDMID   DS    CL2                 ID (D1)                                      
RSDMCODE DS    CL1                 DAYPART CODE                                 
RSDMSNAME DS   CL4                 DAYPART SHORT NAME                           
RSDMLNAME DS   CL16                DAYPART LONG NAME                            
RSDMTREC DSECT                                                                  
RSDMID   DS    CL2                 ID (D1)                                      
RSDMCODE DS    CL1                 DAYPART CODE                                 
RSDMSNAME DS   CL4                 DAYPART SHORT NAME                           
RSDMLNAME DS   CL16                DAYPART LONG NAME                            
RSDMTREC DSECT                                                                  
RSDMID   DS    CL2                 ID (D1)                                      
RSDMCODE DS    CL1                 DAYPART CODE                                 
RSDMSNAME DS   CL4                 DAYPART SHORT NAME                           
RSDMLNAME DS   CL16                DAYPART LONG NAME                            
*                                                                               
*  LAYOUT FOR INVENTORY MASTER                                                  
IMREC    DSECT                                                                  
IMID     DS    CL2                 ID (I1)                                      
IMSTA    DS    CL5                 STATION                                      
IMINVN   DS    CL4                 INVENTORY NUMBER                             
IMEFFS   DS    CL6                 EFFECTIVE START DATE                         
IMEFFE   DS    CL6                 EFFECTIVE END DATE                           
IMDAY1   DS    CL11                DAY 1                                        
IMSTIM1  DS    CL11                START TIME 1                                 
IMETIM1  DS    CL11                END TIME 1                                   
IMDAY2   DS    CL11                DAY 2                                        
IMSTIM2  DS    CL11                START TIME 2                                 
IMETIM2  DS    CL11                END TIME 2                                   
IMDAY3   DS    CL11                DAY 3                                        
IMSTIM3  DS    CL11                START TIME 3                                 
IMETIM3  DS    CL11                END TIME 3                                   
IMDAY4   DS    CL11                DAY 4                                        
IMSTIM4  DS    CL11                START TIME 4                                 
IMETIM4  DS    CL11                END TIME 4                                   
IMDAY5   DS    CL11                DAY 5                                        
IMSTIM5  DS    CL11                START TIME 5                                 
IMETIM5  DS    CL11                END TIME 5                                   
IMDAY6   DS    CL11                DAY 6                                        
IMSTIM6  DS    CL11                START TIME 6                                 
IMETIM6  DS    CL11                END TIME 6                                   
IMDAY7   DS    CL11                DAY 7                                        
IMSTIM7  DS    CL11                START TIME 7                                 
IMETIM7  DS    CL11                END TIME 7                                   
IMDAY8   DS    CL11                DAY 8                                        
IMSTIM8  DS    CL11                START TIME 8                                 
IMETIM8  DS    CL11                END TIME 8                                   
IMPROG1  DS    CL27                PROGRAM TITLE 1                              
IMPROG2  DS    CL27                PROGRAM TITLE 2                              
IMPROG3  DS    CL27                PROGRAM TITLE 3                              
IMPROG4  DS    CL27                PROGRAM TITLE 4                              
IMPROG5  DS    CL27                PROGRAM TITLE 5                              
IMPROG6  DS    CL27                PROGRAM TITLE 6                              
IMPROG7  DS    CL27                PROGRAM TITLE 7                              
IMPROG8  DS    CL27                PROGRAM TITLE 8                              
IMDYPTS  DS    CL6                 DAYPARTS                                     
IMFLTER  DS    CL6                 FILTERS                                      
*                                                                               
*  LAYOUT FOR SURVEY RECORD                                                     
SRVREC   DSECT                                                                  
SRVID    DS    CL2                 ID (S1)                                      
SRVSTA   DS    CL5                 STATION                                      
SRVINV   DS    CL4                 INVENTORY NUMBER                             
SRVSEFF  DS    CL6                 EFFECTIVE START DATE                         
SRVSRC   DS    CL1                 SOURCE                                       
SRVBOOK  DS    CL2                 BOOK                                         
SRVLOOK  DS    CL1                 LOOKUP TYPE (T,P,A,S,E)                      
SRVDAY1  DS    CL11                DAY 1                                        
SRVSTM1  DS    CL11                START TIME 1                                 
SRVETM1  DS    CL11                END TIME 1                                   
SRVDAY2  DS    CL11                DAY 2                                        
SRVSTM2  DS    CL11                START TIME 2                                 
SRVETM2  DS    CL11                END TIME 2                                   
SRVDAY3  DS    CL11                DAY 3                                        
SRVSTM3  DS    CL11                START TIME 3                                 
SRVETM3  DS    CL11                END TIME 3                                   
SRVDAY4  DS    CL11                DAY 4                                        
SRVSTM4  DS    CL11                START TIME 4                                 
SRVETM4  DS    CL11                END TIME 4                                   
SRVDAY5  DS    CL11                DAY 5                                        
SRVSTM5  DS    CL11                START TIME 5                                 
SRVETM5  DS    CL11                END TIME 5                                   
SRVDAY6  DS    CL11                DAY 6                                        
SRVSTM6  DS    CL11                START TIME 6                                 
SRVETM6  DS    CL11                END TIME 6                                   
SRVDAY7  DS    CL11                DAY 7                                        
SRVSTM7  DS    CL11                START TIME 7                                 
SRVETM7  DS    CL11                END TIME 7                                   
SRVDAY8  DS    CL11                DAY 8                                        
SRVSTM8  DS    CL11                START TIME 8                                 
SRVETM8  DS    CL11                END TIME 8                                   
*  STATION LEVEL DEMOS                                                          
STPRAT   DS    CL66                PEOPLE RATINGS 33X2                          
STPIMP   DS    CL132               PEOPLE IMPS 33X4                             
STTHMS   DS    CL4                 TOTAL HOMES 000'S                            
STHLDRTG DS    CL2                 HOUSEHOLD RATINGS                            
STHLDSHR DS    CL2                 HOUSEHOLD SHARES                             
STHLDHUT DS    CL2                 HOUSEHOLD HUTS                               
STMTARTG DS    CL2                 METRO-A RATINGS                              
STMTASHR DS    CL2                 METRO-A SHARE                                
STMTAHUT DS    CL2                 METRO-A HUTS                                 
STMTBRTG DS    CL2                 METRO-B RATINGS                              
STMTBSHR DS    CL2                 METRO-B SHARE                                
STMTBHUT DS    CL2                 METRO-B HUTS                                 
*  MARKET LEVEL DEMOS                                                           
MKPRAT   DS    CL66                PEOPLE RATINGS 33X2                          
MKPIMP   DS    CL132               PEOPLE IMPS 33X4                             
MKTHMS   DS    CL4                 TOTAL HOMES 000'S                            
MKHLDRTG DS    CL2                 HOUSEHOLD RATINGS                            
MKHLDSHR DS    CL2                 HOUSEHOLD SHARES                             
MKHLDHUT DS    CL2                 HOUSEHOLD HUTS                               
MKMTARTG DS    CL2                 METRO-A RATINGS                              
MKMTASHR DS    CL2                 METRO-A SHARE                                
MKMTAHUT DS    CL2                 METRO-A HUTS                                 
MKMTBRTG DS    CL2                 METRO-B RATINGS                              
MKMTBSHR DS    CL2                 METRO-B SHARE                                
MKMTBHUT DS    CL2                 METRO-B HUTS                                 
*                                                                               
*  LAYOUT FOR OVERRIDE RECORD                                                   
OVRREC   DSECT                                                                  
OVRID    DS    CL2                 ID (O1)                                      
OVRSTA   DS    CL5                 STATION                                      
OVRINV   DS    CL4                 INVENTORY NUMBER                             
OVRSEFF  DS    CL6                 EFFECTIVE START DATE                         
OVRCAT   DS    CL7                 CATEGORY NAME                                
OVRVLTYP DS    CL1                 VALUE TYPE R,I                               
OVRDEMO  DS    CL4                 DEMO AMOUNT                                  
*                                                                               
*  LAYOUT FOR COMMENT RECORD                                                    
CMREC    DSECT                                                                  
CMID     DS    CL2                 RECORD ID (C1)                               
CMSTA    DS    CL5                 STATION                                      
CMINVN   DS    CL4                 INVENTORY NUMBER                             
CMEFFS   DS    CL6                 EFFECTIVE START DATE                         
CMTXTN   DS    CL3                 TEXT NUMBER                                  
CMBOOK   DS    CL2                 BOOK                                         
CMBKTYP  DS    CL1                 BOOK TYPE                                    
CMTXT1   DS    CL60                COMMENT TEXT (UP TO 40 LINES)                
*                                                                               
*  LAYOUT FOR TRAILER RECORD                                                    
TRREC    DSECT                                                                  
TRID     DS    CL2                 RECORD ID (T1)                               
TRSTA    DS    CL5                 STATION                                      
TRRECCNT DS    CL6                 RECORD COUNT                                 
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPFFD                                                                      
* DDGENTWA                                                                      
* RERMPWTWA                                                                     
* RERMPD7D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* RERMPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPECD                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
       ++INCLUDE REGENRDP                                                       
         EJECT                                                                  
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*              WORK AREA                                                        
*                                                                               
*  ALL FIELDS DEFINED ABOVE THE DOUBLE LINE OF ASTERIKS                         
*  MUST ALSO BE DEFINED IN THE RERMP30 PHASE.                                   
*                                                                               
TAPECNT  DS    F                   NUMBER OF RECORDS                            
*                                                                               
STRTOPT  DS    CL3                 START DATE                                   
STRTOPTC DS    CL2                 START DATE COMPRESSED                        
ENDOPT   DS    CL3                 END DATE                                     
*                                                                               
DBEXTRA1 DS    CL128               DBLOCK EXTRA LENGTH                          
RECLEN   DS    H                   RECORD LENGTH                                
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T810FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T810FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006RERMP1D   05/01/02'                                      
         END                                                                    
