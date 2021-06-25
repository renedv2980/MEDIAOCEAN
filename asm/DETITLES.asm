*          DATA SET DETITLES   AT LEVEL 216 AS OF 04/10/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DETITLEA                                                                 
*---------------------------------------------------------------------*         
* OFFLINE JOB THAT TAKES AN INPUT CARD WITH THE BOOK IN MONTH/YEAR              
* FORMAT AND CREATES AN OUTPUT FILE OF PROGRAM TITLES FOR EVERY                 
* MARKET/DAY/QUATER HOUR/STATION.                                               
*                                                                               
* SYSPRINT HAS LIST OF ALL US TV MARKETS FOR THE BOOK.                          
*---------------------------------------------------------------------*         
*INCLUDE CARDS                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE LOADER                                                                 
*INCLUDE REGSAVE                                                                
         EJECT                                                                  
DETITLES CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         NBASE 0,DETITLES,=V(REGSAVE),R6                                        
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         GOTO1 =V(LOADER),DMCB,DEMTABOF,0   GET A(DEMTABOF)                     
         OC    DMCB+4,DMCB+4       LOADED OKAY?                                 
         BNZ   *+6                 YES                                          
         DC    H'0'                NO                                           
         MVC   VDEMTBOF,4(R1)         A(DEMTABOF)                               
*                                                                               
MAIN10   DS    0H                                                               
         OPEN  (OUT,(OUTPUT))                                                   
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,DMSYS,DMFLIST                            
*                                                                               
         GOTO1 =V(CARDS),DMCB,INIO,=C'RE00'        VALIDATE INPUT MONTH         
         GOTO1 =V(DATVAL),DMCB,(2,INIO),WORK                                    
         CLC   WORK(6),=6C'0'                                                   
         BNE   VALID                                                            
INVALID  MVC   P(24),=C'INVALID MONTH/YEAR INPUT'                               
         GOTO1 =V(PRINTER)                                                      
         B     EXIT1                                                            
VALID    GOTO1 =V(DATCON),DMCB,(0,WORK),(3,BOOK3)                               
         MVC   BOOK,BOOK3                                                       
         XC    BOOK,=2X'FF'                                                     
*                                                                               
         MVC   P(8),=C'MARKETS:'                                                
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'MLCODE+L'MLMEDIA+L'MLSRC),=C'MTN'                          
         MVC   KEY+MLBOOK-MLKEY(L'MLBOOK),BOOK                                  
         MVI   KEY+MLIND-MLKEY,MLINDEQU                                         
         MVC   SVKEY,KEY                                                        
*                                                                               
MAIN20   MVC   KEY,SVKEY           M-FR PROGRAMS FOR MARKET                     
         MVI   DAY,MTHRUF                                                       
         BAS   RE,GETQH                                                         
*                                                                               
         MVC   KEY,SVKEY           SATURDAY PROGRAMS FOR MARKET                 
         MVI   DAY,SATDAY                                                       
         BAS   RE,GETQH                                                         
*                                                                               
         MVC   KEY,SVKEY           SUNDAY PROGRAMS FOR MARKET                   
         MVI   DAY,SUNDAY                                                       
         BAS   RE,GETQH                                                         
*                                                                               
         CLI   NOMORE,1                                                         
         BE    ENDJOB                                                           
         MVC   SVKEY,KEYSAVE                                                    
         B     MAIN20                                                           
*                                                                               
ENDJOB   DS    0C                                                               
         GOTO1 =V(PRINTER)                                                      
         EDIT  COUNTER,(10,P),ZERO=NOBLANK,COMMAS=YES                           
         MVC   P+15(20),=C'RECORDS PUT TO FILE.'                                
         GOTO1 =V(PRINTER)                                                      
EXIT1    CLOSE (OUT)                                                            
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* GET ALL PROGRAMS FOR MARKET FOR DAY                                           
***********************************************************************         
GETQH    NTR1                                                                   
*                                                                               
*GET MARKET RECORD                                                              
         GOTO1 =V(DATAMGR),DMCB,(1,DMRDHI),=C'DEMDIR',KEY,IOAREA+4              
         XC    PREVMK,PREVMK       CLEAR PREVIOUS MARKET                        
         LA    R4,QHTABLE                                                       
         ST    R4,TBPOINTR         POINTER TO NEXT AVAIL SLOT IN TABLE          
*                                                                               
GET1     CLI   8(R1),0                                                          
         BNE   GETXNM                                                           
         MVC   KEYSAVE,IOAREA+4                                                 
*                                                                               
         LA    R3,IOAREA+4                                                      
         USING MLKEY,R3                                                         
         CLC   MLKEY(MLIND-MLKEY),KEY                                           
         BNE   GETXNM              NO MORE MARKET RECORDS FOR THIS BOOK         
         CLI   MLIND,MLINDEQU                                                   
         BNE   NXTMKDIR                                                         
         CLC   MLKMKT,=XL2'00'                                                  
         BNE   NXTMKDIR                                                         
         CLI   MLBTYP,0                                                         
         BNE   NXTMKDIR                                                         
         CLI   MLSTAT,X'F0'                                                     
         BNL   NXTMKDIR                                                         
GET2     MVC   MARKET,MLRMKT       SAVE MARKET NUMBER FOR FUTURE USE            
         OC    PREVMK,PREVMK                                                    
         BZ    GET3                FIRST MARKET                                 
         CLC   MARKET,PREVMK                                                    
         BE    GET3                SAME MARKET AS THE PREVIOUS                  
         B     GETX                NEW MARKET. UPDATE OUTPUT FILES              
*                                   AND PRINTOUT MARKET LIST                    
GETXNM   MVI   NOMORE,1                                                         
GETX     CLI   DAY,MTHRUF          LIST MARKETS ONLY FIRST TIME THRU            
         BNE   GET2A                                                            
*                                                                               
         EDIT  (2,PREVMK),(3,P),FILL=0      PRINT OUT MARKET#                   
*                                                                               
* GET MARKET NAME                                                               
         LHI   R0,MRKTNAMT         MARKET NAME TABLE INDICATOR                  
         ST    R0,DMCB                                                          
         GOTO1 VDEMTBOF,DMCB       GET A(MARKET NAME TABLE)                     
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   MKTNMLEN,6(R1)      L'TABLE ENTRY                                
MKNM10   CLC   =C'NT',0(RE)        USA NIELSEN TV                               
         BE    MKNM20                                                           
         ICM   RE,7,2(RE)          NF - TRY NEXT ONE                            
         OC    0(2,RE),0(RE)                                                    
         BNZ   MKNM10                                                           
         DC    H'0'                EOT - DIE SOMETHING WRONG                    
MKNM20   LA    RE,5(RE)            BYPASS HEADER                                
MKNM30   OC    0(2,RE),0(RE)                                                    
         BZ    NONAME              EOT - MARKET NOT FOUND                       
         USING MKNTABD,RE                                                       
         CLC   MKNNUM,PREVMK                                                    
         BE    MKNM40                                                           
         AH    RE,MKTNMLEN                                                      
         B     MKNM30                                                           
MKNM40   MVC   P+5(L'MKNNAME),MKNNAME                                           
         DROP  RE                                                               
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         B     GET2A                                                            
NONAME   MVC   P+5(11),=C'**UNKNOWN**'                                          
         GOTO1 =V(PRINTER)                                                      
GET2A    BAS   RE,TOFILE           SORT AND PUT RECORDS TO FILE                 
         LA    R4,QHTABLE          RESTORE POINTER TO AVAILABLE SLOT            
         ST    R4,TBPOINTR                                                      
         B     EXIT                                                             
*                                                                               
GET3     MVC   PREVMK,MARKET                                                    
*                                                                               
*GET STATION RECORD - DIRECTORY READ                                            
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING DRKEY,R2                                                         
         MVC   DRKEY(3),=C'RTN'                                                 
         MVC   DRSTAT,MLSTAT                                                    
         MVC   DRBOOK,MLBOOK                                                    
         MVC   DRKMKT,MLKMKT                                                    
         DROP  R2                                                               
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(1,DMRDHI),=C'DEMDIR',KEY,IOAREA+4              
GET5     CLI   8(R1),0                                                          
         BNE   NXTMKDIR                                                         
         MVC   IOSAVE,IOAREA+4                                                  
*                                                                               
         LA    R2,KEY                                                           
         USING DRKEY,R2                                                         
         CLC   DRKEY(DRSTYP-DRKEY),IOAREA+4                                     
         BNE   NXTMKDIR           CANT FIND STATION RECORD AS REQUIRED          
         LA    R2,IOAREA+4                                                      
         CLI   DRBTYP,X'00'                                                     
         BNE   NXTSTDIR                                                         
*                                                                               
         LA    RE,IOAREA+4                                                      
         MVC   KEY,0(RE)                                                        
         MVC   SVDA,DRNDXDA-DRKEY(RE)       DISK ADDRESS                        
         MVC   SVSTATUS,DRKSTAT-DRKEY(RE)   STATUS                              
*                                                                               
         LA    RE,IOAREA+4                                                      
         XC    0(23,RE),0(RE)                                                   
         MVC   0(18,RE),KEY                                                     
         OC    KEY+19(4),KEY+19                                                 
         BNZ   *+6                                                              
         DC    H'0'                NO D/A                                       
*                                                                               
         LA    R2,IOAREA+4                                                      
         MVC   DRRSTAT,SVSTATUS                                                 
         MVC   DRHIGHD,DAY                                                      
         MVC   PREVQH,=X'FF'                                                    
*                                                                               
*GET STATION RECORDS - FILE READ                                                
         GOTO1 =V(DATAMGR),DMCB,(1,DMRDHI),=C'DEMFIL',SVDA,IOAREA+4             
GET4     XC    IOAREA(4),IOAREA                                                 
         LH    RE,22(R1)                                                        
         LA    RE,4(RE)            LENGTH OF RECORD + 4                         
         STCM  RE,3,IOAREA                                                      
*                                                                               
         CLI   IOAREA+4,X'FF'                                                   
         BE    NXTSTDIR           NO MORE FILE RECORDS FOR STATION              
*                                                                               
         LA    R5,IOAREA+4                                                      
         MVI   ELCODE,QHCODEQ      X'20' ELEMENTS                               
         BAS   RE,GETEL                                                         
E20      BNE   E20X                                                             
         USING QHELEM,R5                                                        
         CLC   QHDAY,DAY                                                        
         BNE   NXTE20                                                           
         MVC   WEEKS,QHWKS                                                      
         NI    WEEKS,X'0F'         GET NIBBLE OF WEEKS                          
         BAS   RE,COUNTWKS         GET NUM OF WEEKS OF RUNTIME                  
         CLC   QHSQH,PREVQH        SAME QH?                                     
         BNE   TOTAB               NO. ADD TO TABLE                             
         CLC   NUMWKS,MAXNUM                                                    
         BL    NXTE20              RUNTIME < MAX SO FAR                         
         BH    *+14                RUNTIME > MAX SO FAR                         
         CLC   WEEKS,MAXWKS        RUNTIME = MAX SO FAR                         
         BNH   NXTE20              FIND WHICH PROGRAM AIRED IN FRST WKS         
         CLC   =C'4 WK AVG',QHPNAME                                             
         BE    NXTE20                                                           
         CLC   =C'AVG. ALL WKS',QHPNAME                                         
         BE    NXTE20                                                           
         L     R4,TBPOINTR         REPLACE TABLE ENTRY FOR SAME QH              
         SHI   R4,L'QHTABLE                                                     
         ST    R4,TBPOINTR                                                      
TOTAB    ST    R5,AE20ELEM                                                      
         BAS   RE,TOTABLE                                                       
         MVC   MAXNUM,NUMWKS       UPDATE MAX NUM OF WEEKS OF RUNTIME           
         MVC   MAXWKS,WEEKS                                                     
NXTE20   MVC   PREVQH,QHSQH                                                     
         CLI   QHELN,QHPNAME-QHELEM   NO PROGRAM NAME?                          
         BE    *+16                DON'T UPDATE PREVIOUS PROGRAM NAME           
         MVC   PREVPROG,QHPNAME                                                 
         MVC   PREVLEN,QHELN                                                    
         BAS   RE,NEXTEL                                                        
         B     E20                                                              
E20X     DS    0H                                                               
         DROP  R5                                                               
*                                                                               
NXTSTFIL LA    R2,IOAREA+4                                                      
         CLC   DRHIGHD,DAY                                                      
         BH    NXTSTDIR                                                         
         GOTO1 =V(DATAMGR),DMCB,(1,DMRSEQ),=C'DEMFIL',SVDA,IOAREA+4             
         B     GET4                                                             
NXTSTDIR MVC   KEY,IOSAVE                                                       
         GOTO1 =V(DATAMGR),DMCB,(1,DMREAD),=C'DEMDIR',KEY,IOAREA+4              
         GOTO1 =V(DATAMGR),DMCB,(1,DMRSEQ),=C'DEMDIR',KEY,IOAREA+4              
         B     GET5                                                             
NXTMKDIR MVC   KEY,KEYSAVE                                                      
         GOTO1 =V(DATAMGR),DMCB,(1,DMREAD),=C'DEMDIR',KEY,IOAREA+4              
         GOTO1 =V(DATAMGR),DMCB,(1,DMRSEQ),=C'DEMDIR',KEY,IOAREA+4              
         B     GET1                                                             
EXIT     XIT1                                                                   
*                                                                               
***SUBROUTINE TO COUNT THE NUMBER OF WEEKS OF PROGRAM RUNTIME***                
COUNTWKS NTR1                                                                   
         SR    R0,R0                                                            
         TM    WEEKS,X'01'                                                      
         BNO   *+8                                                              
         AHI   R0,1                                                             
         TM    WEEKS,X'02'                                                      
         BNO   *+8                                                              
         AHI   R0,1                                                             
         TM    WEEKS,X'04'                                                      
         BNO   *+8                                                              
         AHI   R0,1                                                             
         TM    WEEKS,X'08'                                                      
         BNO   *+8                                                              
         AHI   R0,1                                                             
         STCM  R0,1,NUMWKS                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***SUBROUTINE TO ADD A NEW RECORD TO THE TABLE***                               
TOTABLE  NTR1                                                                   
         L     R4,TBPOINTR                                                      
         USING QHTABD,R4                                                        
         L     R5,AE20ELEM                                                      
         USING QHELEM,R5                                                        
         MVC   MKNUM,MARKET        MARKET NUMBER                                
         MVC   QHNUM,QHSQH         QH                                           
*                                                                               
         LA    R2,IOAREA+4         STATION                                      
         MVC   STATN,DRSTAT                                                     
*                                                                               
         LA    R1,DAYCODES         DAY CODE                                     
TAB1     CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   QHDAY,0(R1)                                                      
         BE    *+12                                                             
         LA    R1,L'DAYCODES(R1)                                                
         B     TAB1                                                             
         MVC   DAYCODE,1(R1)                                                    
*                                                                               
         CLI   QHELN,QHPNAME-QHELEM    PROGRAM NAME AND LENGTH                  
         BNE   TAB2                                                             
         MVC   PGNAME,PREVPROG     GET NAME AND LENGTH FROM PREV ELEM           
         ZIC   R1,PREVLEN                                                       
         B     TAB3                                                             
TAB2     MVC   PGNAME,QHPNAME      GET NAME AND LENGTH FROM CURRNT ELEM         
         ZIC   R1,QHELN                                                         
TAB3     SHI   R1,QHPNAME-QHELEM                                                
         STC   R1,PGLEN                                                         
         LA    R4,L'QHTABLE(R4)                                                 
         ST    R4,TBPOINTR                                                      
         DROP  R4                                                               
         DROP  R5                                                               
         B     EXIT                                                             
***SUBROUTINE TO PUT RECORDS FROM TABLE TO THE FILE***                          
TOFILE   NTR1                                                                   
*                                                                               
         L     R3,TBPOINTR         SORT THE ENTRIES IN THE TABLE                
         S     R3,=A(QHTABLE)                                                   
         SR    R2,R2                                                            
         D     R2,=A(QHTABLQ)      NUMBER OF ENTRIES IN THE TABLE               
         GOTO1 =V(XSORT),DMCB,(0,QHTABLE),(R3),QHTABLQ,QHKEYLQ,0                
*                                                                               
         LA    R4,QHTABLE                                                       
         USING QHTABD,R4                                                        
         LA    R5,OUTDATA                                                       
         USING OUTRECD,R5                                                       
TOFILE1  C     R4,TBPOINTR                                                      
         BE    TOFILEX                                                          
         EDIT  MKNUM,OUTMK,FILL=0                                               
         ZIC   R9,QHNUM                                                         
         AHI   R9,1                                                             
         EDIT  (R9),OUTQH,FILL=0                                                
         MVC   OUTCALL,STATN                                                    
         MVI   OUTCALL+L'OUTCALL-1,C' '                                         
         MVC   OUTDAY,DAYCODE                                                   
         MVC   OUTPROG(15),=CL15' '                                             
         ZIC   R1,PGLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OUTPROG(0),PGNAME                                                
         PUT   OUT,OUTDATA                                                      
         LA    R4,L'QHTABLE(R4)                                                 
         L     R9,COUNTER                                                       
         AHI   R9,1                                                             
         ST    R9,COUNTER          UPDATE NUMBER OF RECORDS                     
         B     TOFILE1                                                          
TOFILEX  DS    0H                                                               
         DROP  R4                                                               
         DROP  R5                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
KEY      DS    XL23                                                             
KEYSAVE  DS    XL23                                                             
SVKEY    DS    XL23                                                             
IOSAVE   DS    XL23                                                             
MKTKEY   DS    XL23                                                             
DMRDHI   DC    C'DMRDHI'                                                        
DMRSEQ   DC    C'DMRSEQ'                                                        
DMREAD   DC    C'DMREAD'                                                        
IOAREA   DS    XL1024                                                           
IOAREA2  DS    XL1024                                                           
SVDA     DS    XL4                                                              
SVSTATUS DS    XL1                                                              
DAY      DS    X                                                                
SATDAY   EQU   X'60'                                                            
SUNDAY   EQU   X'70'                                                            
MTHRUF   EQU   X'95'                                                            
INIO     DS    XL80                                                             
DUB      DS    D                                                                
MKTNMLEN DS    H                   MKTNAME TABLE ENTRY LENGTH                   
WORK     DS    XL17                                                             
NOMORE   DS    X'0'                                                             
BOOK     DS    XL2                                                              
BOOK3    DS    XL3                                                              
PREVQH   DS    XL(L'QHSQH)                                                      
PREVPROG DS    XL14                                                             
PREVLEN  DS    XL(L'QHELN)                                                      
MARKET   DS    XL(L'MLRMKT)                                                     
PREVMK   DS    XL(L'MLRMKT)                                                     
WEEKS    DS    XL(L'QHWKS)                                                      
MAXWKS   DS    XL(L'QHWKS)                                                      
MAXNUM   DS    HL1                                                              
NUMWKS   DS    HL1                                                              
COUNTER  DC    F'0'                                                             
DEMTABOF DC    CL8'T00AD2'                                                      
VDEMTBOF DS    V                                                                
AE20ELEM DS    A                                                                
ELCODE   DS    X                                                                
         GETEL R5,23,ELCODE                                                     
*                                                                               
UTL      DS    0D                                                               
         DC    4X'00',X'0C'        UTL FOR DEMO SYSTEM                          
* DATA MANAGER LITERALS                                                         
*                                                                               
DMOPEN   DC    CL8'DMOPEN'                                                      
DMSYS    DC    CL8'DEMO'                                                        
DMFLIST  DC    0C                                                               
         DC    C'NDEMDIRN'                                                      
         DC    C'NL=DEMFN'                                                      
         DC    C'NDEMDIRA'                                                      
         DC    C'NL=DEMFA'                                                      
         DC    C'NDEMDIRR'                                                      
         DC    C'NL=DEMFR'                                                      
         DC    C'NNTIDIR '                                                      
         DC    C'NL=NTIFL'                                                      
         DC    C'NPAVDIR '                                                      
         DC    C'NL=PAVFL'                                                      
         DC    C'NCTFILE '                                                      
         DC    C'X'                                                             
         EJECT                                                                  
*                                                                               
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00026,                                            X        
               BLKSIZE=02600,                                          X        
               MACRF=PM                                                         
         EJECT                                                                  
         DC    CL8'*DMCB***'                                                    
DMCB     DS    6F                                                               
*                                                                               
DAYCODES DS    0XL2                                                             
         DC    X'60',C'7'          SATURDAY                                     
         DC    X'70',C'8'          SUNDAY                                       
         DC    X'95',C'9'          M-FR                                         
         DC    X'FF'                                                            
*                                                                               
         DS    0F                                                               
         DC    CL8'*OUTDAT*'                                                    
OUTDATA  DS    XL1000                                                           
TBPOINTR DS    A                                                                
*                                                                               
QHTABLE  DS    3000XL(QHTABLQ)                                                  
SSB      DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'00' NO RECOVERY                                  
         ORG                                                                    
         EJECT                                                                  
*                                                                               
*DSECT TO COVER QHTABLE                                                         
QHTABD   DSECT                                                                  
MKNUM    DS    HL2                                                              
DAYCODE  DS    CL1                                                              
QHNUM    DS    HL1                                                              
STATN    DS    CL5                                                              
QHKEYLQ  EQU   *-QHTABD                                                         
PGLEN    DS    HL1                                                              
PGNAME   DS    CL14                                                             
QHTABLQ  EQU   *-QHTABD                                                         
         EJECT                                                                  
*                                                                               
*DSECT TO COVER THE OUTPUT RECORD                                               
OUTRECD  DSECT                                                                  
OUTMK    DS    CL3                                                              
OUTQH    DS    CL2                                                              
OUTCALL  DS    CL5                                                              
OUTDAY   DS    CL1                                                              
OUTOVQ   EQU   *-OUTRECD                                                        
OUTPROG  DS    CL14                                                             
         DS    C                                                                
         EJECT                                                                  
* INCLUDE DDDPRINT                                                              
* INCLUDE DEDEMFILE                                                             
* INCLUDE DEDEMTABD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'216DETITLES  04/10/13'                                      
         END                                                                    
