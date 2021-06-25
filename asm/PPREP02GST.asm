*          DATA SET PPREP02GST AT LEVEL 051 AS OF 05/01/02                      
*PHASE PP0202G                                                                  
*INCLUDE MININAM                                                                
*INCLUDE BINSRCH2                                                               
         TITLE 'PP0202 - GST TAX BUY REC MODIFICATION'                          
* OVERVIEW                                                                      
*  TO MOVE VALUE OF GST IN PUB REC TO BUY RECORD                                
* 1- READ CANADIAN AGENCIES PUB FILE AND CREATE TABLE OF PUB NO.                
*     WITH EXCEPTIONS.  IF PUB GST IS BLANK, ZERO OR S DO NOT LOAD              
*     CREATE THIS TABLE ON CHANGE OF MEDIA.                                     
* 2- LOOKUP TABLE BASED ON BUY RECORD AGENCY/PUB#/ZONE/ED                       
*           IF NOT FOUND MOVE STANDARD CODE ('S') - OTHERWISE MOVE              
*           GST CODE                                                            
*                                                                               
PP0202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0202                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         BAS   RE,RUNX                                                          
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INPCNT,=P'0'                                                     
         ZAP   OUTCNT,=P'0'                                                     
         OPEN (IN,(INPUT))                                                      
         OPEN (OUT,(OUTPUT))                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         BAS   RE,TAPEGET                                                       
PROCA01  DS    0H                                                               
         LA    RF,AGENCYS                                                       
         CLC   REC(4),=X'FFFFFFFF'                                              
         BNE   PROCA011                                                         
         BAS   RE,TAPEPUT                                                       
         B     EXIT                                                             
*                                                                               
*                                                                               
PROCA011 CLC   REC(2),0(RF)           AGENCY M/B IN AGENCY TABLE                
         BE    PROCA02                                                          
         LA    RF,2(RF)                                                         
         CLI   0(RF),255                                                        
         BE    PROCA                IF NOT PUT AND GET TAPE                     
         B     PROCA011                                                         
*                                                                               
PROCA02  CLI   REC+3,1              AGENCY RECORD                               
         BE    FNDAGEN                                                          
PROCA    BAS   RE,TAPEPUT                                                       
         B     PROC                                                             
* FOUND FIRST AGENCY RECORD                                                     
*                                                                               
FNDAGEN  MVC   TMEDIA,REC+2                                                     
         MVC   TAGENCY,REC         SAVE AGENCY CODE                             
         BAS   RE,BUILD                 BUILD TABLE                             
**************                                                                  
*  BEGIN PROCESSING TAPE                                                        
**************                                                                  
ISITABUY CLI   REC+3,X'20'         BUY RECORD                                   
         BE    PROCBUY                                                          
PUTGET   BAS   RE,TAPEPUT                                                       
         BAS   RE,TAPEGET                                                       
         CLC   TAGENCY,REC         CHANGE IN AGENCY                             
         BNE   CLR                         LOOK FOR A AGENCY RECORD             
         CLC   TMEDIA,REC+2        ENSURE IN SAME MEDIA                         
         BE    ISITABUY                                                         
*        *                                                                      
*    CLEAR TABLE AND INITIALIZE BINSRCH PARMS                                   
*        *                                                                      
CLR      MVI  P+50,C'='                                                         
         MVC   P+51(80),P+50                                                    
         MVC   P+55(17),=C' FOR AGENCY      '                                   
         MVC   P+67(2),TAGENCY                                                  
         MVC   P+70(4),=C'MEDIA'                                                
         MVC   P+75(1),TMEDIA                                                   
         BAS   RE,RUNX                                                          
         BAS RE,CLEARTAB                                                        
         XC    BINCOUNT,BINCOUNT                                                
         ZAP   GSTREC,=P'0'                                                     
         ZAP   PUBCNT,=P'0'                                                     
        ZAP    TABLEC,=P'0'                                                     
         B     PROCA01                                                          
         SPACE 3                                                                
*****                                                                           
PROCBUY  CLC   TAGENCY,REC                                                      
         BNE   PUTGET               IF NOT PUT AND GET TAPE                     
*                                                                               
LOOK4PUB XC    WORK(9),WORK                                                     
         MVC   WORK(2),REC          AGEN CY                                     
         MVC   WORK+2(6),REC+10    PUB/ZONE/EDITION                             
         L     R4,=A(TABLE)                                                     
         BAS   RE,FINDENT                                                       
         LA    RF,REC+33                                                        
         USING PBDELEM,RF                                                       
         CLI   1(RF),X'74'                                                      
         BE    RIGHTLEN                                                         
         MVC   KEY,REC                                                          
         BAS   RE,DMPKEY                                                        
         GOTO1 REPORT                                                           
         B     PUTGET                                                           
*                                                                               
RIGHTLEN CH    R4,=H'255'                                                       
         BNE   GSTFND                                                           
         MVI   PBDGST,C'S'                                                      
APGST    AP    GSTREC,=P'1'                                                     
         MVI   PBDCNDA,X'80'       CANADIAN PUB                                 
         CP    GSTREC,=P'25'                                                    
         BH    PUTGET                                                           
         MVC   P(9),=C'GST OUT//'                                               
         MVC   P+12(4),=C'REC='                                                 
         UNPK  P+17(9),GSTREC+4(4)                                              
         OI    P+25,X'F0'                                                       
         GOTO1 REPORT                                                           
         BAS   RE,DMPREC                                                        
         B     PUTGET                                                           
*                                                                               
GSTFND   MVC   PBDGST,8(R4)                                                     
         B     APGST                                                            
         DROP  RF                                                               
         SPACE 3                                                                
TAPEMED  DC    X'0'                                                             
         EJECT                                                                  
**************************************************                              
*  BUILD TABLE FOR MEDIA  FOR SELECTED AGENCIES                                 
**************************************************                              
*                                                                               
BUILD    NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(1),TMEDIA       MEDIA                                        
*--------------\                                                                
         GOTO1 HIGHPUB                                                          
*--------------/                                                                
         B     SAMEDIA                                                          
         SPACE 2                                                                
*--------------\                                                                
PUBSEQ   GOTO1 SEQPUB                                                           
*--------------/                                                                
         CLI   DMCB+8,X'80'                                                     
         BE    FINISHD                                                          
*                                                                               
SAMEDIA  CLC   TMEDIA(1),KEY        SAME MEDIA                                  
         BNE   FINISHD                                                          
         CLI   KEY+9,X'81'                                                      
         BNE   PUBSEQ                                                           
         CLC   KEY+7(2),TAGENCY                                                 
         BNE   PUBSEQ                                                           
*--------------\                                                                
READIT   GOTO1 GETNAME                                                          
*--------------/                                                                
         CLI   DMCB+8,0                                                         
         BNE   *-2                                                              
         AP    PUBCNT,=P'1'                                                     
         CLI   PUBGST,0                                                         
         BE    PUBSEQ                                                           
         CLI   PUBGST,C'S'                                                      
         BE    PUBSEQ                                                           
         CLI   PUBGST,C' '                                                      
         BE    PUBSEQ                                                           
***************                                                                 
*  BUILD TABLE*                                                                 
***************                                                                 
*                                                                               
           XC  WORK(9),WORK                                                     
           MVC WORK(2),PUBKAGY                                                  
           MVC WORK+2(6),PUBKPUB                                                
           MVC  WORK+8(1),PUBGST                                                
           L   R4,=A(TABLE)                                                     
           BAS  RE,LOADTAB                                                      
           B    PUBSEQ                                                          
*                                                                               
FINISHD  XIT1                                                                   
           SPACE 3                                                              
*                                                                               
**************************************************                              
*       CLEAR GST PUB TABLE                                                     
**************************************************                              
         SPACE 3                                                                
CLEARTAB NTR1                                                                   
         L     RE,=A(TABLE)                                                     
         L     RF,=A(90000)                                                     
         PRINT GEN                                                              
         XCEF                                                                   
         XIT1                                                                   
         PRINT NOGEN                                                            
*                                                                               
AGENCYS  DC    CL2'BA',C'BE',C'CD',C'DA',C'DV',C'GR'     6                      
         DC    CL2'MW',C'PA',C'PB',C'PJ'                 4                      
         DC    CL2'PT',C'SA',C'SM',C'WS'                 4                      
         DC    CL2'YR',C'VB',C'HD'                   3                          
         DC    CL2'MI',C'RC'                   2                                
         DC    X'FFFF'                          ------>19  CANADIAN AGY         
*                                                                               
*                                                                               
**************************************************                              
*       LOAD  GST PUB TABLE                                                     
**************************************************                              
         SPACE 3                                                                
LOADTAB  NTR1              LOAD TABLE R4 POINTS TO ENTRY TO BE ADDED            
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK),(R4)                           
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                OUT OF ROOM                                  
         L     R4,0(R1)                                                         
         AP    TABLEC,=P'1'                                                     
         PRINT GEN                                                              
LOADX    XIT1  REGS=(R4)                                                        
*                                                                               
**************************************************                              
*      FIND GST PUB TABLE ELEMENT FOR PUB                                       
**************************************************                              
         SPACE 3                                                                
FINDENT  NTR1                       R4 TO POINT TO SEARCH                       
         GOTO1 =V(BINSRCH),BINPARMS,(X'00',WORK),(R4)                           
         L     R4,0(R1)                                                         
         CLI   0(R1),1               NOT FOUND                                  
         BNE   LOADX                                                            
         LA    R4,255                                                           
         B     LOADX                                                            
*                                                                               
*                                                                               
TAPEGET  NTR1                                                                   
GETINTA  AP    INPCNT,=P'1'                                                     
         GET   IN,REC-4                                                         
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0           END OF RECORD                                  
         CLI   REC+3,255                                                        
         BE    EOFREC                                                           
         CLI   REC+3,X'02'                                                      
         BL    EOFREC                                                           
         CLI   REC+3,X'20'                                                      
         BNE   NOPRT                                                            
         CP    INPCNT,=P'20'                                                    
         BH    NOPRT                                                            
EOFREC   LA    R5,REC                                                           
         ST    R5,AREC                                                          
         MVC   P(9),=C'INPUT  //'                                               
         MVC   P+12(4),=C'REC='                                                 
         UNPK  P+17(9),INPCNT+4(4)                                              
         OI    P+25,X'F0'                                                       
         GOTO1 REPORT                                                           
         BAS   RE,DMPREC                                                        
NOPRT    B     EXIT                                                             
*                                                                               
TAGENCY  DC    C'  '                                                            
TMEDIA   DC    X'0'                                                             
*                                                                               
TAPEPUT  NTR1                                                                   
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
         CLI   REC+3,255                                                        
         BE    CNTSEEP                                                          
         CLI   REC+3,X'02'                                                      
         BL    CNTSEE                                                           
         CLI   REC+3,X'20'                                                      
         BNE   XIT                                                              
CNTSEE   AP    DMPCNT,=P'1'                                                     
         CP    DMPCNT,=P'50'                                                    
         BH    XIT                                                              
CNTSEEP  MVC   P(6),=C'OUTPUT'                                                  
         MVC   P+10(4),=C'REC='                                                 
         UNPK  P+15(9),OUTCNT+4(4)                                              
         OI    P+23,X'F0'                                                       
         GOTO1 REPORT                                                           
         BAS   RE,DMPREC                                                        
XIT      XIT1                                                                   
*                                                                               
DMPCNT   DC    PL5'0'                                                           
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
INPCNT   DC    PL8'0'                                                           
         DC    CL15'RECORDS IN    '                                             
OUTCNT   DC    PL8'0'                                                           
         DC    CL15'RECORDS OUT    '                                            
GSTREC   DC    PL8'0'                                                           
         DC    CL15'GSTREC CHANGES '                                            
PUBCNT   DC    PL8'0'                                                           
         DC    CL15'PUBS READ      '                                            
TABLEC   DC    PL8'0'                                                           
         DC    CL15'TABLE ENTRIES  '                                            
         DC    X'FF'                                                            
*                                                                               
*                                                                               
RUNX     NTR1                                                                   
         DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
EOF      CLOSE (IN,)                                                            
         CLOSE (OUT,)                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                             LINK TO REPORT                                    
         EJECT                                                                  
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1                                                                   
         SPACE 2                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         IC    R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(2),PAGYKAGY                                                
         GOTO1 HIGHPUB                                                          
         B     NP2B                                                             
*                                                                               
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
NP2B     DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NPX                                                              
         CLC   KEY+7(2),KEYSAVE+7                                               
         BNE   NP2                                                              
         CLI   KEY+9,X'81'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LASTPUB,KEY+1                                                    
         GOTO1 GETNAME                                                          
         L     RF,ALTLREC                                                       
         XC    0(50,RF),0(RF)                                                   
         GOTO1 SEQPUB                                                           
         CLC   KEY(9),PUBKEY                                                    
         BNE   NPX                                                              
         GOTO1 GETLTL                                                           
*                                                                               
NPX      DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         LA    R5,REC                                                           
         LA    R2,280                                                           
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
*                                                                               
         SPACE 3                                                                
RPRT     NTR1                                                                   
MYSPACE  GOTO1  REPORT                                                          
         B     EXIT                                                             
         SPACE 3                                                                
BINPARMS DC    A(0)                                                             
BINATAB  DC    A(TABLE)                                                         
BINCOUNT DC    A(0)                RECORD COUNT                                 
         DC    A(9)                LENGTH                                       
         DC    AL1(0),AL3(8)                                                    
BINMAX   DC    A(10000)            MAX NUMBER OF RECORDS                        
BIG      DS    24F                                                              
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         LTORG                                                                  
*                                                                               
IN       DCB   DDNAME=IN,                                              X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
*                                                                               
         DS    F   FOR REC LEN                                                  
REC      DS    4000C                                                            
TABLE    DS    9CL10000                                                         
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051PPREP02GST05/01/02'                                      
         END                                                                    
