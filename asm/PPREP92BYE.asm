*          DATA SET PPREP92BYE AT LEVEL 004 AS OF 05/01/02                      
*          DATA SET PPREP02GST AT LEVEL 049 AS OF 01/04/91                      
*PHASE PP0202G,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE DATCON                                                                 
         TITLE 'PP0202BYE - ENSURE ESTIMATE EXISTS FOR BUY'                     
* OVERVIEW                                                                      
*                                                                               
* 1- BUILD TABLE OF ESTIMATES FOR A AGENCY MEDIA WITH START AND END             
*     DATES                                                                     
* 2- LOOKUP BUYS AND PRINT IF NOT IN EST TABLE                                  
*           KEEP COUNT OF ACTIVE AND DELETED BAD BUYS FOR AGENCY/MED            
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
         XC    TMEDIA,TMEDIA                                                    
         L     RE,=A(BADCOUNT)                                                  
         L     RF,=A(3000)                                                      
         XCEF                                                                   
         BAS   RE,CLEARTAB                                                      
         XC    THISCNT,THISCNT                                                  
         OI    THISACT+2,12                                                     
         MVC   THISDEL(6),THISACT                                               
         OPEN (IN,(INPUT))                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         BAS   RE,TAPEGET                                                       
PROCA01  DS    0H                                                               
         CLC   REC(4),=X'FFFFFFFF'                                              
         BE    EXIT                                                             
*                                                                               
*                                                                               
PROCA011 DS    0H                                                               
*                                                                               
PROCA02  CLI   REC+3,2              CLIENT RECORD                               
         BNE   CKFORBUY                                                         
*        CHECK TO SEE IF THERE ARE ANY ERRORS   AND LOAD INTO BADTAB            
         BAS   RE,CLEARTAB                                                      
         CP    THISBADD,=P'0'                                                   
         BNE   PROCNZ                                                           
         CP    THISACT,=P'0'                                                    
         BNE   PROCNZ                                                           
         CP    THISDEL,=P'0'                                                    
         BE    FNDCLR                                                           
PROCNZ   L     R1,=A(BADCOUNT)                                                  
         LA    RE,200                                                           
PROCAGN  CLI   0(R1),0                                                          
         BE    FNDEMPTY                                                         
         LA    R2,14(R1)                                                        
         BCT   RE,PROCAGN                                                       
         DC    H'0'            TABLE EXCEEDED                                   
FNDEMPTY MVC   0(15,R1),THISCNT                                                 
FNDCLR   XC    THISCNT,THISCNT                                                  
         MVC   THISAGY(3),REC      AGENCY MEDIA                                 
         MVC   THISAGY+3(3),REC+4      CLIENT                                   
         ZAP   THISACT,=P'0'                                                    
         MVC   THISDEL(L'THISDEL*2),THISACT                                     
         B      PROC                                                            
*                                                                               
CKFORBUY DS    0H                                                               
         CLI   REC+3,X'20'                                                      
         BE    PROCBUY                                                          
         CLI   REC+3,X'07'                                                      
         BNE   PROC        GET NEXT                                             
         CLC   THISAGY(3),REC                                                   
         BNE   *-2              SERIOUS ERROR                                   
         CLC   THISAGY+3(3),REC+4                                               
         BNE   *-2              SERIOUS ERROR                                   
         XC    WORK,WORK                                                        
         LA    RF,PESTREC                                                       
         USING PESTRECD,RF                                                      
         MVC   WORK(5),PESTKPRD  MOVE PRODUCT/EST                               
         GOTO1 =V(DATCON),PUBDMWRK,PESTST,(3,WORK+5)                            
         GOTO1 =V(DATCON),PUBDMWRK,PESTEND,(3,WORK+8)                           
         L      R4,=A(TABLE)                                                    
         BAS    RE,LOADTAB                                                      
         B     PROC                                                             
* FOUND FIRST AGENCY RECORD                                                     
*                                                                               
**************                                                                  
*  BEGIN PROCESSING TAPE                                                        
**************                                                                  
         SPACE 3                                                                
*****                                                                           
PROCBUY  CLC   THISAGY(3),REC    SAME AGENCY MED CLINET                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   THISAGY+3(3),REC+4    SAME AGENCY MED CLINET                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK(9),WORK CLEAR                                               
         MVC   WORK(3),REC+7        PRODUCT                                     
         MVC   WORK+3(2),REC+19     ESTIMATE                                    
         L     R4,=A(TABLE)                                                     
         BAS   RE,FINDENT                                                       
         CH    R4,=H'255'                                                       
         BNE   CKDATES                                                          
*        MISSING ESTIMATE FOR BUY                                               
         MVC   KEY,REC                                                          
         BAS   RE,DMPKEY                                                        
         MVC   P+30(26),=C'MISSING ESTIMATE'                                    
         TM    REC+25,X'80'                                                     
         BO    THISDL                                                           
         AP    THISACT,=P'1'                                                    
         B     PROC                                                             
THISDL   AP    THISDEL,=P'1'                                                    
*                                                                               
ERRCOM   GOTO1 REPORT                                                           
         B     PROC                                                             
*                                                                               
CKDATES  CLC   5(3,R4),REC+16    START EST DATE TO BUY INS DATE                 
         BL    OUTOFRAN                                                         
         CLC   8(3,R4),REC+16                                                   
         BH    OUTOFRAN                                                         
         B     PROC                                                             
*                                                                               
OUTOFRAN MVC   KEY,REC                                                          
         BAS   RE,DMPKEY                                                        
         MVC   P+30(29),=C'EST EXISTS BUT BUY ID OUTSIDE'                       
         AP    THISBADD,=P'1'                                                   
         B     ERRCOM                                                           
         EJECT                                                                  
**************************************************                              
*  CLEAR ESTIMATE TABLE                                                         
**************************************************                              
*                                                                               
BUILD    BAS   RE,CLEARTAB                                                      
*                                                                               
FINISHD  XIT1                                                                   
           SPACE 3                                                              
*                                                                               
**************************************************                              
*       CLEAR EST                                                               
**************************************************                              
         SPACE 3                                                                
CLEARTAB NTR1                                                                   
         L     RE,=A(TABLE)                                                     
         L     RF,=A(13000)                                                     
         PRINT GEN                                                              
         XCEF                                                                   
         XIT1                                                                   
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
*                                                                               
**************************************************                              
*       LOAD  EST PUB TABLE                                                     
**************************************************                              
         SPACE 3                                                                
LOADTAB  NTR1              LOAD TABLE R4 POINTS TO ENTRY TO BE ADDED            
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK),(R4)                           
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                OUT OF ROOM                                  
         L     R4,0(R1)                                                         
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
         B     NOPRT      NO PRINTING OF INPUT                                  
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
TAGENCY  DS    CL6   AG/MED/CLT                                                 
TMEDIA   DC    X'0'                                                             
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
INPCNT   DC    PL8'0'                                                           
         DC    CL15'RECORDS IN    '                                             
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
         MVC   P(46),=C' AGENCY- MED- CLIENT- MISSING EST  - BAD DATES'         
         GOTO1 REPORT                                                           
         MVC   P(46),=C'                      DELETED/ ACTIV          '         
         GOTO1 REPORT                                                           
         L     R4,=A(BADCOUNT)                                                  
LOOPTH   CLI   0(R4),0                                                          
         BE    EXIT                                                             
         MVC   P+2(2),0(R4)                                                     
         MVC   P+9(1),2(R4)                                                     
         MVC   P+14(3),3(R4)       CLIENT                                       
         OI    P+8,15                                                           
         CP    6(3,R4),=P'0'                                                    
         BE    *+10                                                             
         UNPK  P+23(5),6(3,R4)                                                  
         OI    P+11,15                                                          
         CP    9(3,R4),=P'0'                                                    
         BE    *+10                                                             
         UNPK  P+30(5),9(3,R4)                                                  
         OI    P+14,15                                                          
         CP   12(3,R4),=P'0'                                                    
         BE    *+10                                                             
         UNPK  P+40(5),12(3,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,15(R4)                                                        
         B     LOOPTH                                                           
         EJECT                                                                  
*                             LINK TO REPORT                                    
         EJECT                                                                  
*                             GET FIRST/NEXT PUB                                
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
THISCNT  DS    0CL15                                                            
THISAGY  DS    CL2                                                              
THISMED  DS    CL1                                                              
THISCLT  DS    CL3                                                              
THISACT  DS    PL3        ACTIVE BUY MISSING EST                                
THISDEL  DS    PL3        DELETED BUY MISSING EST                               
THISBADD DS    PL3        BUY OUTSIDE OF EST START & END DATE                   
BINPARMS DC    A(0)                                                             
BINATAB  DC    A(TABLE)                                                         
BINCOUNT DC    A(0)                RECORD COUNT                                 
         DC    A(11)                LENGTH                                      
         DC    AL1(0),AL3(5)    PROD/EST                                        
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
*                                                                               
         DS    F   FOR REC LEN                                                  
REC      DS    4000C                                                            
TABLE    DS    11CL1000                                                         
BADCOUNT DS    3000C     AG(2) M(1) CLT(3) DEL(3) ACT(3) BAD(3)                 
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
         PRINT ON                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PPREP92BYE05/01/02'                                      
         END                                                                    
