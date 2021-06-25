*          DATA SET PSETSER    AT LEVEL 052 AS OF 05/01/02                      
*PHASE PSETSERA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE RECUP                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
         TITLE 'PSETSER- CREATE SERIAL NUMBER ELEMENTS'                         
*                                                                               
*        ADDS SEQUENTIAL SERIAL NUMBERS TO ALL INSERTIONS                       
*        SEQUENCED WITHIN AGY/MEDIA/CLIENT                                      
*                                                                               
*        CONTROL CARDS                                                          
*        DUMP=NNNN  TO DUMP FIRST NNNN INSERTIONS                               
*                                                                               
PSETSER CSECT                                                                   
         NBASE 0,PSETSER,=V(REGSAVE)                                            
         SPACE 2                                                                
         LA    R6,PSETSER+4095                                                  
         LA    R6,1(R6)                                                         
         USING PSETSER+4096,R6                                                  
*                                  NOTE USE OF R6 AS BASE REGISTER              
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*&&DO                                                                           
         OPEN  IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*&&                                                                             
*                                                                               
*                                  SET LOOKUP TABLE BINSRCH PARS                
*                                                                               
START1   DS    0H                                                               
         XC    LASTAMC,LASTAMC       CLEAR LAST AGY/MEDIA/CLIENT                
*                                                                               
         BAS   RE,CARDS                                                         
         CLC   =C'/*',CARD                                                      
         BE    START10                                                          
         CLC   =C'DUMP=',CARD                                                   
         BNE   START2                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
         B     START1                                                           
START2   DS    0H                                                               
         CLC   =C'PRINT',CARD                                                   
         BNE   START3                                                           
         MVI   PRTSW,C'Y'                                                       
         B     START1                                                           
START3   DS    0H                                                               
         B     START1                                                           
         EJECT                                                                  
START10  DS    0H                                                               
GET      DS    0H                                                               
         BAS   RE,GETREC                                                        
         CLI   EOFSW,C'Y'                                                       
         BE    EOJ                                                              
*                                                                               
*              ADD CODE FOR THIS RUN HERE                                       
*                                                                               
         CLC   REC+25(2),=X'80FF'     DELETED DIRECTORY ONLY                    
         BE    GET                 SKIP                                         
         CLC   REC+25(2),=X'FFFF'     DELETED DIRECTORY ONLY                    
         BE    GET                 SKIP                                         
*                                                                               
         CLC   REC(3),=X'D40000'   BAD RECORD                                   
         BE    GET                 JUST SKIP IT                                 
*                                                                               
         CLC   REC(7),=X'E2D1D417C7D700'                                        
         BE    GET                     BAD RECORD ON TST PRINTFILE              
         CLC   REC(10),=X'E2D1D517E9E9E9C7D7F1'                                 
         BE    GET                     BAD RECORD ON TST PRINTFILE              
         CLC   REC(14),=X'E2D1D53A000000C7D740404040C2'                         
         BE    GET                   BAD RECORD ON TST PRINTFILE                
*                                                                               
         CLI   REC+3,X'20'         BUY                                          
         BE    BUY                                                              
         BNE   PUTXX                                                            
*                                                                               
         SPACE 3                                                                
BUY      DS    0H                                                               
         CLC   REC(7),LASTAMC     CHECK FOR SAME CLIENT                         
         BE    BUY10                                                            
         OC    LASTAMC,LASTAMC   SEE IF FIRST                                   
         BZ    BUY05                                                            
         MVC   P+1(2),LASTAMC    AGENCY                                         
         MVC   P+5(1),LASTAMC+2  MEDIA                                          
         MVC   P+7(3),LASTAMC+4  CLIENT CODE                                    
         EDIT  MYDUB,(10,P+13),0                                                
         BAS   RE,PRNT                                                          
*                                                                               
BUY05    MVC   LASTAMC,REC       SAVE AGY/MED/REC CODE/CLIENT                   
         ZAP   MYDUB,=P'0'       SET FOR FIRST SERIAL NUMBER                    
*                                WILL GET INCREMENTED AT BUY20                  
*                                                                               
BUY10    DS    0H                                                               
*                                                                               
         MVI   DMPSW,C'Y'                                                       
         CP    DMPCNT,=P'1'                                                     
         BNH   BUY12                                                            
         MVC   P+1(12),=C'** BEFORE **'                                         
         BAS   RE,PRNT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
BUY12    DS    0H                                                               
         LA    R2,REC+33                                                        
         MVI   ELCODE,X'99'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   BUY20                                                            
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(RECUP),DMCB,(1,REC),(R2)     REMOVE OLD                       
         AP    OLDCNT,=P'1'                                                     
*                                                                               
         SR    R4,R4                                                            
         IC    R4,REC+25                                                        
         SLL   R4,8                                                             
         IC    R4,REC+26                                                        
         SR    RE,RE                                                            
         LA    RE,REC                                                           
         AR    RE,R4                                                            
         SR    RF,RF                                                            
         LA    RF,REC+999                                                       
         AH    RF,=H'3000'                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
         B     BUY12        TRY AGAIN JUST IN CASE                              
*                                                                               
BUY20    DS    0H                                                               
         CLC   PBUYKDAT,=X'640101'     SEE IF BEFORE JAN01/00                   
         BL    BUYX         DON'T ADD SERIAL NUMBER ELEMENTS                    
*                                                                               
         AP    MYDUB,=P'1'       INCREMENT FOR NEXT BUY                         
         XC    ELEM,ELEM                                                        
         LA    R8,ELEM                                                          
         USING PSERELED,R8                                                      
         MVI   PSERELEM,X'99'                                                   
         MVI   PSERLEN,X'09'     LENGTH                                         
         MVC   PSERNUM,MYDUB+3   SERIAL NUMBER                                  
*                                                                               
         LA    R2,REC+33                                                        
         MVI   ELCODE,X'FF'      GET ME TO END OF RECORD                        
         BAS   RE,NEXTEL                                                        
         GOTO1 =V(RECUP),DMCB,(1,REC),ELEM,(R2)                                 
         AP    BUYCNT,=P'1'                                                     
         CP    DMPCNT,=P'1'                                                     
         BNH   BUYX                                                             
         MVC   P+1(11),=C'** AFTER **'                                          
         BAS   RE,PRNT                                                          
BUYX     B     PUT                                                              
         SPACE 3                                                                
         DROP  R8                                                               
*                                                                               
PUT      DS    0H                                                               
PUTXX    BAS   RE,PUTREC                                                        
         B     GET                                                              
         SPACE 2                                                                
*&&DO                                                                           
EOF      CLOSE IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
EOF      CLOSE (IN,)                                                            
         CLOSE (OUT,)                                                           
*&&                                                                             
         BAS   RE,PRNT                                                          
         LA    R3,COUNTS                                                        
         LA    R4,25                                                            
         LA    R5,COUNTSX                                                       
*                                                                               
EOF2     MVC   P+1(20),5(R3)                                                    
         OI    4(R3),X'0F'                                                      
         UNPK  P+22(7),0(5,R3)                                                  
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF2                                                       
         MVI   EOFSW,C'Y'                                                       
         XIT1                                                                   
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         SPACE 2                                                                
SKIP     MVC   PCOM,=C'BC01'                                                    
         ZAP   LNCNT,=P'0'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT3    MVC   PCOM,=C'BL03'                                                    
         AP    LNCNT,=P'3'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT2    MVC   PCOM,=C'BL02'                                                    
         AP    LNCNT,=P'2'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT     MVC   PCOM,=C'BL01'                                                    
         AP    LNCNT,=P'1'                                                      
*                                                                               
PRNTR    NTR1                                                                   
*                                                                               
         GOTO1 =V(PRINT),DMCB,P,PCOM                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
         B     XIT                                                              
         SPACE 3                                                                
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         LA    R2,4(R2)                                                         
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
DMPREC   NTR1                                                                   
*                                                                               
         LA    R5,REC-4                                                         
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R2,4(R2)                                                         
         LA    R3,0(R5,R2)         EOR                                          
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   XIT                                                              
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 =V(HEXOUT),DMCB,(R5),WORK,(R4),=C'N'                             
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,PRNT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         B     XIT                                                              
         SPACE 3                                                                
DMPKEY   NTR1                                                                   
*                                                                               
         LA    R5,REC                                                           
         LA    R2,KLEN                                                          
         GOTO1 =V(HEXOUT),DMCB,(R5),P+01,(R2),=C'N'                             
*                                                                               
         MVC   WORK(KLEN),0(R5)                                                 
         TR    WORK(KLEN),TRTAB                                                 
         MVC   P+75(KLEN),WORK                                                  
         B     XIT                                                              
         SPACE 3                                                                
GETREC   NTR1                                                                   
         GET   IN,REC-4                                                         
*                                                                               
         CLC   REC+25(2),=X'80FF'     CHECK FOR DIRECTORY ONLY-DELETED          
         BE    GETRECA                SKIP END OF RECORD SET                    
         CLC   REC+25(2),=X'FFFF'     CHECK FOR DIRECTORY ONLY-DELETED          
         BE    GETRECA                SKIP END OF RECORD SET                    
*                                                                               
         CLC   REC(3),=X'D40000'                                                
         BE    GETRECX                 BAD RECORD ON TST PRINTFILE              
         CLC   REC(7),=X'E2D1D417C7D700'                                        
         BE    GETRECX                 BAD RECORD ON TST PRINTFILE              
         CLC   REC(10),=X'E2D1D517E9E9E9C7D7F1'                                 
         BE    GETRECX                 BAD RECORD ON TST PRINTFILE              
         CLC   REC(14),=X'E2D1D53A000000C7D740404040C2'                         
         BE    GETRECX                 BAD RECORD ON TST PRINTFILE              
*                                                                               
         CLI   REC+3,X'3A'           DIRECTORY ONLY REC                         
         BL    GETREC5               3A-3F                                      
         CLI   REC+3,X'3F'           DIRECTORY ONLY REC                         
         BNH   GETRECA                                                          
         CLI   REC+3,X'E0'           DIRECTORY ONLY REC                         
         BL    GETREC5               E0-E5                                      
         CLI   REC+3,X'E5'           DIRECTORY ONLY REC                         
         BNH   GETRECA                                                          
*                                                                               
GETREC5  MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0             EOR                                          
GETRECX  AP    INCNT,=P'1'                                                      
*                                                                               
         B     XIT                                                              
*                                                                               
GETRECA  AP    DIRCNT,=P'1'        DIRECTORY ONLY COUNT                         
         B     XIT                                                              
*                                                                               
         SPACE 3                                                                
PUTREC   NTR1                                                                   
*                                                                               
         CLI   DMPSW,C'Y'                                                       
         BNE   PUTREC2                                                          
         MVI   DMPSW,C'N'                                                       
         SP    DMPCNT,=P'1'                                                     
         BNP   PUTREC2                                                          
         BAS   RE,DMPREC                                                        
PUTREC2  DS    0H                                                               
         AP    DIROUT,=P'1'                                                     
*                                                                               
         CLI   REC+3,X'3A'           DIRECTORY ONLY                             
         BL    PUTREC5               3A-3F                                      
         CLI   REC+3,X'3F'                                                      
         BNH   PUTRECX                                                          
         CLI   REC+3,X'E0'           DIRECTORY ONLY                             
         BL    PUTREC5               E0-E5                                      
         CLI   REC+3,X'E5'                                                      
         BNH   PUTRECX                                                          
*                                                                               
PUTREC5  DS    0H                                                               
         SP    DIROUT,=P'1'                                                     
         MVC   HALF,REC+25                                                      
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
PUTRECX  DS    0H                                                               
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
         B     XIT                                                              
         SPACE 3                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
*&&DO                                                                           
IN       DTFMT BLKSIZE=32760,RECFORM=VARBLK,TYPEFLE=INPUT,             X        
               IOAREA1=IN1,DEVADDR=SYS010,FILABL=NO,WORKA=YES,         X        
               EOFADDR=EOF                                                      
*&&                                                                             
*&&OS                                                                           
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*&&                                                                             
         EJECT                                                                  
*&&DO                                                                           
OUT      DTFMT BLKSIZE=32760,RECFORM=VARBLK,TYPEFLE=OUTPUT,            X        
               IOAREA1=OUT1,DEVADDR=SYS011,FILABL=NO,WORKA=YES                  
*&&                                                                             
*&&OS                                                                           
OUT      DCB   DDNAME=OUT,             DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=PM                                                         
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
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
*                                                                               
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
UPSI     DS    XL1                                                              
         DS    0F                                                               
WORK     DS    CL256                                                            
KLEN     EQU   25                                                               
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
EOFSW    DC    C'N'                                                             
DMPCNT   DC    PL5'100'                                                         
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
LKPARS   DS    6F                                                               
ESTPARS  DS    6F                                                               
TODAY    DS    CL8                                                              
PRTSW    DS    CL1                                                              
RTOTS    DS    11PL8               RUN TOTALS                                   
MTOTS    DS    11PL8               AGY MEDIA TOTALS                             
CTOTS    DS    11PL8               CLT TOTALS                                   
SAVAMCL  DS    CL6                                                              
         DS    0D                                                               
MYDUB    DS    PL8                                                              
*                                                                               
ELEM     DS    CL250                                                            
LASTAMC  DS    CL7       AGYMED/REC CODE/CLIENT                                 
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
DIRCNT   DC    PL5'0',CL20'DIRECTORY ONLY-IN'                                   
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
DIROUT   DC    PL5'0',CL20'DIRECTORY ONLY-OUT'                                  
BUYCNT   DC    PL5'0',CL20'BUYREC COUNT'                                        
OLDCNT   DC    PL5'0',CL20'OLD ELEMENTS'                                        
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    4000C                                                            
         DS    D                                                                
*&&DO                                                                           
IN1      DS    8500C                                                            
*&&                                                                             
*&&DO                                                                           
OUT1     DS    8500C                                                            
*&&                                                                             
         SPACE 3                                                                
         ORG   REC                                                              
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PBILLREC                                                       
*                                                                               
ETABD    DSECT                                                                  
ETAGY    DS    CL2                                                              
ETMED    DS    CL1                                                              
ETCLT    DS    CL3                                                              
ETPRD    DS    CL3                                                              
ETEST    DS    XL2                                                              
ETESTSW  DS    CL1                                                              
ETBUYS   DS    F                                                                
ETBILLS  DS    F                                                                
ETBUYGRS DS    F                                                                
ETBUYAC  DS    F                                                                
ETBUYCD  DS    F                                                                
ETPAYGRS DS    F                                                                
ETPAYAC  DS    F                                                                
ETPAYCD  DS    F                                                                
ETBILGRS DS    F                                                                
ETBILAC  DS    F                                                                
ETBILCD  DS    F                                                                
ETABEL   EQU   *-ETABD                                                          
*                                                                               
PSERELED DSECT                                                                  
       ++INCLUDE PSERELEM                                                       
*                                                                               
         CSECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052PSETSER   05/01/02'                                      
         END                                                                    
