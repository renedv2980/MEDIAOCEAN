*          DATA SET COPYPRTJW  AT LEVEL 015 AS OF 08/18/00                      
*PHASE COPYPRTD                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE GETINS                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
         TITLE 'COPYPRT- COPY PRINTPAK DATA'                                    
*                                                                               
*        THIS PROGRAM CAN BE USED TO COPY DATA FROM ONE AGY TO ANOTHER          
*        IT ALWAYS PRODUCES AN OUTPUT TAPE THAT ONLY HAS THE RECORDS            
*        BEING COPIED                                                           
*                                                                               
*        ENTER A CONTROL CARD FOR EACH CLIENT THAT IS TO BE COPIED              
*        IF YOU WISH TO COPY DATA SET UP OR AN OFFICE - ENTER                   
*        A CARD FOR THE OFFICE AS WELL.                                         
*                                                                               
*        NOTE: IF YOU WANT TO COPY DATA FOR ALL CLIENTS IN AN OFFICE            
*              YOU MUST ENTER A CARD FOR EACH CLIENT AND ONE FOR THE            
*              OFFICE.                                                          
*                                                                               
*        THE PROGRAM WILL COPY ONLY CLIENTS (OR OFFICES) ENTERED                
*        IN THE CONTROL CARDS                                                   
*                                                                               
*        THE FOLLOWING RECORDS ARE COPIED TO THE NEW AGENCY                     
*        THESE RECORDS ARE COPIED REGARDLESS OF THE CLIENT/OFFICE               
*                                                                               
*        X'11'   REPS                                                           
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
*        FOR THIS RUN THE OUTPUT AGENCY IS HARDCODED TO H7                      
*        AND ONLY DATA FOR 1999 AND SUBSEQUENT IS COPIED                        
*                                                                               
***********************************************************************         
*                                                                               
COPYPRT  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,COPYPRT,=V(REGSAVE)                                            
         SPACE 2                                                                
         LA    R6,COPYPRT+4095                                                  
         LA    R6,1(R6)                                                         
         USING COPYPRT+4096,R6     NOTE USE OF R6 AS BASE REGISTER              
*                                                                               
         USING ETABD,R8                                                         
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*                                                                               
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*                                                                               
* SET LOOKUP TABLE BINSRCH PARS                                                 
*                                                                               
         SR    R0,R0                                                            
         L     R1,=A(LKTAB)                                                     
         SR    R2,R2                                                            
         LA    R3,11                                                            
         LA    R4,11                                                            
         LA    R5,200                                                           
         STM   R0,R5,LKPARS                                                     
*                                                                               
* SET ESTTAB BINSRCH PARS                                                       
*                                                                               
         SR    R0,R0                                                            
         L     R1,=A(ESTTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,ETABEL                                                        
         LA    R4,11                                                            
         L     R5,=F'6000'                                                      
         STM   R0,R5,ESTPARS                                                    
*                                                                               
***********************************************************************         
*                                                                               
START1   DS    0H                                                               
         BAS   RE,CARDS                                                         
         CLC   =C'/*',CARD                                                      
         BE    START10                                                          
         CLC   =C'DUMP=',CARD                                                   
         BNE   START2                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
         B     START1                                                           
*                                                                               
START2   DS    0H                                                               
         CLC   =C'PRINT',CARD                                                   
         BNE   START3                                                           
         MVI   PRTSW,C'Y'                                                       
         B     START1                                                           
*                                                                               
START3   DS    0H                                                               
         XC    WORK(11),WORK                                                    
         MVC   WORK(6),CARD+2                                                   
         CLC   CARD+5(3),=C'ALL'                                                
         BNE   *+10                                                             
         MVC   WORK+3(3),=3X'FF'                                                
         MVC   WORK+6(3),CARD+11                                                
         CLC   CARD+11(3),=C'ALL'                                               
         BNE   *+10                                                             
         MVC   WORK+6(3),=3X'FF'                                                
         MVC   WORK+9(2),=3X'FF'                                                
         CLC   CARD+20(3),=C'ALL'                                               
         BE    START3B                                                          
         CLI   CARD+20,C' '                                                     
         BE    START3B                                                          
         PACK  DUB,CARD+20(3)                                                   
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   WORK+9(2),HALF                                                   
START3B  DS    0H                                                               
         MVC   P+1(80),CARD                                                     
         BAS   RE,PRNT                                                          
         GOTO1 =V(BINSRCH),LKPARS,(1,WORK)                                      
*                                                                               
         OC    LKPARS+1(3),LKPARS+1                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     START1                                                           
*                                                                               
***********************************************************************         
*                                                                               
START10  DS    0H                                                               
         XC    X,X                                                              
         OC    LKPARS+8(4),LKPARS+8                                             
         BNZ   START12                                                          
         MVC   P(18),=C'**NO INPUT CARDS**'                                     
         BAS   RE,PRNT                                                          
         B     EOJ                                                              
*                                                                               
START12  DS    0H                                                               
*                                                                               
GET      DS    0H                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         CLI   REC,C'Z'            SPECIAL DDS RECORD                           
         BE    EOF                 PAST ALL REAL DATA                           
*                                                                               
         CP    INCNT,=P'1'                                                      
         BH    GET20                                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
         MVC   P+1(11),=C'FILE HEADER'                                          
         BAS   RE,PRNT                                                          
         BAS   RE,DMPREC                                                        
         BAS   RE,PRNT             SKIP A LINE                                  
*                                                                               
* ADD CHECK FOR AGENCY BEING COPIED HERE                                        
*                                                                               
GET20    CLC   REC(2),=C'JW'                                                    
         BH    EOF                                                              
         BE    GET30                                                            
         AP    NJWCNT,=P'1'                                                     
         B     GET                                                              
GET30    AP    JWCNT,=P'1'                                                      
*                                                                               
* ONLY INTERESTED IN REP RECORDS (X'11')                                        
*                                                                               
         CLI   REC+3,X'11'         REPS                                         
         BNE   GET                 SKIP OTHER RECORD TYPES                      
*                                                                               
* MEDIA I,M,N,O,T,S                                                             
*                                                                               
         CLI   REC+2,C'I'                                                       
         BNE   GET63A                                                           
         AP    MEDICNT,=P'1'                                                    
         B     REP                                                              
GET63A   CLI   REC+2,C'M'                                                       
         BE    GET63B                                                           
         AP    MEDMCNT,=P'1'                                                    
         B     REP                                                              
GET63B   CLI   REC+2,C'N'                                                       
         BE    GET63C                                                           
         AP    MEDNCNT,=P'1'                                                    
         B     REP                                                              
GET63C   CLI   REC+2,C'O'                                                       
         BE    GET63D                                                           
         AP    MEDOCNT,=P'1'                                                    
         B     REP                                                              
GET63D   CLI   REC+2,C'T'                                                       
         BNE   GET63E                                                           
         AP    MEDTCNT,=P'1'                                                    
         B     REP                                                              
GET63E   CLI   REC+2,C'S'                                                       
         BNE   GET63U                                                           
         AP    MEDSCNT,=P'1'                                                    
         B     REP                                                              
*                                                                               
GET63U   DS    0H                                                               
         AP    OTHMCNT,=P'1'       MEDIA OTHER THAN I,M,N,O,T,S                 
         AP    NREPCNT,=P'1'       REPS NOT RPOCESSED                           
*                                                                               
GETXX    B     GET                 SKIP OTHER RECORD TYPES                      
*                                                                               
***********************************************************************         
*                                                                               
REP      DS    0H                  OUTPUT REPS                                  
         AP    REPCNT,=P'1'                                                     
*                                                                               
AGSW     DS    0H                                                               
         MVC   REC(2),=C'H7'                                                    
*                                                                               
         CLI   PRTSW,C'Y'                                                       
         BNE   PUT                                                              
         MVI   DMPSW,C'Y'                                                       
         B     PUT                                                              
*                                                                               
***********************************************************************         
*                                                                               
PUT      DS    0H                                                               
         MVI   BYTE,X'80'                                                       
         TM    REC+27,X'20'        TEST DELETED                                 
         BNZ   *+8                                                              
         MVI   BYTE,0                                                           
         MVC   REC+27(1),BYTE                                                   
PUTXX    BAS   RE,PUTREC                                                        
         B     GET                                                              
*                                                                               
***********************************************************************         
*                                                                               
EOF      CLOSE (IN,)                                                            
         CLOSE (OUT,)                                                           
*                                                                               
         SP    INCNT,=P'1'         OFFEST INPUT CNT (EXCL HDR FILE)             
*                                                                               
         BAS   RE,PRNT                                                          
         LA    R3,COUNTS                                                        
         LA    R4,35               LENGTH OF ONE COUNT ENTRY                    
         LA    R5,COUNTSX                                                       
*                                                                               
EOF2     MVC   P+1(30),5(R3)                                                    
         OI    4(R3),X'0F'                                                      
         UNPK  P+32(7),0(5,R3)                                                  
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF2                                                       
*                                                                               
EOJ      DS    0H                                                               
*                                                                               
         XBASE                                                                  
*                                                                               
***********************************************************************         
*                                                                               
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
*                                                                               
***********************************************************************         
*                                                                               
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         LA    R2,4(R2)                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
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
*                                                                               
***********************************************************************         
*                                                                               
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
*                                                                               
***********************************************************************         
*                                                                               
GETREC   NTR1                                                                   
         GET   IN,REC-4                                                         
*                                                                               
         CLC   REC+25(2),=X'80FF'   SEE IF DIRECTORY ONLY (DELETED)             
         BE    GETRDO                                                           
         CLC   REC+25(2),=X'00FF'   SEE IF DIRECTORY ONLY                       
         BE    GETRDO                                                           
*                                                                               
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0             EOR                                          
         B     GETRX                                                            
*                                                                               
GETRDO   DS    0H               FOR DIRECTORY ONLY RECS                         
*                               NO NEED FOR END OF REC ZERO                     
GETRX    AP    INCNT,=P'1'                                                      
*                                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
PUTREC   NTR1                                                                   
*                                                                               
         CLI   DMPSW,C'Y'                                                       
         BNE   PUTREC2                                                          
         MVI   DMPSW,C'N'                                                       
         SP    DMPCNT,=P'1'                                                     
         CP    DMPCNT,=P'0'                                                     
         BL    PUTREC2                                                          
         MVI   P,C'-'                                                           
         MVC   P+1(132),P                                                       
         BAS   RE,PRNT                                                          
         BAS   RE,PRNT             SKIP A LINE                                  
         AP    RPRTCNT,=P'1'                                                    
         MVC   P+1(17),=C'PRINTING RECORD #'                                    
         EDIT  (P5,RPRTCNT),(8,P+18),ALIGN=LEFT                                 
         BAS   RE,PRNT                                                          
         BAS   RE,DMPREC                                                        
         BAS   RE,PRNT             SKIP A LINE                                  
PUTREC2  DS    0H                                                               
         MVC   HALF,REC+25                                                      
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
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
*                                                                               
***********************************************************************         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
*                                                                               
*                                                                               
OUT      DCB   DDNAME=OUT,             DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=PM                                                         
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
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
DMPCNT   DC    PL5'100'                                                         
RPRTCNT  DC    PL5'0'              RECORDS PRINTED                              
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
SAVAMED  DS    CL3                                                              
         DS    0D                                                               
MYDUB    DS    PL8                                                              
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL30'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL30'OUTPUT COUNT'                                        
REPCNT   DC    PL5'0',CL30'REPS PROCESSED'                                      
NREPCNT  DC    PL5'0',CL30'REPS NOT PROCSSED'                                   
MEDICNT  DC    PL5'0',CL30'MEDIA I PROCESSED'                                   
MEDMCNT  DC    PL5'0',CL30'MEDIA M PROCESSED'                                   
MEDNCNT  DC    PL5'0',CL30'MEDIA N PROCESSED'                                   
MEDOCNT  DC    PL5'0',CL30'MEDIA O PROCESSED'                                   
MEDSCNT  DC    PL5'0',CL30'MEDIA S PROCESSED'                                   
MEDTCNT  DC    PL5'0',CL30'MEDIA T PROCESSED'                                   
OTHMCNT  DC    PL5'0',CL30'OTHER MED (NOT PROCESSED)'                           
JWCNT    DC    PL5'0',CL30'AGENCY JW'                                           
NJWCNT   DC    PL5'0',CL30'NOT AGENCY JW'                                       
*                                                                               
* OTHER COUNTERS ADDED HERE WILL BE PRINTED AT EOJ                              
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    4000C                                                            
         DS    D                                                                
*                                                                               
*                                                                               
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PBILLREC                                                       
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PCONREC                                                        
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PESTREC                                                        
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
*                                                                               
*                                                                               
ESTTAB   CSECT                                                                  
         DS    6000CL56            ROOM FOR 6000 ESTS                           
         DC    X'0000'                                                          
*                                                                               
*                                                                               
*                                                                               
LKTAB    CSECT                                                                  
         DS    200CL11                                                          
         DC    X'0000'                                                          
*                                                                               
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015COPYPRTJW 08/18/00'                                      
         END                                                                    
