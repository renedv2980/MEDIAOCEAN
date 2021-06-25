*          DATA SET PPREPYR02  AT LEVEL 110 AS OF 01/22/01                      
*PHASE PPYR02A                                                                  
*INCLUDE MININAM                                                                
*INCLUDE CASHVAL                                                                
*INCLUDE BINSRCH2                                                               
         TITLE 'TEST - SPECIAL PUB FILE CREATION'                               
*                                                                               
*        OUTPUT IS A TAPE TO BE ADDED TO THE LOAD                               
*                                                                               
*        QOPT1 N= TEST RUN - DON'T MARK FILE                                    
*        QOPT2 Y= DUMP RECORD                                                   
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*                                                                               
PPYR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPYR02,RR=R9                                                   
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPYRWRKD,R8                                                      
*                                                                               
**                                                                              
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   PUBCNT,=P'0'                                                     
         ZAP   MREPCNT,=P'0'                                                    
*                                                                               
         L     RF,=V(CASHVAL)                                                   
         A     RF,RELO                                                          
         ST    RF,ACASHVAL                                                      
*                                                                               
         LA    RE,REPTAB           CLEAR REP TABLE                              
         LH    RF,=H'4000'                                                      
         XCEF                                                                   
         LA    RE,REPTAB                                                        
         ST    RE,BINATAB                                                       
*                                                                               
         OPEN  (OUT,(OUTPUT))                                                   
*                                                                               
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT1,C'N'          TEST RUN ?                                   
         BNE   *+8                 NO                                           
         MVI   RCWRITE,C'N'        MEANS DON'T MARK FILE                        
         MVI   FORCEHED,C'Y'                                                    
         MVI   DMINBTS,X'08'   ??   SET TO PASS DELETES                         
         MVI   DMOUTBTS,X'FD'  ??   SET TO PASS DELETES                         
*                                                                               
         OPEN  (IN,(INPUT))                                                     
GET      DS    0H                                                               
         BAS   RE,TAPEGET                                                       
         LA    R7,REC                                                           
         USING TSTRECD,R7                                                       
*                                                                               
         CLI   TSTMEDIA+1,C' '    CHECK FOR SPACE AFTER MEDIA                   
         BNE   GET                                                              
         CLI   TSTMEDIA,C'N'      CHECK FOR MEDIA N AND S                       
         BE    VALMED                                                           
         CLI   TSTMEDIA,C'S'                                                    
         BE    VALMED                                                           
         B     GET                                                              
*                                                                               
VALMED   CLI   EOFSW,C'Y'                                                       
         BE    EXIT                                                             
         AP    INCNT,=P'1'                                                      
********                           BUILD PUB RECORD KEY                         
PUBKEY1  LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
********                           CLEAR PUB RECORD AREA                        
*                                                                               
         LA    RE,PUBREC                                                        
         LH    RF,=H'1999'                                                      
         XCEF                                                                   
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
*                                                                               
         MVC   KEY(1),TSTMEDIA        MEDIA                                     
*                                                                               
*********                          SET PUB NUMBER                               
         XC    NEWNUM,NEWNUM                                                    
         XC    WORK(20),WORK                                                    
         MVC   WORK(8),TSTPUB#                                                  
         LHI   R6,8                                                             
         CLC   TSTPUB#+6(2),=C'00' CHECK FOR 8 DIGIT NUMBER                     
         BH    YR04                                                             
         MVC   WORK(2),=C'00'      ACBO ONLY HAD 6 DIGITS                       
         MVC   WORK+2(6),TSTPUB#                                                
         LA    R6,8                INPUT FIELD LENGTH                           
         CLI   TSTPUB#+5,C'0'      OR SOMESTIMES 5 DIGITS                       
         BNL   YR04                                                             
         MVC   WORK(3),=C'000'                                                  
         MVC   WORK+3(5),TSTPUB#                                                
         CLI   TSTPUB#+4,C'0'                                                   
         BH    *+6                                                              
         DC    H'0'          DUMP IF LESS THAN 5 DIGITS GIVEN                   
*                                                                               
*                                                                               
YR04     GOTO1 PUBVAL,DMCB,((R6),WORK),(0,NEWNUM)                               
         MVC   KEY+1(6),NEWNUM                                                  
*                                                                               
         MVC   KEY+7(2),QAGENCY    AGENCY                                       
         MVI   KEY+9,X'81'         RECORD CODE                                  
*                                                                               
*                                                                               
         MVC   PUBREC(25),KEY                                                   
         MVI   PUBREC+26,33        RECORD LENGTH                                
*                                                                               
*                                  BUILD NAME + ADDR ELEM                       
*                                                                               
         LA    R2,PUBREC+33        FIRST ELEM                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING PUBNAMEL,R5                                                      
*                                                                               
         MVI   PUBNAMEL,X'10'      ELEM CODE                                    
         MVI   PUBNAMEL+1,196      ELEM LEN                                     
*                                                                               
         MVC   PUBNAME,TSTPUBNM    PUBLICATION NAME                             
         OC    PUBNAME,SPACES                                                   
         CLC   TSTPUBNM+L'PUBNAME(L'TSTPUBNM-L'PUBNAME),SPACES                  
         BNH   *+8                                                              
         MVI   TRNKSW,C'Y'                                                      
*                                                                               
*        MVC   PUBZNAME,TSTZONE                                                 
*        OC    PUBZNAME,SPACES                                                  
*        CLC   TSTZONE+L'PUBZNAME(L'TSTZONE-L'PUBZNAME),SPACES                  
*        BNH   *+8                                                              
*        MVI   TRNKSW,C'Y'                                                      
*                                                                               
         MVC   PUBLINE1,TSTADDR1   ADDRESS LINE 1                               
         OC    PUBLINE1,SPACES                                                  
         CLC   TSTADDR1+L'PUBLINE1(L'TSTADDR1-L'PUBLINE1),SPACES                
         BNH   *+8                                                              
         MVI   TRNKSW,C'Y'                                                      
*                                                                               
         CLI   TSTADDR2,C' '                                                    
         BE    ADRS00                                                           
         MVC   PUBLINE2,TSTADDR2                                                
         OC    PUBLINE2,SPACES                                                  
         CLC   TSTADDR2+L'PUBLINE2(L'TSTADDR2-L'PUBLINE2),SPACES                
         BNH   *+8                                                              
         MVI   TRNKSW,C'Y'                                                      
         B     CITY                                                             
*                                                                               
ADRS00   MVC   PUBLINE2(16),TSTCITY  ADDRESS LINE 2                             
         LA    R3,PUBLINE2+15                                                   
ADRS10   CLI   0(R3),C' '                                                       
         BNE   ADRS20                                                           
         BCTR  R3,0                                                             
         B     ADRS10                                                           
*                                                                               
ADRS20   MVI   1(R3),C','                                                       
         MVC   2(2,R3),TSTSTATE                                                 
         MVI   4(R3),C' '                                                       
         CLI   TSTZIP,C' '                                                      
         BE    *+10                                                             
         MVC   5(10,R3),TSTZIP                                                  
         OC    PUBLINE2,SPACES                                                  
*                                                                               
CITY     MVC   PUBCITY,TSTCITY      CITY                                        
         OC    PUBCITY,SPACES                                                   
         CLC   TSTCITY+L'PUBCITY(L'TSTCITY-L'PUBCITY),SPACES                    
         BNH   *+8                                                              
         MVI   TRNKSW,C'Y'                                                      
*                                                                               
         CLC   TSTSTATE,SPACES                                                  
         BNH   CITY02                                                           
         MVC   PUBSTATE,TSTSTATE    STATE                                       
         OC    PUBSTATE,SPACES                                                  
*        CLC   TSTSTATE+L'PUBSTATE(L'TSTSTATE-L'PUBSTATE),SPACES                
*        BNH   *+8                                                              
*        MVI   TRNKSW,C'Y'                                                      
*                                                                               
CITY02   CLI   TSTZIP,C' '                                                      
         BE    *+10                                                             
         MVC   PUBNWZIP,TSTZIP                                                  
*        CLC   TSTZIP+L'PUBNWZIP(L'TSTZIP-L'PUBNWZIP),SPACES                    
*        BNH   *+8                                                              
*        MVI   TRNKSW,C'Y'                                                      
*                                                                               
         MVC   0(PUBNMELE,R2),ELEM                                              
         MVI   PUBREC+26,(33+PUBNMELE)     UPDATE REC LEN                       
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R2,PUBNMELE(R2)     PT R2 TO END OF RECORD                       
*                                                                               
         CLI   TSTATTN,C' '         ALL FIELDS ARE OPTIONAL                     
         BNE   SUP                                                              
         CLI   TSTPHONE,C' '                                                    
         BNE   SUP                                                              
         CLI   TSTFAX,C' '                                                      
         BE    CHECKREC            IF NONE, NO ELEMENT                          
*                                                                               
SUP      XC    ELEM,ELEM           PUB SUPPL. ADDR ELEM                         
         LA    R5,ELEM                                                          
         USING PUBSADEL,R5                                                      
*                                                                               
         MVI   PUBSADEL,X'11'      ELEM CODE                                    
         MVI   PUBSADEL+1,62       NEW ELEM LEN                                 
*                                                                               
         MVC   PUBSAOFF,=3X'FF'                                                 
         CLI   TSTATTN,C' '                                                     
         BE    PHONE                                                            
         MVC   PUBATTN,TSTATTN                                                  
         OC    PUBATTN,SPACES                                                   
*        CLC   TSTATTN+L'PUBATTN(L'TSTATTN-L'PUBATTN),SPACES                    
*        BNH   *+8                                                              
*        MVI   TRNKSW,C'Y'                                                      
PHONE    CLI   TSTPHONE,C' '                                                    
         BE    FAX                                                              
         MVC   PUBTEL,TSTPHONE                                                  
*        CLC   TSTPHONE+L'PUBTEL(L'TSTPHONE-L'PUBTEL),SPACES                    
*        BNH   *+8                                                              
*        MVI   TRNKSW,C'Y'                                                      
FAX      CLI   TSTFAX,C' '                                                      
         BE    CREATE                                                           
         MVC   PUBSFAXN,TSTFAX                                                  
*        CLC   TSTFAX+L'PUBSFAXN(L'TSTFAX-L'PUBSFAXN),SPACES                    
*        BNH   *+8                                                              
*        MVI   TRNKSW,C'Y'                                                      
*                                                                               
CREATE   BAS   RE,PUBUP                                                         
*                                                                               
*        DROP  R5                                                               
*                                                                               
************************                                                        
************************                                                        
CHECKREC OC    PUBREC+1(4),PUBREC+1     PUB NUMBER THERE ?                      
         BNZ   PUBNX                    YES                                     
         B     ADDPUB5B                 NO - DO NOT WRITE TO FILE               
*                                                                               
PUBNX    AP    PUBCNT,=P'1'                                                     
*                                                                               
ADDPUB1  DS    0H                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    ADDPUB5                                                          
         MVC   HALF,PUBREC+25                                                   
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,PUBREC-4                                                      
         PUT   OUT,PUBREC-4                                                     
                                                                                
*****    GOTO1 ADDPUB                                                           
ADDPUB5  DS    0H                                                               
         CLI   QOPT2,C'Y'          DUMP RECORDS ?                               
         BNE   ADDPUB6             NO                                           
         CLI   RCWRITE,C'N'                                                     
         BNE   ADDPUB6                                                          
         CP    PUBCNT,=P'25'       DUMP FIRST 25 PUBS                           
         BH    ADDPUB6                                                          
ADDPUB5B MVI   FORCEHED,C'Y'       EACH ON A NEW PAGE                           
         BAS   RE,DMPREC                                                        
*                                                                               
ADDPUB6  DS    0H                  NOW ALWAYS PRINT NAMES/ADDRESSES             
         CLI   LINE,50             SEE IF I CAN PRINT ALL LINES                 
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+2(L'PUBKMED),PUBKMED                                           
         MVC   P+21(L'PUBNAME),PUBNAME                                          
         MVC   P+43(L'PUBLINE1),PUBLINE1                                        
         MVC   P+75(L'PUBLINE2),PUBLINE2                                        
         MVC   P+107(L'PUBCITY),PUBCITY                                         
*                                                                               
         DROP  R5                                                               
*                                                                               
         CLI   TRNKSW,C'Y'                SEE IF TRUNCATED                      
         BNE   *+8                                                              
         MVI   P+4,C'Y'                                                         
*                                                                               
         OC    PUBREC+1(4),PUBREC+1     PUB NUMBER THERE ?                      
         BNZ   PUBN3                    YES                                     
         MVC   P+7(25),=C'*** ERROR ERROR ERROR ***'                            
         MVC   P+55(24),=C'** PUB NUMBER MISSING **'                            
         B     PUBN4                                                            
*                                                                               
PUBN3    IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PUBREC+1),P+6                                 
PUBN4    BAS   RE,RPRT                                                          
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    PUBN5                                                            
************************                                                        
*                                                                               
PUBN5    LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
*                                                                               
ADDPUBX  DS    0H                                                               
         MVI   TRNKSW,C'N'                                                      
         B     GET                                                              
*                                                                               
         SPACE 2                                                                
*                                                                               
PUBUP    DS    0H                                                               
         ST    RE,SAVERE                                                        
         GOTO1 RECUP,DMCB,(1,PUBREC),ELEM,0(R2),0                               
*                                                                               
         LA    R2,PUBREC                                                        
         ZICM  R1,PUBREC+25,2      RECORD LEN                                   
         AR    R2,R1               BUMP R2 TO END OF RECORD                     
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
TAPEGET  NTR1                                                                   
         GET   IN,REC                                                           
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DC    PL8'0'                                                           
         DC    CL20'TAPE RECS READ'                                             
PUBCNT   DC    PL8'0'                                                           
         DC    CL20'TOTAL PUBS ADDED'                                           
MREPCNT  DC    PL8'0'                                                           
         DC    CL20'REPS MISSING'                                               
         DC    X'FF'                                                            
*                                                                               
EOFSW    DC    C'N'                                                             
*                                                                               
NEWNUM   DS    XL6                                                              
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         L     R0,BINCOUNT                                                      
         CVD   R0,DUB                                                           
         ZAP   MREPCNT,DUB                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   WORK(4),=4X'FF'      END OF TABLE                                
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                          PRINT STD COMMENT CODES FOUND                        
         XC    WORK(4),WORK                                                     
         GOTO1 =V(BINSRCH),BINPARMS,(X'02',WORK)                                
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'                MUST BE FOUND                                
         ZICM  R3,1(R1),3          POINT R3 TO TABLE - A(FOUND RECORD)          
*                                                                               
*                                                                               
RUNL10   CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    RUNL40              YES - FINISH UP                              
*                                                                               
         MVC   P+19(4),0(R3)       REP CODES                                    
         MVI   RCSUBPRG,20                                                      
         BAS   RE,RPRT                                                          
         LA    R3,4(R3)                                                         
         B     RUNL10                                                           
*                                                                               
RUNL40   MVI   RCSUBPRG,20                                                      
         BAS   RE,RPRT                                                          
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(20),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         MVI   RCSUBPRG,20                                                      
         BAS   RE,RPRT                                                          
         LA    R4,28(R4)                                                        
         B     RUNL50                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
REPCHECK NTR1                 SUBRTINE TO VALIDATE REPS                         
         LA    R3,KEY2                                                          
         USING PREPREC,R3                                                       
         XC    KEY2,KEY2                                                        
         MVC   PREPKAGY,QAGENCY    AGENCY                                       
         MVC   PREPKMED,QMEDIA     MEDIA                                        
         MVI   PREPKRCD,X'11'                                                   
         MVC   PREPKREP,REPS                                                    
         GOTO1 DATAMGR,DMCB,(0,DMREAD),PRTDIR,KEY2,IO                           
         TM    8(R1),X'10'                                                      
         BZ    REPCKX                                                           
         MVC   P(4),REPS                                                        
         MVC   P+5(34),=C'ERROR-THIS REP NEEDS TO BE ENTERED'                   
         BAS   RE,RPRT                                                          
*                                                                               
*        ADD TO REPTAB IF NOT FOUND                                             
*                                                                               
         MVC   WORK(4),REPS                                                     
REPCK    GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'      TABLE OVERFLOW                                         
*                                                                               
REPCKX   XIT1                                                                   
*                                                                               
EOF      CLOSE (IN,,OUT,)                                                       
         MVI   EOFSW,C'Y'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         MVI   RCSUBPRG,10                                                      
         CLI   MODE,RUNLAST                                                     
         BNE   *+8                                                              
         MVI   RCSUBPRG,20                                                      
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
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
         L     R5,AREC                                                          
         MVC   HALF,25(R5)                                                      
         LH    R2,HALF              USE RECORD LENGTH                           
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
         SPACE 3                                                                
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
ACASHVAL DS    F                                                                
*                                                                               
BINPARMS DC    A(0)                                                             
BINATAB  DC    A(0)                                                             
BINCOUNT DC    A(0)                RECORD COUNT                                 
         DC    A(4)                LENGTH                                       
         DC    AL1(0),AL3(4)                                                    
BINMAX   DC    A(1000)             MAX NUMBER OF RECORDS                        
*                                                                               
IN       DCB   DDNAME=TAPEIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00420,                                            X        
               BLKSIZE=04200,                                          X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
OUT      DCB   DDNAME=TAPEOUT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
*                                                                               
         DS    F                                                                
REC      DS    200C                                                             
*                                                                               
REPTAB   DS    4000X                                                            
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
PPYRWRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    CL200                                                            
FRSTSW   DS    XL1                                                              
TRNKSW   DS    CL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ITOT     DS    F                                                                
SAVERE   DS    F                                                                
WPUBNAME DS    CL35                                                             
DPUBNAME DS    CL35                                                             
WADDR1   DS    CL45                                                             
DADDR1   DS    CL45                                                             
WADDR2   DS    CL45                                                             
DADDR2   DS    CL45                                                             
WCITY    DS    CL30                                                             
DCITY    DS    CL30                                                             
FIELD    DS    XL3                 FOR PACKING COMMISSION                       
KEY2     DS    CL25                                                             
IO       DS    1000X                                                            
REPS     DS    CL4                                                              
*                                                                               
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDBUFFALOD                                                     
         EJECT                                                                  
*                                                                               
TSTRECD  DSECT                                                                  
*                                                                               
TSTMEDIA DS    C                                                                
         DS    C                                                                
TSTPUB#  DS    CL8                                                              
         DS    C                                                                
TSTPUBNM DS    CL30                                                             
         DS    C                                                                
TSTADDR1 DS    CL31                                                             
         DS    C                                                                
TSTADDR2 DS    CL31                                                             
         DS    C                                                                
TSTCITY  DS    CL20                                                             
         DS    C                                                                
TSTSTATE DS    CL2                                                              
         DS    C                                                                
TSTZIP   DS    CL10                                                             
         DS    C                                                                
TSTATTN  DS    CL24                                                             
         DS    C                                                                
TSTPHONE DS    CL12                                                             
         DS    C                                                                
TSTFAX   DS    CL12                                                             
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110PPREPYR02 01/22/01'                                      
         END                                                                    
