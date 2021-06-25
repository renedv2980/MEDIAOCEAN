*          DATA SET PPREPWW02  AT LEVEL 042 AS OF 05/01/02                      
*PHASE PPWW02A,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PPWW02S - SPECIAL PUB FILE CREATION FOR WESTERN'                
*                                                                               
*                                                                               
*        QOPT1 N= TEST RUN - DON'T MARK FILE                                    
*        QOPT2 Y= DUMP RECORD                                                   
*                                                                               
PPWW02   CSECT                                                                  
***      PRINT NOGEN                                                            
         NMOD1 0,PPWW02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         L     R6,ALTLREC                                                       
         USING LTLREC,R6                                                        
         USING PPWWWRKD,R8                                                      
*                                                                               
         LA    R7,REC                                                           
         USING PPWWREC,R7                                                       
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   NEWSCNT,=P'0'                                                    
         ZAP   MAGSCNT,=P'0'                                                    
         ZAP   PUBCNT,=P'0'                                                     
         ZAP   FSICNT,=P'0'                                                     
         ZAP   LTLCNT,=P'0'                                                     
*                                                                               
*                                 SET STARTING PUB NUMBERS                      
*                                                                               
         ZAP   FSINUM,=P'10'      REALLY 1                                      
         ZAP   NEWNUM,=P'1000'    REALLY 100                                    
         ZAP   MAGNUM,=P'50000'   REALLY 5000                                   
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
*                                                                               
         CLI   QOPT1,C'N'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   DMINBTS,X'08'        SET TO PASS DELETES                         
         MVI   DMOUTBTS,X'FD'       SET TO PASS DELETES                         
*                                                                               
         OPEN  (IN,(INPUT))                                                     
GET      DS    0H                                                               
         BAS   RE,TAPEGET                                                       
         CLI   EOFSW,C'Y'                                                       
         BE    EXIT                                                             
         AP    INCNT,=P'1'                                                      
*                                  SEE CONVERSION STATUS                        
         CLI   PWJGPKSF,C'I'                                                    
         BE    CKMED                                                            
         CLI   PWJGPKSF,C'M'                                                    
         BE    CKMED                                                            
         CLI   PWJGPKSF,C'V'                                                    
         BE    CKMED                                                            
         CLI   PWJGPKSF,C'R'                                                    
         BNE   GET                                                              
*                                                                               
CKMED    CLI   PWJGJ6ST,C'M'     ONLY PROCESS MAGAZINES AND NEWSPAPERS          
         BE    PUBKEY1                                                          
         CLI   PWJGJ6ST,C'N'                                                    
         BNE   GET                                                              
*                                                                               
*                                  BUILD PUB RECORD KEY                         
*                                                                               
PUBKEY1  LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
*                                  CLEAR PUB RECORD AREA                        
         LA    RE,PUBREC                                                        
         LA    RF,1999                                                          
         XCEF                                                                   
*                                  CLEAR LTL RECORD AREA                        
         LA    RE,LTLREC                                                        
         LA    RF,1999                                                          
         XCEF                                                                   
*                                                                               
         XC    WPUBNAME,WPUBNAME                                                
         XC    WADDR1,WADDR1                                                    
         XC    WADDR2,WADDR2                                                    
         XC    WCITY,WCITY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),PWJGJ6ST     MEDIA                                        
*                                                                               
*                                  SET PUB NUMBER                               
         LA    RE,MAGNUM                                                        
         CLI   PWJGJ6ST,C'M'       SEE IF MAGAZINES                             
         BE    PUBKEY2                                                          
         LA    RE,NEWNUM                                                        
         CLI   PWJGBBST,C'S'       SEE IF FSI                                   
         BNE   *+8                                                              
         LA    RE,FSINUM                                                        
*                                                                               
PUBKEY2  MVC   KEY+1(4),0(RE)      WILL MOVE PACKED WITHOUT SIGN                
         AP    0(5,RE),=P'10'      REALLY ONLY BUMPS ONE NUMBER                 
*                                                                               
         MVC   KEY+7(2),=C'WI'     AGENCY                                       
         MVI   KEY+9,X'81'         RECORD CODE                                  
         MVC   PUBREC(25),KEY                                                   
         MVI   PUBREC+26,33                                                     
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
         MVC   PUBNAME,PWJGBBTX    PUBLICATION NAME                             
         OC    PUBNAME,SPACES                                                   
         MVC   WPUBNAME,PWJGBBTX                                                
         MVC   DPUBNAME,PUBNAME                                                 
         CLC   PWJGBBTX+20(15),SPACES SEE IF PUB NAME WAS TRUNCATED             
         BNH   *+8                                                              
         MVI   TRNKSW,C'Y'                                                      
*                                                                               
         MVC   PUBLINE1,PWNEMAD1   ADDRESS LINE 1                               
         OC    PUBLINE1,SPACES                                                  
         MVC   WADDR1,PWNEMAD1                                                  
         MVC   DADDR1,PUBLINE1                                                  
         CLC   PWNEMAD1+30(15),SPACES                                           
         BNH   *+8                                                              
         MVI   TRNKSW,C'Y'                                                      
*                                                                               
         MVC   PUBLINE2,PWNEMAD2   ADDRESS LINE 2                               
         OC    PUBLINE2,SPACES                                                  
         MVC   WADDR2,PWNEMAD2                                                  
         MVC   DADDR2,PUBLINE2                                                  
         CLC   PWNEMAD2+30(15),SPACES                                           
         BNH   *+8                                                              
         MVI   TRNKSW,C'Y'                                                      
*                                                                               
CITY     MVC   PUBCITY,PWNEMCTY    CITY                                         
         OC    PUBCITY,SPACES                                                   
         MVC   WCITY,PWNEMCTY                                                   
         MVC   DCITY,PUBCITY                                                    
         CLC   PWNEMCTY+16(14),SPACES                                           
         BNH   *+8                                                              
         MVI   TRNKSW,C'Y'                                                      
*                                                                               
         CLC   PWNEMST,SPACES                                                   
         BNH   *+10                                                             
         MVC   PUBSTATE,PWNEMST    STATE                                        
*                                                                               
         CP    PWNEMZP1,=P'0'                                                   
         BE    PUBE1                                                            
*                                                                               
         UNPK  PUBNWZIP(5),PWNEMZP1(3) FIRST 5 DIGITS OF ZIP                    
         OI    PUBNWZIP+4,X'F0'                                                 
*                                                                               
         CP    PWNEMZP2,=P'0'                                                   
         BE    PUBE1                                                            
*                                                                               
         UNPK  PUBNWZIP+6(4),PWNEMZP2(3) LAST 4 DIGITS OF ZIP                   
         OI    PUBNWZIP+9,X'F0'                                                 
         MVI   PUBNWZIP+5,C'-'                                                  
*                                  ADD ELEM TO THE RECORD                       
         DROP  R5                                                               
*                                                                               
PUBE1    MVC   PUBREC+33(PUBNMELE),ELEM                                         
         MVI   PUBREC+26,(33+PUBNMELE) UPDATE REC LEN                           
*                                                                               
*                                  GENERAL INFO ELEMENT                         
*                                                                               
         LA    R2,PUBNMELE(R2)     END OF RECORD                                
*                                                                               
         XC    ELEM,ELEM                                                        
         USING PUBGENEL,R5                                                      
*                                                                               
         MVI   PUBGENEL,X'20'      ELEM CODE                                    
         MVI   PUBGENEL+1,50       ELEM LEN                                     
*                                                                               
*                              INITIALIZE PACKED FIELDS                         
*                              FIRST DO FIELDS COMMOM TO BOTH MEDIAS            
         ZAP   PUBCD,=P'0'                                                      
         ZAP   PUBCDDAS,=P'0'                                                   
         ZAP   PUBCLMO,=P'0'                                                    
         ZAP   PUBCLDA,=P'0'                                                    
         ZAP   PUBMCLMO,=P'0'                                                   
         ZAP   PUBMCLDA,=P'0'                                                   
*                                                                               
         CLI   PWJGJ6ST,C'N'       SEE IF NEWSPAPER                             
         BNE   PUBE1A                                                           
         ZAP   PUBCPP,=P'0'                                                     
         ZAP   PUBLPC,=P'0'                                                     
         ZAP   PUBLDT,=P'0'                                                     
         ZAP   PUBFD,=P'0'                                                      
         B     PUBE1B                                                           
*                                                                               
PUBE1A   ZAP   PUBOSMO,=P'0'      DO MAGAZINE ONLY FIELDS                       
         ZAP   PUBOSDA,=P'0'                                                    
         ZAP   PUBPAYMO,=P'0'                                                   
         ZAP   PUBPAYDA,=P'0'                                                   
*                                                                               
PUBE1B   DS    0H                                                               
         ZAP   PUBAC,PWNECOMI      COMMISSION                                   
*                                                                               
         CP    PUBAC,=P'1500'      PWNECOMI WAS 2 DECIMALS                      
         BNE   *+10                AND PUBAC HAS 3                              
         ZAP   PUBAC,=P'15000'     THIS IS OK SINCE PWNECOMI WAS                
*                                  EITHER 0 OR 15.00                            
*                                                                               
         CLI   PWJGJ6ST,C'M'       IS MEDIA =M?                                 
         BNE   PUBE1C                                                           
*                                                                               
         AP    MAGSCNT,=P'1'                                                    
         AP    PUBCNT,=P'1'                                                     
         MVC   PUBMFREQ,PWNENTYP   FREQUENCY (MAGS ONLY)                        
*                                                                               
PUBE1C   DS    0H                                                               
         BAS   RE,PUBUP                                                         
*                                                                               
         CLI   PWJGJ6ST,C'M'       IS MEDIA =M?                                 
         BE    PUBE3                                                            
*                                                                               
PUBNX    AP    NEWSCNT,=P'1'                                                    
         CLI   PWJGBBST,C'S'      SEE IF FSI NEWSPAPER                          
         BNE   *+10                                                             
         AP    FSICNT,=P'1'                                                     
         AP    PUBCNT,=P'1'                                                     
*                                  PUB TAX ELEM                                 
*                                                                               
PUBE3    CP    PWNETAX,=P'0'                                                    
         BE    ADDPUB1                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         USING PUBTAXEL,R5                                                      
*                                                                               
         MVI   PUBTAXEL,X'22'      ELEM CODE                                    
         MVI   PUBTAXEL+1,30       ELEM LEN                                     
*                                                                               
         XC    DUB,DUB                                                          
         ZAP   DUB,PWNETAX                                                      
         CVB   R1,DUB                                                           
         STCM  R1,7,PUBTAX1                                                     
*                                                                               
         BAS   RE,PUBUP                                                         
*                                                                               
         DROP  R5                                                               
*                                                                               
ADDPUB1  DS    0H                                                               
         CLI   QOPT1,C'N'                                                       
         BE    ADDPUB5                                                          
         GOTO1 ADDPUB                                                           
ADDPUB5  DS    0H                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   ADDPUB6                                                          
         CP    PUBCNT,=P'30'       DUMP FIRST 30 PUBS                           
         BH    ADDPUB6                                                          
         MVI   FORCEHED,C'Y'       EACH ON A NEW PAGE                           
         BAS   RE,DMPREC                                                        
*                                                                               
ADDPUB6  DS    0H                  NOW ALWAYS PRINT NAMES/ADDRESSES             
         CLI   LINE,50             SEE IF I CAN PRINT ALL LINES                 
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+34(35),DPUBNAME                                                
         MVC   P+81(35),WPUBNAME                                                
*                                                                               
         MVC   P+2(1),PUBREC               MEDIA                                
*                                                                               
         CLI   TRNKSW,C'Y'                SEE IF TRUNCATED                      
         BNE   *+8                                                              
         MVI   P+4,C'Y'                                                         
*                                                                               
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PUBREC+1),P+7                                 
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+34(45),DADDR1                                                  
         MVC   P+81(45),WADDR1                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+34(45),DADDR2                                                  
         MVC   P+81(45),WADDR2                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+34(30),DCITY                                                   
         MVC   P+66(2),PUBSTATE                                                 
         MVC   P+81(30),WCITY                                                   
         MVC   P+103(2),PUBSTATE                                                
         MVC   PSECOND+34(10),PUBNWZIP                                          
         UNPK  PSECOND+81(5),PWNEMZP1                                           
         OI    PSECOND+85,X'F0'                                                 
         UNPK  PSECOND+87(5),PWNEMZP2                                           
         OI    PSECOND+91,X'F0'                                                 
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
PUBN     DS    0H                  MUST BE NEWSPAPERS                           
*                                  PUB SUPPL INFO ELEMENT                       
         CLI   PWJGJ6ST,C'M'       IS MEDIA =M?                                 
         BE    ADDPUBX                                                          
*                                  PUB SUPPL INFO ELEMENT                       
         CLI   PWNENTYP,C' '       SEE IF PRESENT                               
         BNH   ADDPUBX                                                          
*                                  LTLREC                                       
         LA    R0,LTLREC                                                        
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PUBREC                                                   
         MVI   KEY+9,X'85'                                                      
*                                                                               
         MVC   LTLREC(25),KEY                                                   
         MVI   LTLREC+26,53                                                     
*                                                                               
         XC    ELEM,ELEM                                                        
         USING PUBSPREL,R5                                                      
*                                                                               
         MVI   PUBSPREL,X'21'      ELEM CODE                                    
         MVI   PUBSPREL+1,20       L'ELEMENT                                    
         MVC   PUBFREQ,PWNENTYP                                                 
         ZAP   PUBLCFC,=P'0'                                                    
         ZAP   PUBCOLWD,=P'0'                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         MVC   LTLREC+33(20),ELEM  ADD ELEM TO RECORD                           
*                                                                               
         CLI   QOPT1,C'N'                                                       
         BE    PUBN2                                                            
         GOTO1 ADDPUB                                                           
*                                                                               
PUBN2    DS    0H                                                               
         AP    LTLCNT,=P'1'                                                     
         CLI   QOPT2,C'Y'                                                       
         BNE   PUBN5                                                            
         CP    LTLCNT,=P'30'                                                    
         BH    PUBN5                                                            
         MVI   FORCEHED,C'Y'       EACH ON A NEW PAGE                           
         BAS   RE,DMPREC                                                        
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
NEWSCNT  DC    PL8'0'                                                           
         DC    CL20'NEWSPAPERS ADDED'                                           
FSICNT   DC    PL8'0'                                                           
         DC    CL20'FSI NEWSPAPERS ADDED'                                       
LTLCNT   DC    PL8'0'                                                           
         DC    CL20'LITTLE RECORDS ADDED'                                       
MAGSCNT  DC    PL8'0'                                                           
         DC    CL20'MAGAZINES ADDED'                                            
PUBCNT   DC    PL8'0'                                                           
         DC    CL20'TOTAL PUBS ADDED'                                           
         DC    X'FF'                                                            
*                                                                               
EOFSW    DC    C'N'                                                             
*                                                                               
FSINUM   DS    PL5                                                              
NEWNUM   DS    PL5                                                              
MAGNUM   DS    PL5                                                              
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL10   CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(20),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,28(R4)                                                        
         B     RUNL10                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
EOF      CLOSE (IN,)                                                            
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
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
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
         L     R5,AREC                                                          
         MVC   HALF,25(R5)                                                      
         LH    R2,HALF              USE RECORD LENGHT                           
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
*                                                                               
IN       DCB   DDNAME=IN,                                              X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00551,                                            X        
               BLKSIZE=24244,                                          X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
PPWWWRKD DSECT                                                                  
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
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
PPDUMD20 DSECT                                                                  
       ++INCLUDE PUBTAXEL                                                       
*                                                                               
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDBUFFALOD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE PPWWREC                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042PPREPWW02 05/01/02'                                      
         END                                                                    
