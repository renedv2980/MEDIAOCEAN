*          DATA SET PPREPWW02F AT LEVEL 052 AS OF 05/01/02                      
*PHASE PPWW02F,+0,NOAUTO                                                        
         TITLE 'PPWW02S - SPECIAL PUB FILE FIX FOR WESTERN'                     
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
*                                                                               
         XC    WPUBNAME,WPUBNAME                                                
         XC    WADDR1,WADDR1                                                    
         XC    WADDR2,WADDR2                                                    
         XC    WCITY,WCITY                                                      
         XC    DPUBNAME,DPUBNAME                                                
         XC    DZONE,DZONE                                                      
         XC    DADDR1,DADDR1                                                    
         XC    DADDR2,DADDR2                                                    
         XC    DCITY,DCITY                                                      
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
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST FIND                                    
         GOTO1 GETNAME                                                          
*                                                                               
         MVI   TRNKSW,C'N'                                                      
*                                                                               
         MVC   WPUBNAME,PWJGBBTX                                                
         MVC   DPUBNAME,PUBNAME                                                 
         CLC   PWJGBBTX+20(15),SPACES SEE IF PUB NAME WAS TRUNCATED             
         BNH   *+8                                                              
         MVI   TRNKSW,C'Y'                                                      
*                                                                               
         MVC   WADDR1,PWNEMAD1                                                  
*                               IF ALPHA MOVE TO ZONE                           
*                               AND MOVE PWNEMAD2 TO PUBLINE1                   
         OI    PWNEMAD1,C' '    BE SURE ITS UPPER CASE                          
         CLI   PWNEMAD1,C'0'                                                    
         BNL   NOZONE                                                           
         MVC   DZONE,PWNEMAD1                                                   
         OC    DZONE,SPACES                                                     
         MVC   PUBZNAME,DZONE                                                   
         MVC   WADDR2,PWNEMAD2    SAVE LINE 2                                   
         MVC   PUBLINE1,PWNEMAD2     MOVE LINE 2 TO LINE 1                      
         OC    PUBLINE1,SPACES                                                  
         MVC   DADDR1,PUBLINE1                                                  
         CLC   PWNEMAD2+30(15),SPACES                                           
         BNH   *+8                                                              
         MVI   TRNKSW,C'Y'                                                      
*                                                                               
         B     CITY                                                             
*                                                                               
NOZONE   MVC   DADDR1,PUBLINE1                                                  
         CLC   PWNEMAD1+30(15),SPACES                                           
         BNH   *+8                                                              
         MVI   TRNKSW,C'Y'                                                      
*                                                                               
         MVC   WADDR2,PWNEMAD2                                                  
*                           WILL  FIX ADDRESS LINE 2                            
*                                 TO CITY STATE ZIP                             
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
         BE    FIXADD2                                                          
*                                                                               
         UNPK  PUBNWZIP(5),PWNEMZP1(3) FIRST 5 DIGITS OF ZIP                    
         OI    PUBNWZIP+4,X'F0'                                                 
*                                                                               
         CP    PWNEMZP2,=P'0'                                                   
         BE    FIXADD2                                                          
*                                                                               
         UNPK  PUBNWZIP+6(4),PWNEMZP2(3) LAST 4 DIGITS OF ZIP                   
         OI    PUBNWZIP+9,X'F0'                                                 
         MVI   PUBNWZIP+5,C'-'                                                  
*                                                                               
FIXADD2  DS    0H                                                               
         MVC   PUBLINE2,PWNEMAD2                                                
         OC    PUBLINE2,SPACES                                                  
         CLI   PUBCITY,C' '     BE SURE I HAVE A CITY                           
         BNH   FIXAC        IF NOT,LEAVE PUBLINE2 SET TO THEIR DATA             
*                                                                               
         MVC   PUBLINE2,SPACES                                                  
         MVC   PUBLINE2(16),PUBCITY                                             
         LA    RE,PUBLINE2+16                                                   
FIXA3    CLI   0(RE),C' '                                                       
         BNE   FIXA5                                                            
         BCTR  RE,0                                                             
         B     FIXA3                                                            
*                                                                               
FIXA5    MVI   1(RE),C','                                                       
         MVC   2(2,RE),PUBSTATE                                                 
         MVC   5(10,RE),PUBNWZIP                                                
         OC    PUBLINE2,SPACES     JUST IN CASE                                 
*                                  ADD ELEM TO THE RECORD                       
         DROP  R5                                                               
*                                  FIRST FIX AGY COMM                           
FIXAC    DS    0H                                                               
         MVC   DADDR2,PUBLINE2                                                  
*                                                                               
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   ADDPUB1                                                          
         USING PUBGENEL,R2                                                      
         CP    PUBAC,=P'1500'                                                   
         BNE   *+10                                                             
         ZAP   PUBAC,=P'15000'                                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
ADDPUB1  DS    0H                                                               
         CLI   QOPT1,C'N'                                                       
         BE    ADDPUB5                                                          
         GOTO1 PUTPUB                                                           
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
         MVC   P+21(4),=C'NAME'                                                 
         MVC   P+34(20),DPUBNAME                                                
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
         MVC   P+21(4),=C'ZONE'                                                 
         MVC   P+34(20),DZONE                                                   
         MVC   P+81(45),WADDR1                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+21(5),=C'LINE1'                                                
         MVC   P+34(30),DADDR1                                                  
         MVC   P+81(45),WADDR2                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+21(5),=C'LINE2'                                                
         MVC   P+34(30),DADDR2                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+21(10),=C'CITY/STATE'                                          
         MVC   P+34(16),DCITY                                                   
         MVC   P+66(2),PUBSTATE                                                 
         MVC   P+81(30),WCITY                                                   
         MVC   P+103(2),PUBSTATE                                                
         MVC   PSECOND+21(8),=C'ZIP CODE'                                       
*                                                                               
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
*                                                                               
ADDPUBX  DS    0H                                                               
         MVI   TRNKSW,C'N'                                                      
         B     GET                                                              
*                                                                               
         SPACE 2                                                                
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
DPUBNAME DS    CL20                                                             
WADDR1   DS    CL45                                                             
DADDR1   DS    CL30                                                             
WADDR2   DS    CL45                                                             
DADDR2   DS    CL30                                                             
WCITY    DS    CL30                                                             
DCITY    DS    CL16                                                             
DZONE    DS    CL20                                                             
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
**PAN#1  DC    CL21'052PPREPWW02F05/01/02'                                      
         END                                                                    
