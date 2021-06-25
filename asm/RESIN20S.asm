*          DATA SET RESIN20S   AT LEVEL 038 AS OF 05/01/02                      
*PHASE T80620A,*                                                                
         TITLE 'T80620 - REPPAK SINGLE INVOICE (SIN) - DISPLAY MODULE'          
*                                                                               
*********************************************************************           
*                                                                   *           
*        RESIN20 --- REP SINGLE INVOICE - DISPLAY                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* AUG23/89 (MRR) --- HISTORY LOST                                   *           
*                                                                   *           
* JUL22/94 (SKU) --- PREVENT DIS/CHA OF FORECAST CONTRACTS          *           
*                                                                   *           
* MAY20/96 (WSB) --- CHANGE RESINGEN TO ADD 600 BYTES OF LOCAL      *           
*                    WORKING STORAGE                                *           
*                                                                   *           
* MAR25/99 (BU ) --- ALTERNATE CALENDAR DISPLAY                     *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
T80620   CSECT                                                                  
         NMOD1 0,T80620                                                         
         PRINT NOGEN                                                            
         USING GENOLD,RC                                                        
         USING T806FFD,RA                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R8,IOAREA           SET A(CONTRACT RECORD)                       
         AH    R8,=Y(RCONREC-IOAREA)                                            
*                                  DISPLACE TO CONTRACT RECORD                  
         USING RCONREC,R8                                                       
*                                                                               
         CLC   SINACT(3),=C'CHA'   REGULAR $ CHANGE?                            
         BE    CHGENTRY                                                         
         CLC   SINACT(3),=C'CHN'   ALTERNATE CALENDAR $ CHANGE?                 
         BE    CHGENTRY                                                         
* DISPLAY K HEADER                                                              
         FOUT  SINMODH,SPACES,11                                                
         FOUT  SINAGYH,SPACES,29                                                
         FOUT  SINBUYH,SPACES,29                                                
         FOUT  SINADVH,SPACES,29                                                
         FOUT  SINSTAH,SPACES,29                                                
         FOUT  SINPRDH,SPACES,29                                                
         FOUT  SINSALH,SPACES,29                                                
         FOUT  SINOFFH,SPACES,29                                                
         FOUT  SINDTSH,SPACES,29                                                
         FOUT  SINCM1H,SPACES,60                                                
         FOUT  SINCM2H,SPACES,60                                                
         B     D150                                                             
*                                                                               
* THIS ROUTINE FOUTS ZEROES TO NON-ZERO FIELDS                                  
FOUTZERO NTR1                                                                   
* FOUT ZEROES TO ALL NON-ZERO FIELDS                                            
         SR    R4,R4                                                            
D50      IC    R4,0(R2)            FIELD LEN                                    
         SH    R4,=H'9'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)                                                    
         BZ    D100                                                             
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         FOUT  (R2)                                                             
*                                                                               
D100     IC    R4,0(R2)            NEXT FIELD                                   
         LA    R2,0(R4,R2)                                                      
         CR    R2,R3                                                            
         BL    D50                                                              
         XIT1                                                                   
*                                                                               
D150     LA    R2,SINMN1H                                                       
         LA    R3,SINLAST                                                       
* FOUT ZEROES TO ALL NON-ZERO AMOUNT FIELDS IN CASE DISPLAY ERROR               
         BAS   RE,FOUTZERO                                                      
* GET CONTRACT                                                                  
         LA    R3,CONERR                                                        
         LA    R2,SINNUMH                                                       
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         XC    TWAKADDR,TWAKADDR   NO CONTRACT DISK ADDR                        
* GET 9'S COMPLEMENT                                                            
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   RCONPCON,WORK                                                    
* BUILD K KEY                                                                   
         MVC   RCONPREP,REPALPHA                                                
         MVI   RCONPTYP,X'8C'                                                   
         MVC   KEY,RCONREC                                                      
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         BAS   RE,GETREC                                                        
         TM    IOAREA+29,X'80'     DELETE?                                      
         BO    ERROR                                                            
*                                                                               
         LA    R3,136              CANNOT DIS/CHA A FORECAST CONTRACT           
         LA    R2,SINNUMH                                                       
         BAS   RE,IS4CAST          IS THIS A FORECAST CONTRACT?                 
         BZ    ERROR               YES, THEN ERROR                              
*                                                                               
         MVC   TWAKADDR,KEY+28     K DISK ADDR                                  
         GOTO1 VMOVEREC,DMCB,IOAREA,RCONREC                                     
         MVC   TWAKDTES,RCONDATE   SAVE K DATES                                 
         EJECT                                                                  
* DISPLAY MOD NUMBER IF NON 'ACE'                                               
* DISPLAY MOD NUMBER OR VERSION IF ACE OR GRAPHNET                              
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    D200                                                             
         SPACE 1                                                                
         SR    R4,R4                                                            
         LA    R3,RCONELEM                                                      
         B     *+12                                                             
D180     IC    R4,1(R3)            NEXT ELEM                                    
         LA    R3,0(R4,R3)                                                      
         CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'1F'                                                      
         BNE   D180                                                             
         SPACE 1                                                                
         USING RCONXEL,R3                                                       
         TM    RCONCONF,X'40'      CONFIRMED - SHOW MOD NO.                     
         BO    D200                                                             
         IC    R4,1(R3)            OTHERWISE, SHOW LATEST VERSION               
         AR    R3,R4                                                            
         CLI   0(R3),X'20'         SHOULD BE SEND ELEMENT                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R3                                                      
         CLC   RCONSRV,RCONSSV                                                  
         BH    D190                                                             
         EDIT  (1,RCONSSV),(3,SINMOD+8),ALIGN=LEFT                              
         B     D195                                                             
D190     EDIT  (1,RCONSRV),(3,SINMOD+8),ALIGN=LEFT                              
D195     MVC   SINMOD(7),=C'VERSION'                                            
         TM    RCONMODR+1,X'40'    GRAPHNET                                     
         BZ    D250                                                             
         MVC   SINMOD(7),=C'TWX VER'                                            
         DROP  R3                                                               
         B     D250                                                             
         SPACE 1                                                                
D200     CLI   RCONMOD,0                                                        
         BE    D250                                                             
         MVC   SINMOD(7),=C'MOD NUM'                                            
         EDIT  (1,RCONMOD),(3,SINMOD+8),ALIGN=LEFT                              
* GEN AGENCY NAME                                                               
D250     XC    IOAREA(32),IOAREA                                                
         MVI   RAGYKTYP,10                                                      
         MVC   RAGYKAGY,RCONKAGY                                                
         MVC   RAGYKAOF,RCONKAOF                                                
         MVC   RAGYKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   D270                                                             
         CLC   KEY+25(2),REPALPHA                                               
         BE    D260                                                             
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    D260                                                             
         MVC   KEY+25(2),=C'ZZ'                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   D270                                                             
D260     BAS   RE,GETREC                                                        
         MVC   SINAGY(4),RAGYKAGY                                               
         CLC   RAGYKAOF(2),SPACES                                               
         BE    *+14                                                             
         MVI   SINAGY+4,C'-'                                                    
         MVC   SINAGY+5(2),RAGYKAOF                                             
         MVC   SINAGY+9(20),RAGYNAM1                                            
* BUYER                                                                         
D270     MVC   SINBUY+9(20),RCONBUYR                                            
*                                                                               
* GET ADVERTISER                                                                
         XC    IOAREA(32),IOAREA                                                
         MVI   RADVKTYP,8                                                       
         MVC   RADVKADV,RCONKADV                                                
         MVC   RADVKREP,REPALPHA                                                
         MVC   KEY,IOAREA                                                       
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   D290                                                             
         CLC   KEY+25(2),REPALPHA                                               
         BE    D280                                                             
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    D280                                                             
         MVC   KEY+25(2),=C'ZZ'                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   D290                                                             
D280     BAS   RE,GETREC                                                        
         MVC   SINADV(4),RADVKADV                                               
         MVC   SINADV+9(20),RADVNAME                                            
*                                                                               
* GET STATION                                                                   
D290     XC    IOAREA(32),IOAREA                                                
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         MVC   SINSTA(4),RSTAKSTA                                               
         MVI   SINSTA+4,C'-'                                                    
         MVC   SINSTA+5(2),=C'FM'                                               
         CLI   RSTAKSTA+4,C'F'                                                  
         BE    D300                                                             
         MVC   SINSTA+5(2),=C'AM'                                               
         CLI   RSTAKSTA+4,C'A'                                                  
         BE    D300                                                             
         MVC   SINSTA+5(2),=C'CM'  COMBINED STATION                             
         CLI   RSTAKSTA+4,C'C'                                                  
         BE    D300                                                             
         MVC   SINSTA+5(2),=C'TV'                                               
D300     MVC   SINSTA+9(20),RSTAMKT                                             
*                                                                               
* GET PRODUCT ELEMENT                                                           
         SR    R4,R4                                                            
         LA    R3,RCONELEM                                                      
         B     *+12                                                             
D400     IC    R4,1(R3)            NEXT ELEM                                    
         LA    R3,0(R4,R3)                                                      
         CLI   0(R3),0                                                          
         BE    D500                                                             
         CLI   0(R3),5             PRD ELEM?                                    
         BNE   D400                                                             
         MVC   SINPRD+9(20),2(R3)  PRODUCT EXPANSION                            
*                                                                               
* GET SALESMAN                                                                  
D500     XC    IOAREA(32),IOAREA                                                
         MVI   RSALKTYP,6                                                       
         MVC   RSALKREP,REPALPHA                                                
         MVC   RSALKSAL,RCONSAL                                                 
         MVC   KEY,IOAREA                                                       
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         MVC   SINSAL(3),RSALKSAL                                               
         MVC   SINSAL+9(20),RSALNAME                                            
*                                                                               
* GET OFFICE                                                                    
         XC    IOAREA(32),IOAREA                                                
         MVI   ROFFKTYP,4                                                       
         MVC   ROFFKREP,REPALPHA                                                
         MVC   ROFFKOFF,RCONKOFF                                                
         MVC   KEY,IOAREA                                                       
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         MVC   SINOFF(2),ROFFKOFF                                               
         MVC   SINOFF+9(20),ROFFNAME                                            
*                                                                               
* DISPLAY K DATES                                                               
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(5,SINDTS+9)                           
         MVI   SINDTS+18,C'-'                                                   
         GOTO1 (RF),(R1),(3,RCONDATE+3),(5,SINDTS+20)                           
*                                                                               
* GET COMMENTS                                                                  
         LA    R3,RCONELEM                                                      
         SR    R4,R4                                                            
         B     *+12                                                             
D550     IC    R4,1(R3)                                                         
         LA    R3,0(R4,R3)                                                      
         CLI   0(R3),0                                                          
         BE    D600                                                             
         CLI   0(R3),2             COMMENT CODE                                 
         BNE   D550                                                             
*                                                                               
         IC    R4,1(R3)                                                         
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   SINCM1(0),2(R3)                                                  
*                                                                               
         IC    R4,1(R3)                                                         
         LA    R3,0(R4,R3)         NEXT ELEM                                    
         CLI   0(R3),2                                                          
         BNE   D600                                                             
* 2D COMMENT                                                                    
         IC    R4,1(R3)                                                         
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   SINCM2(0),2(R3)                                                  
         EJECT                                                                  
* GET K BROADCAST MONTHS IN TWAMONS FOR CONTRACT                                
D600     GOTO1 VDATCON,DMCB,(3,RCONDATE),WORK                                   
         GOTO1 (RF),(R1),(3,RCONDATE+3),WORK+6                                  
* GET MONDAY START FOR K                                                        
         GOTO1 VGETDAY,DMCB,WORK,FULL                                           
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R6,R6                                                            
         IC    R6,DMCB                                                          
         BCTR  R6,R0                                                            
         LNR   R6,R6                                                            
* GET PREVIOUS MONDAY                                                           
         GOTO1 VADDAY,(R1),WORK,DUB,(R6)                                        
         MVC   WORK(6),DUB         K START IS NOW MONDAY START                  
         XC    TWAMONS,TWAMONS                                                  
         LA    R3,TWAMONS                                                       
D700     GOTO1 VGTBROAD,(R1),(1,WORK),WORK+12,VGETDAY,VADDAY                    
* BROADCAST DATES NOW IN WORK+12                                                
         CLI   DMCB2,X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
* CONVERT BROADCAST MONTH END DATE                                              
         GOTO1 VDATCON,(R1),WORK+18,(3,FULL)                                    
         CLC   0(2,R3),FULL        MONTH IN TWAMONS YET?                        
         BE    D750                                                             
         CLI   0(R3),0             FIRST?                                       
         BE    *+8                                                              
         LA    R3,2(R3)                                                         
         MVC   0(2,R3),FULL        NEXT TWAMONS                                 
* ADD 1 WEEK TO RUNNING K START DATE                                            
D750     GOTO1 VADDAY,(R1),WORK,WORK+24,7                                       
         MVC   WORK(6),WORK+24                                                  
         LA    R6,TWAMONS+24                                                    
         CR    R3,R6                                                            
         BNL   *+14                                                             
         CLC   WORK(6),WORK+6      PAST K END DATE YET?                         
         BNH   D700                                                             
         EJECT                                                                  
* DISPLAY MONTH TOTALS                                                          
CHGENTRY LA    R2,SINMN1H                                                       
         LA    R3,SINLAST                                                       
* FOUT NON-ZERO FIELDS WITH ZEROES                                              
         BAS   RE,FOUTZERO                                                      
         LA    R2,SINMN1H          FIRST OUTPUT LINE                            
         SR    R4,R4                                                            
         LA    R3,TWAMONS          YR-MONTH LIST FOR K                          
         XC    WORK2(12),WORK2     TOTALS                                       
* DISPLAY MONTH LINE                                                            
D800     EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,(R3)),(6,8(R2))                                  
* GET ORDERED AMOUNT                                                            
         CLC   SINACT(3),=C'DIS'   REGULAR $ DISPLAY REQUEST?                   
         BE    D805                YES                                          
         CLC   SINACT(3),=C'CHA'   REGULAR $ CHANGE REQUEST?                    
         BNE   D810                NO                                           
D805     EQU   *                                                                
         GOTO1 VGETMON,(R1),(3,RCONREC),(R3),FULL                               
         B     D820                                                             
D810     EQU   *                                                                
         GOTO1 VGETMON,(R1),(X'53',RCONREC),(R3),FULL                           
*                                                                               
D820     EQU   *                                                                
         FOUT  (R2)                                                             
         L     R5,FULL             ORD AMOUNT                                   
         LTR   R5,R5                                                            
         BZ    D850                                                             
         EDIT  (R5),(12,19(R2)),2,COMMAS=YES,FLOAT=-                            
* IN CASE OF MILLION                                                            
         CLI   WORK+6,C','    COULD BE NEG. NUMBER INSTEAD OF MILLION           
         BNE   *+10                                                             
         MVC   19(2,R2),WORK+4      IF MILLION, SKIP COMMA                      
D850     IC    R4,0(R2)            NEXT FIELD                                   
         LA    R2,0(R4,R2)                                                      
* GET INVOICE AMT FOR MONTH                                                     
         CLC   SINACT(3),=C'DIS'   REGULAR $ DISPLAY REQUEST?                   
         BE    D855                YES                                          
         CLC   SINACT(3),=C'CHA'   REGULAR $ CHANGE REQUEST?                    
         BNE   D860                NO                                           
D855     EQU   *                                                                
         GOTO1 VGETMON,(R1),(4,RCONREC),(R3),FULL                               
         B     D870                                                             
D860     EQU   *                                                                
         GOTO1 VGETMON,(R1),(X'54',RCONREC),(R3),FULL                           
D870     EQU   *                                                                
         L     R6,FULL             INV AMT                                      
         FOUT  (R2)                                                             
         CLI   4(R1),X'FF'         ANY BUCKETS?                                 
         BE    *+10                                                             
         LTR   R6,R6                                                            
         BZ    D900                                                             
         EDIT  (R6),(11,8(R2)),2,COMMAS=YES,FLOAT=-                             
* IN CASE OF MILLION                                                            
         CLI   WORK+6,C','    COULD BE NEG. NUMBER INSTEAD OF MILLION           
         BNE   *+10                                                             
         MVC   8(1,R2),WORK+5      IF MILLION, SKIP COMMA                       
* GET DIFFERENCE                                                                
D900     LR    R7,R5                                                            
         SR    R7,R6               GET DIFFERENCE                               
         IC    R4,0(R2)            NEXT FIELD                                   
         LA    R2,0(R4,R2)                                                      
         FOUT  (R2)                                                             
*                                                                               
         LTR   R7,R7                                                            
         BZ    D950                                                             
         EDIT  (R7),(11,8(R2)),2,COMMAS=YES,FLOAT=-                             
* IN CASE OF MILLION                                                            
         CLI   WORK+6,C','    COULD BE NEG. NUMBER INSTEAD OF MILLION           
         BNE   *+10                                                             
         MVC   8(1,R2),WORK+5      IF MILLION, SKIP COMMA                       
* ADD TO TOTALS                                                                 
D950     A     R5,WORK2                                                         
         A     R6,WORK2+4                                                       
         A     R7,WORK2+8                                                       
         STM   R5,R7,WORK2                                                      
*                                                                               
         IC    R4,0(R2)                                                         
         LA    R2,0(R4,R2)         NEXT FIELD                                   
*                                                                               
         LA    R3,2(R3)            TWAMONS - NEXT MONTH                         
         CLI   0(R3),0             LAST MONTH?                                  
         BNE   D800                                                             
* DISPLAY TOTALS                                                                
         MVC   SINMNT(6),=C'TOTALS'                                             
         LA    R3,SINMNT+11        ORD TOTAL                                    
         EDIT  (4,WORK2),(12,(R3)),2,COMMAS=YES,FLOAT=-                         
* IN CASE OF MILLION                                                            
         CLI   WORK+6,C','    COULD BE NEG. NUMBER INSTEAD OF MILLION           
         BNE   *+10                                                             
         MVC   SINMNT+11(2),WORK+4    IF MILLION, SKIP COMMA                    
         MVI   SINMNT+23,C'*'                                                   
         FOUT  SINMNTH                                                          
* INVOICE TOTAL                                                                 
         EDIT  (4,WORK2+4),(12,SININT),2,COMMAS=YES,FLOAT=-                     
* IN CASE OF MILLION                                                            
         CLI   WORK+6,C','    COULD BE NEG. NUMBER INSTEAD OF MILLION           
         BNE   *+10                                                             
         MVC   SININT(2),WORK+4    IF MILLION, SKIP COMMA                       
         FOUT  SININTH                                                          
         MVI   SININT+12,C'*'                                                   
* DIFFERENCE TOTAL                                                              
         EDIT  (4,WORK2+8),(12,SINDFT),2,COMMAS=YES,FLOAT=-                     
* IN CASE OF MILLION                                                            
         CLI   WORK+6,C','    COULD BE NEG. NUMBER INSTEAD OF MILLION           
         BNE   *+10                                                             
         MVC   SINDFT(2),WORK+4    IF MILLION, SKIP COMMA                       
         FOUT  SINDFTH                                                          
         MVI   SINDFT+12,C'*'                                                   
         EJECT                                                                  
* CONTRACT DISPLAYED                                                            
         LA    R2,SINACTH          CURSOR                                       
         MVC   SINMSG(31),=C'CONTRACT INVOICE DATA DISPLAYED'                   
         CLC   SINACT(3),=C'DIS'                                                
         BE    EXIT                                                             
         CLC   SINACT(3),=C'DIN'                                                
         BE    EXIT                                                             
         MVC   SINMSG+22(9),=CL9'CHANGED'                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DROP  R8                  MAIN 'DROP' FOR CONTRACT RECORD              
*                                     4100 BYTE AREA                            
***********************************************************************         
* IS CONTRACT REQUESTED A FORECAST CONTRACT?                                    
* RETURNS CC                                                                    
***********************************************************************         
IS4CAST  NTR1                                                                   
         LA    R6,IOAREA                                                        
         USING RCONREC,R6                                                       
         LA    R3,RCONELEM                                                      
         B     IS4CST20                                                         
         DROP  R6                                                               
                                                                                
IS4CST10 DS    0H                                                               
         ZIC   R0,1(R3)            NEXT ELEM                                    
         AR    R3,R0                                                            
                                                                                
IS4CST20 DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    NOT4CAST            NOT FOUND                                    
                                                                                
         CLI   0(R3),3             FORECAST CONTRACT HAS NO                     
         BE    NOT4CAST            BUCKETS                                      
         CLI   0(R3),4                                                          
         BE    NOT4CAST            INVOICE BUCKETS                              
         CLI   0(R3),6                                                          
         BE    NOT4CAST            SPL/EPL DATA                                 
                                                                                
         CLI   0(R3),X'12'                                                      
         BNE   IS4CST10                                                         
                                                                                
         USING RSARXEL,R3          FOUND. IS THERE A FORECAST FLAG?             
         CLI   RSARXLEN,RSARXLTH   NO ELEMENT IS OLD SAR ELEMENT                
         BL    NOT4CAST                                                         
         TM    RSARXFLG,X'18'                                                   
         BNZ   YES4CAST                                                         
         DROP  R3                                                               
                                                                                
NOT4CAST LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES4CAST SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XIT1                                                                   
       ++INCLUDE RESINGEN                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038RESIN20S  05/01/02'                                      
         END                                                                    
