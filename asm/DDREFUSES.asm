*          DATA SET DDREFUSES  AT LEVEL 016 AS OF 10/19/94                      
**********************************************************************          
*  THIS PROGRAM IS DESIGNED TO READ CARDS FROM YOUR JCL                         
*  AND READ THE APPROPRIATE RECORDS FROM ANY RECOVERY FILE.                     
*                                                                               
*  YOU MUST SPECIFY THE DISK/TAPE TO BE READ IN //RECVIN DD STATEMENT           
*  AND THE DATASET IF YOU WISH TO WRITE TO ONE IN A //RECVOUT DD                
*  STATEMENT.                                                                   
*                                                                               
*  CARDS CAN BE GIVEN AS FOLLOWS:                                               
* REQUIRED CARDS-- (INPUT PARAMETERS)                                           
*************************************                                           
*OUTPUT=PRINT/DISK/BOTH                                                         
*      NOTE: WHEN DISK IS SPECIFIED YOU WILL STILL GET A PRINT                  
*            OF THE INPUT PARAMETERS AND THE TOTALS OF RECORDS                  
*            READ AND WRITTEN.                                                  
*            AND WHEN PRINT IS SPECIFIED THE RECOVERY HEADER IS                 
*            PARTLY TRANSLATED FOR EASY READING                                 
*                                                                               
*FILES=##,##,##                                                                 
*      THE LIST OF FILE NUMBERS TO DESIRED - AS THEY APPEAR IN HEX              
*                                                                               
*KEY=X'000000'10C'ABC'                                                          
*      AN EXACT KEY TO SEARCH FOR OR...                                         
*                                                                               
*LOWKEY=X'0000'                                                                 
*      A START AT KEY (CAN USE X'00' FOR ALL RECORDS)                           
*                                                                               
* OPTIONAL CARDS-- (INPUT PARAMETERS)                                           
*************************************                                           
*HIGHKEY=X'0000'                                                                
*      AN UPPER LIMIT FOR KEY SEARCH                                            
*                                                                               
*TERMNUM=2746                                                                   
*      FILTER ON A SPECIFIC TERMINAL NUMBER (DECIMAL)                           
*                                                                               
*LOWTIME=01:52:34                                                               
*      A START AT TIME FILTER- SECONDS NEED NOT BE SPECIFIED                    
*      BUT THE LEADING ZERO IS REQUIRED FOR HOURS                               
*                                                                               
*HIGHTIME=11:54                                                                 
*      AN UPPER LIMIT ON TIME - SAME RULES                                      
*                                                                               
*DISKADR=00010A00                                                               
*      FILTER ON DIXK ADDRESS - CAN BE 6 OR 8 CHARACTERS                        
*                                                                               
*USERID=25                                                                      
*      FILTER ON USER ID NUMBER (DECIMAL)                                       
*                                                                               
*SYSIN=01AB45                                                                   
*      FILTER ON SYSTEM INPUT NUMBER IN HEX                                     
*                                                                               
****************************************************************                
*PHASE REFUSEA,*                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE DECODE                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE CASHVAL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE SCANNER                                                                
         TITLE 'MYRCVPEEL - PEEL/SORT/SAVE ANY RECOVERY DATA'                   
REFUSE   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,REFUSE,VREGSAVE                                                
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING REFUSE+4096,RC                                                   
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
*                                                                               
INIT2    DS    0H                                                               
         LA    RE,REFUSE            SET FOR STXITER                             
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'INPUT PARAMETERS:'                                      
         GOTO1 =V(PRINTER)                                                      
         EJECT                                                                  
********************************************************************            
*  READ CARDS TO GET INFO                                                       
********************************************************************            
NXTCARD  GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    ENDCARD                                                          
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*-----------------> OUTPUT CARD SPECIFIES PRINT, DISK, OR BOTH                  
CKOUT    CLC   =C'OUTPUT=',CARD    OUTPUT CARD?                                 
         BNE   CKFILE                                                           
         OI    FLAG,X'02'          YES OUTPUT CARD FOUND                        
         CLC   =C'PRINT',CARD+7    OUTPUT TO PRINTER                            
         BNE   *+12                                                             
         OI    FLAG,X'80'          FLAG FOR PRINT                               
         B     NXTCARD                                                          
         CLC   =C'DISK',CARD+7     OUTPUT TO DISK FILE                          
         BNE   *+12                                                             
         OI    FLAG,X'40'          FLAG FOR DISK                                
         B     NXTCARD                                                          
         CLC   =C'BOTH',CARD+7     OUTPUT TO BOTH                               
         BNE   *+12                                                             
         OI    FLAG,X'C0'          BOTH                                         
         B     NXTCARD                                                          
*                                                                               
*-----------------> FILES CARD LISTS FILE NUMS WANTED                           
CKFILE   CLC   =C'FILES=',CARD     FILES CARD?                                  
         BNE   CKKEY               NO                                           
         OI    FLAG,X'04'          FILES CARD FOUND, YES                        
         LA    R2,CARD                                                          
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),SCANBOX                             
         CLI   DMCB+4,0            ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R3,SCANBOX                                                       
         LA    R4,FILETBL                                                       
*        *                                                                      
* CHECK SECOND FIELD OF SCANNER FOR FIRST GIVEN FILE                            
*        *                                                                      
         CLI   1(R3),0             ANY INPUT IN 2ND FIELD                       
         BNE   *+6                                                              
         DC    H'0'                NEED AT LEAST ONE FILE                       
         TM    3(R3),X'20'         VALID HEX?                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ZIC   R5,1(R3)            SOURCE LENGTH                                
         GOTO1 =V(HEXIN),DMCB,22(R3),0(R4),(R5)                                 
         CLI   DMCB+16,1           WANT OUTPUT LENGTH OF ONE                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R4,1(R4)            FILES TABLE BUMPED                           
         LA    R3,32(R3)           BUMP SCANNER BOX                             
         B     FILELOOP                                                         
*                                                                               
* NOW CHECK FOR MORE FILES                                                      
*                                                                               
FILELOOP CLI   0(R3),0             ANY INPUT IN FIRST FIELD                     
         BE    ENDFILE                                                          
         TM    2(R3),X'20'         VALID HEX?                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ZIC   R5,0(R3)            SOURCE LENGTH                                
         GOTO1 =V(HEXIN),DMCB,12(R3),0(R4),(R5)                                 
         CLI   DMCB+16,1           WANT OUTPUT LENGTH OF ONE                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,1(R4)            FILES TABLE BUMPED                           
         CLI   1(R3),0             ANY INPUT IN 2ND FIELD                       
         BE    ENDFILE                                                          
         TM    3(R3),X'20'         VALID HEX?                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ZIC   R5,1(R3)            SOURCE LENGTH                                
         GOTO1 =V(HEXIN),DMCB,22(R3),0(R4),(R5)                                 
         CLI   DMCB+16,1           WANT OUTPUT LENGTH OF ONE                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R4,1(R4)            FILES TABLE BUMPED                           
         LA    R3,32(R3)           BUMP SCANNER BOX                             
         B     FILELOOP                                                         
ENDFILE  MVI   0(R4),X'FF'         END OF TABLE MARKER                          
         B     NXTCARD                                                          
*                                                                               
*-----------------> KEY CARD FILTERS RECORD BY THE GIVEN KEY                    
CKKEY    CLC   =C'KEY=',CARD       KEY CARD?                                    
         BNE   CKLKEY              NO, CHECK FOR LOW KEY                        
         OI    FLAG,X'08'          FOUND KEY CARD FLAG                          
         LA    R3,CARD+4                                                        
         GOTO1 =V(DECODE),DMCB,(R3),FKEY                                        
         MVC   KLEN(4),DMCB+8      KEY LENGTH                                   
         B     NXTCARD                                                          
*                                                                               
*-----------------> LOWKEY CARD IS A START AT KEY FILTER                        
CKLKEY   CLC   =C'LOWKEY=',CARD    LOWKEY CARD?                                 
         BNE   CKHKEY              NO, CHECK FOR HIGH KEY                       
         OI    FLAG,X'10'          LOW KEY CARD FLAG                            
         LA    R3,CARD+7                                                        
         GOTO1 =V(DECODE),DMCB,(R3),FLKEY                                       
         MVC   LKLEN(4),DMCB+8     LOWKEY LENGTH                                
         B     NXTCARD                                                          
*                                                                               
*-----------------> HIGHKEY IS AN UPPER LIMIT TO KEY FILTER                     
CKHKEY   CLC   =C'HIGHKEY=',CARD   HIGHKEY CARD?                                
         BNE   CKTERM              NO                                           
         OI    FLAG,X'01'          HIGH KEY CARD FLAG                           
         LA    R3,CARD+8                                                        
         GOTO1 =V(DECODE),DMCB,(R3),FHKEY                                       
         MVC   HKLEN(4),DMCB+8     HIGHKEY LENGTH                               
         B     NXTCARD                                                          
*                                                                               
*-----------------> TERMNUM CARD FILTER ON TERMINAL NUM (DECIMAL)               
CKTERM   CLC   =C'TERMNUM=',CARD   TERMNUM CARD?                                
         BNE   CKTIME                                                           
         LA    R2,CARD                                                          
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),SCANBOX                             
         CLI   DMCB+4,0            ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R2,SCANBOX                                                       
         TM    3(R2),X'80'         VALID NUMERIC                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   1(R2),5             ONLY ALLOW UP TO 5 NUMBERS                   
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   TRNUM,10(R2)        BINARY VALUE OF TERM#                        
         B     NXTCARD                                                          
*                                                                               
*-----------------> LOWTIME CARD GIVES START TIME (HH:MM:SS/HH:MM)              
CKTIME   CLC   =C'LOWTIME=',CARD                                                
         BNE   CKHTIME                                                          
         LA    R2,CARD                                                          
         MVC   SCANBOX(130),SPACES                                              
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),SCANBOX,C',=:='                     
         CLI   DMCB+4,0            ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R2,SCANBOX                                                       
         CLI   1(R2),2             LENGTH OF 2                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   1(R2),0             NEED SOMETHING                               
         BH    *+6                                                              
         DC    H'0'                                                             
         TM    3(R2),X'80'         VALID NUMERIC?                               
         BNZ   *+6                 YES                                          
         DC    H'0'                                                             
         MVC   DUB+2(2),22(R2)     MOVE HH INTO DUB                             
*                                                                               
         LA    R2,32(R2)           NEXT SCANNER LINE                            
         CLI   0(R2),2             LENGTH MUST BE 2                             
         BNH   *+6                                                              
         DC    H'0'                                                             
         TM    2(R2),X'80'         VALID NUMERIC?                               
         BNZ   *+6                 YES                                          
         DC    H'0'                                                             
         MVC   DUB+4(2),12(R2)     MOVE MM INTO DUB                             
*                                                                               
         LA    R2,32(R2)           NEXT SCANNER LINE                            
         CLI   0(R2),2             ARE SECONDS GIVEN?                           
         BE    SECSL                                                            
         MVC   DUB+6(2),=X'00C0'    NO,PUT IN 00                                
         PACK  LTIME(4),DUB(8)                                                  
         B     NXTCARD                                                          
SECSL    DS    0H                                                               
         TM    2(R2),X'80'         VALID NUMERIC?                               
         BNZ   *+6                 YES                                          
         DC    H'0'                                                             
         MVC   DUB+6(2),12(R2)     MOVE SS INTO DUB                             
         NI    DUB+7,X'CF'                                                      
         PACK  LTIME(4),DUB(8)                                                  
         B     NXTCARD                                                          
*                                                                               
*-----------------> HIGHTIME CARD GIVES END TIME (HH:MM:SS/HH:MM)               
CKHTIME  CLC   =C'HIGHTIME=',CARD                                               
         BNE   CKDSCAD             CHECK DISK ADDRESS                           
         LA    R2,CARD                                                          
         MVC   SCANBOX(130),SPACES                                              
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),SCANBOX,C',=:='                     
         CLI   DMCB+4,0            ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R2,SCANBOX                                                       
         CLI   1(R2),2             LENGTH OF 2                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   1(R2),0             NEED SOMETHING                               
         BH    *+6                                                              
         DC    H'0'                                                             
         TM    3(R2),X'80'         VALID NUMERIC?                               
         BNZ   *+6                 YES                                          
         DC    H'0'                                                             
         MVC   DUB+2(2),22(R2)     MOVE HH INTO DUB                             
*                                                                               
         LA    R2,32(R2)           NEXT SCANNER LINE                            
         CLI   0(R2),2             LENGTH MUST BE 2                             
         BNH   *+6                                                              
         DC    H'0'                                                             
         TM    2(R2),X'80'         VALID NUMERIC?                               
         BNZ   *+6                 YES                                          
         DC    H'0'                                                             
         MVC   DUB+4(2),12(R2)     MOVE MM INTO DUB                             
*                                                                               
         LA    R2,32(R2)           NEXT SCANNER LINE                            
         CLI   0(R2),2             ARE SECONDS GIVEN?                           
         BE    SECSH                                                            
         MVC   DUB+6(2),=X'00C0'    NO,PUT IN 00                                
         PACK  HTIME(4),DUB(8)                                                  
         B     NXTCARD                                                          
SECSH    DS    0H                                                               
         TM    2(R2),X'80'         VALID NUMERIC?                               
         BNZ   *+6                 YES                                          
         DC    H'0'                                                             
         MVC   DUB+6(2),12(R2)     MOVE SS INTO DUB                             
         NI    DUB+7,X'CF'                                                      
         PACK  HTIME(4),DUB(8)                                                  
         B     NXTCARD                                                          
*                                                                               
*-----------------> DISKADR CARD FILTERS ON DISK ADDRESS (TTTTBBRR)             
CKDSCAD  CLC   =C'DISKADR=',CARD                                                
         BNE   CKPROG                                                           
         LA    R2,CARD                                                          
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),SCANBOX                             
         CLI   DMCB+4,0            ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R2,SCANBOX                                                       
         CLI   1(R2),0             ANY ADDRESS?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   1(R2),X'08'         8 DIGITS?                                    
         BL    ADR6                                                             
         DC    H'0'                                                             
         ZIC   R3,1(R2)            LENGTH                                       
         LA    R2,22(R2)                                                        
         GOTO1 =V(HEXIN),DMCB,(R2),DSKADR,(R3)                                  
         STC   R3,DALEN                                                         
         B     NXTCARD                                                          
ADR6     CLI   1(R2),X'06'         6 DIGITS?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R3,1(R2)            LENGTH                                       
         MVC   20(2,R2),=C'00'                                                  
         LA    R2,20(R2)                                                        
         GOTO1 =V(HEXIN),DMCB,(R2),DSKADR,(R3)                                  
         STC   R3,DALEN                                                         
         B     NXTCARD                                                          
*                                                                               
*-----------------> PROG CARD FILTERS ON PROGAM BYTE                            
CKPROG   CLC   =C'PROG=',CARD                                                   
         BNE   CKUSER                                                           
         LA    R2,CARD                                                          
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),SCANBOX                             
         CLI   DMCB+4,0            ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R2,SCANBOX                                                       
         CLI   1(R2),0             ANY ADDRESS?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   1(R2),X'02'         8 DIGITS?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R3,1(R2)            LENGTH                                       
         LA    R2,22(R2)                                                        
         GOTO1 =V(HEXIN),DMCB,(R2),PROGNUM,(R3)                                 
         B     NXTCARD                                                          
*                                                                               
*-----------------> USERID CARD FILTERS ON USER ID (DECIMAL)                    
CKUSER   CLC   =C'USERID=',CARD                                                 
         BNE   CKSYSIN                                                          
         LA    R2,CARD                                                          
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),SCANBOX                             
         CLI   DMCB+4,0            ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R2,SCANBOX                                                       
         CLI   1(R2),0             ANY INPUT                                    
         BH    *+6                                                              
         DC    H'0'                                                             
         TM    3(R2),X'80'         VALID NUMERIC                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   USER,10(R2)         BINARY VALUE                                 
         B     NXTCARD                                                          
*                                                                               
*-----------------> SYSIN CARD FILTERS ON SYSTEM INPUT NUMBER                   
CKSYSIN  CLC   =C'SYSIN=',CARD                                                  
         BNE   CKTRNAMT                                                         
         LA    R2,CARD                                                          
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),SCANBOX                             
         CLI   DMCB+4,0            ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R2,SCANBOX                                                       
         CLI   1(R2),0             ANY ADDRESS?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   1(R2),8             8 DIGITS?                                    
         BL    SI10                                                             
         DC    H'0'                                                             
         ZIC   R3,1(R2)            LENGTH                                       
         LA    R2,22(R2)                                                        
         GOTO1 =V(HEXIN),DMCB,(R2),SYSINP,(R3)                                  
         STC   R3,SILEN                                                         
         B     NXTCARD                                                          
SI10     CLI   1(R2),X'06'         6 DIGITS?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R3,1(R2)            LENGTH                                       
         MVC   20(2,R2),=C'00'                                                  
         LA    R3,2(R3)                                                         
         LA    R2,20(R2)                                                        
         GOTO1 =V(HEXIN),DMCB,(R2),SYSINP,(R3)                                  
         STC   R3,SILEN                                                         
         B     NXTCARD                                                          
*                                                                               
*-----------------> SYSIN CARD FILTERS ON SYSTEM INPUT NUMBER                   
CKTRNAMT CLC   =C'TRNAMT=',CARD                                                 
         BNE   NXTCARD                                                          
         LA    R2,CARD                                                          
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),SCANBOX                             
         CLI   DMCB+4,0            ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R2,SCANBOX                                                       
         CLI   1(R2),0             ANY ADDRESS?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R3,1(R2)            LENGTH                                       
         LA    R2,22(R2)                                                        
         GOTO1 =V(CASHVAL),DMCB,(X'82',0(R2)),(R3)                              
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZAP   TRANAMT(6),DMCB+6(6)                                             
         B     NXTCARD                                                          
         EJECT                                                                  
****************************************************************                
ENDCARD  DS    0H                                                               
         TM    FLAG,X'0E'          ARE ALL REQUIRED CARD GIVEN?                 
         BNO   *+8                                                              
         B     *+14                                                             
         TM    FLAG,X'16'                                                       
         BO    *+6                                                              
         DC    H'0'                REQUIRED CARD(S) NOT PRESENT                 
*                                                                               
****************************************************************                
*                                                                               
         SR    R7,R7               # OF RECORDS READ                            
         SR    R8,R8               # OF RECORDS WRITTEN                         
         OPEN  (RECVIN,(INPUT))                                                 
         TM    FLAG,X'40'          WRITING OUT TO FILE?                         
         BNO   MTITLE                                                           
         OPEN  (RECVOUT,(OUTPUT))  YES                                          
*                                                                               
MTITLE   MVC   TITLE(30),=CL30'RECOVERY PEEL PROGRAM'                           
         GOTO1 =V(PRINTER)         SKIP A SPACE                                 
         B     IN2                                                              
*                                                                               
         EJECT                                                                  
*****************************************************************               
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      LA    RE,RDATA                                                         
         LA    RF,RDATALNQ                                                      
         LR    R0,RE                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
         LA    R7,1(R7)            # OF RECORDS READ                            
*                                                                               
         LA    R4,FILETBL                                                       
NXTFILE  CLC   RFILTY,0(R4)        SAME FILE NUMBER                             
         BE    TSTKEY              YES                                          
         LA    R4,1(R4)            NEXT FILE NUMBER                             
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    IN2                 NEXT                                         
         B     NXTFILE                                                          
*                                                                               
TSTKEY   TM    FLAG,X'08'          STRAIGHT KEY?                                
         BZ    TSTLOWK             NO, TEST FOR LOW KEY                         
         L     R5,KLEN             KEY LENGTH                                   
*        LA    R5,3                                                             
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   RECVHDR+24(0),FKEY                                               
         BNE   IN2                                                              
         B     TSTTERM                                                          
*                                                                               
TSTLOWK  TM    FLAG,X'10'          LOW-HIGH KEY RANGE?                          
         BZ    TSTTERM             NO                                           
         L     R5,LKLEN                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   RECVHDR+24(0),FLKEY MATCH ON LOW KEY?                            
         BL    IN2                 NEXT                                         
         TM    FLAG,X'01'          IS THERE A HIGH KEY                          
         BZ    TSTTERM                                                          
         L     R5,HKLEN                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   RECVHDR+24(0),FHKEY MATCH ON HIGH KEY?                           
         BH    IN2                 NEXT                                         
*                                                                               
TSTTERM  DS    0H                                                               
         OC    TRNUM,TRNUM         TERMINAL NUMBER FILTER?                      
         BZ    TSTTIME                                                          
         CLC   TRNUM,RTRM                                                       
         BNE   IN2                                                              
*                                                                               
TSTTIME  OC    LTIME,LTIME         LOWTIME?                                     
         BZ    TSTHTIME                                                         
         CLC   RTIME,LTIME                                                      
         BL    IN2                                                              
*                                                                               
TSTHTIME OC    HTIME,HTIME         LOWTIME?                                     
         BZ    TSTDA                                                            
         CLC   RTIME,HTIME                                                      
         BH    IN2                                                              
*                                                                               
TSTDA    OC    DSKADR,DSKADR                                                    
         BZ    TSTSYSIN                                                         
         CLC   RVCHR,DSKADR                                                     
         BNE   IN2                                                              
*                                                                               
TSTSYSIN OC    SYSINP,SYSINP                                                    
         BZ    TSTPROG                                                          
         CLC   RSIN+1(3),SYSINP+1                                               
         BNE   IN2                                                              
*                                                                               
TSTPROG  OC    PROGNUM,PROGNUM                                                  
         BZ    TSTUSER                                                          
         CLC   RPRG,PROGNUM                                                     
         BNE   IN2                                                              
*                                                                               
TSTUSER  OC    USER,USER                                                        
         BZ    TSTTRN                                                           
         CLC   RUSER,USER                                                       
         BNE   IN2                                                              
*                                                                               
TSTTRN   CLI   RFILTY,X'6A'        ONLY CHECK FOR TRNAMT ON ACCMST              
         BNE   OUTREC                                                           
         OC    TRANAMT,TRANAMT                                                  
         BZ    OUTREC                                                           
*                                                                               
         USING TRNRECD,R6                                                       
         LA    R6,RECVHDR+24                                                    
         LA    R5,TRNRFST                                                       
TTRN05   CLI   0(R5),TRNELQ        X'44' TRANS ELEM                             
         BE    TTRN10                                                           
         ZIC   R1,0(R5)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),0                                                          
         BE    IN2                 NO MATCH                                     
         B     TTRN05                                                           
         USING TRNELD,R5                                                        
TTRN10   CP    TRANAMT,TRNAMNT                                                  
         BNE   IN2                                                              
         B     OUTREC                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
******************************************************************              
OUTREC   DS    0H                                                               
         LA    R8,1(R8)            # OF RECORDS WRITTEN                         
         TM    FLAG,X'40'          WRITE TO DISC FILE?                          
         BZ    PRNT                NO                                           
         LA    R0,RECVHDR-4                                                     
         PUT   RECVOUT,(R0)                                                     
         B     PRNT                                                             
*                                                                               
PRNT     DS    0H                                                               
         TM    FLAG,X'80'          PRINT OUT?                                   
         BZ    IN2                 NO                                           
*                                                                               
         MVC   P(5),=C'FILE='                                                   
         GOTO1 =V(HEXOUT),DMCB,RFILTY,P+5,1                                     
         MVC   P+9(5),=C'TRNS='                                                 
         CLI   RRECTY,X'01'        COPY?                                        
         BNE   *+10                                                             
         MVC   P+14(4),=C'COPY'                                                 
         CLI   RRECTY,X'02'        CHANGE?                                      
         BNE   *+10                                                             
         MVC   P+14(4),=C'CHG '                                                 
         CLI   RRECTY,X'03'        ADD?                                         
         BNE   *+10                                                             
         MVC   P+14(4),=C'ADD '                                                 
         MVC   P+20(6),=C'TERM#='                                               
         LA    R2,P+26                                                          
         EDIT  RTRM,(5,(R2))                                                    
         GOTO1 =V(HEXOUT),DMCB,RTIME,TEMPTIME,4                                 
         MVC   P+33(5),=C'TIME='                                                
         MVC   P+38(2),TEMPTIME+1                                               
         MVI   P+40,C':'                                                        
         MVC   P+41(2),TEMPTIME+3                                               
         MVI   P+43,C':'                                                        
         MVC   P+44(2),TEMPTIME+5                                               
         MVC   P+48(7),=C'DSKADD='                                              
         GOTO1 =V(HEXOUT),DMCB,RVCHR,P+55,4                                     
         MVC   P+65(5),=C'DATE='                                                
         GOTO1 =V(DATCON),DMCB,(3,RDATE),(11,P+70)                              
         MVC   P+80(5),=C'USER='                                                
         LA    R2,P+85                                                          
         EDIT  RUSER,(4,(R2))                                                   
         GOTO1 =V(PRINTER)                                                      
         LH    R0,RECVHDR-4                                                     
         GOTO1 =V(PRNTBL),DMCB,0,RECVHDR,C'DUMP',(R0),FORMAT                    
         GOTO1 =V(PRINTER)                                                      
         B     IN2                                                              
*******************************************************************             
*                                                                               
ENDIN    CLOSE (RECVIN,)                                                        
         CLOSE (RECVOUT,)                                                       
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(20),=C'# OF RECORDS READ = '                                   
         EDIT  (R7),(8,P+23),ZERO=NOBLANK                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(23),=C'# OF RECORDS WRITTEN = '                                
         EDIT  (R8),(8,P+23),ZERO=NOBLANK                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         LTORG                                                                  
FORMAT   DC    C'1D'                                                            
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDIN                                             
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=8200,            X        
               BLKSIZE=8204,MACRF=PM                                            
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
KLEN     DS    F                                                                
LKLEN    DS    F                                                                
HKLEN    DS    F                                                                
DSKADR   DS    F                                                                
SYSINP   DS    F                                                                
TEMPTIME DS    2F                                                               
LTIME    DS    PL4                                                              
HTIME    DS    PL4                                                              
TRANAMT  DS    PL6                                                              
WORK     DS    XL64                                                             
USER     DS    XL2                                                              
TRNUM    DS    XL2                                                              
PROGNUM  DS    XL1                                                              
DALEN    DS    XL1                                                              
SILEN    DS    XL1                                                              
FLAG     DS    XL1                                                              
FKEY     DS    CL40                                                             
         ORG   FKEY                                                             
FLKEY    DS    CL40                                                             
FHKEY    DS    CL40                                                             
FILETBL  DS    XL30                                                             
SCANBOX  DS    CL300                                                            
CARD     DS    CL80                                                             
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
RDATA    DS    9000C                                                            
RDATALNQ EQU   2000                                                             
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE ACGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016DDREFUSES 10/19/94'                                      
         END                                                                    
