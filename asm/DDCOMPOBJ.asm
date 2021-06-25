*          DATA SET DDCOMPOBJ  AT LEVEL 004 AS OF 05/01/02                      
*PHASE COMPOBJ,*                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
         TITLE 'COMPARE OBJECT MODULES'                                         
COMPOBJ  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE LWSX-LWSD,**COBJ**,SAVEREGS,RA,RR=R2,CLEAR=YES                   
*                                                                               
         USING LWSD,RC             MAP  WORKING   STORAGE                       
         ST    R2,RELO                                                          
*                                                                               
         L     R9,CPRINT                                                        
         USING DPRINT,R9                                                        
*                                                                               
         LA    RE,COMPOBJ          SET  FOR  STXITER                            
         L     RF,STXITER                                                       
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 STXITER,DMCB,DUB                                                 
*                                                                               
         LH    RE,=Y(MAPO-LWSD)    INITIALIZE     OLDMAP                        
         AR    RE,RC                                                            
         ST    RE,OLDMAP                                                        
*                                                                               
         LH    RE,=Y(MAPN-LWSD)    INITIALIZE     NEWMAP                        
         AR    RE,RC                                                            
         ST    RE,NEWMAP                                                        
*                                                                               
         L     RE,=A(K*K)          INITIALIZE     LENGTH                        
         ST    RE,LENGTH                                                        
         EJECT ,                                                                
***********************************************************************         
* READ CARDS                                                          *         
***********************************************************************         
         SPACE 1                                                                
CARDIN   DS    0H                                                               
         GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   =C'/*',CARD         END  OF   INPUT ?                            
         BE    CARDIN50            YES, FINISHED  READING   CARDS               
         MVC   P(80),CARD          PRINT     INPUT     CARD                     
         GOTO1 PRINTER                                                          
*                                                                               
         LA    R4,OLD              OLD= CARD                                    
         CLC   CARD(L'COLDEQ),COLDEQ                                            
         BE    CARDIN20                                                         
*                                                                               
         LA    R4,NEW              NEW= CARD                                    
         CLC   CARD(L'CNEWEQ),CNEWEQ                                            
         BE    CARDIN20                                                         
*                                                                               
         LA    R4,OLDBOOK          OLDB=     CARD                               
         CLC   CARD(L'COLDBEQ),COLDBEQ                                          
         BE    CARDIN30                                                         
*                                                                               
         LA    R4,NEWBOOK          NEW=      CARD                               
         CLC   CARD(L'CNEWBEQ),CNEWBEQ                                          
         BE    CARDIN30                                                         
*                                                                               
         DC    H'0'                                                             
*                                                                               
CARDIN20 DS    0H                  SAVE MODULE    NAME                          
         MVC   0(L'LDINAME,R4),CARD+L'COLDEQ                                    
         B     CARDIN                                                           
*                                                                               
CARDIN30 DS    0H                  SAVE BOOK      NAME                          
         MVC   0(L'LDIBOOK,R4),CARD+L'COLDBEQ                                   
         B     CARDIN                                                           
*                                                                               
CARDIN50 DS    0H                  CHECK     FOR  MISSING   CARDS               
         LA    R2,L'COLDEQ         OLD= MISSING ?                               
         LA    R3,COLDEQ                                                        
         CLC   OLD,SPACES                                                       
         BNH   MISSING             YES, ERROR                                   
*                                                                               
         LA    R2,L'CNEWEQ         NEW= MISSING ?                               
         LA    R3,CNEWEQ                                                        
         CLC   NEW,SPACES                                                       
         BNH   MISSING             YES, ERROR                                   
*                                                                               
         CLC   OLDBOOK,SPACES      OLDB=     MISSING ?                          
         BNH   CARDIN60            NO,  CONTINUE                                
         LA    R2,L'CNEWBEQ        NEWB=     MISSING ?                          
         LA    R3,CNEWBEQ                                                       
         CLC   NEWBOOK,SPACES      YES, ERROR                                   
         BNH   MISSING                                                          
         B     CARDIN70                                                         
*                                                                               
CARDIN60 DS    0H                  NO   OLDB=                                   
         LA    R2,L'COLDBEQ                                                     
         LA    R3,COLDBEQ                                                       
         CLC   NEWBOOK,SPACES      NEWB=      MISSING ?                         
         BH    MISSING             NO,  ERROR                                   
*                                                                               
CARDIN70 DS    0H                  PROCESS   THE  DATA                          
         LA    R2,OLD                                                           
         BAS   RE,LOAD             LOAD OLD  PHASE                              
*                                                                               
         LA    R2,NEW                                                           
         BAS   RE,LOAD             LOAD NEW  PHASE                              
*                                                                               
         BAS   RE,PMAP             PRINT     SOURCE    MAP                      
         BAS   RE,COMP             COMPARE                                      
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
*                                                                               
*&&DO                                                                           
         LA    R0,X'EEE'           FOR  DUMP                                    
         DC    H'0'                DUMP                                         
*&&                                                                             
         EJECT ,                                                                
***********************************************************************         
* LOAD OBJECT CODE                                                    *         
*  BUILD SOURCE MAP                                                   *         
*   R2 = A(TABLE) FOR OLD OR NEW                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING LOADINFO,R2                                                      
         SPACE 1                                                                
LOAD     NTR1 ,                                                                 
         MVC   DUB,0(R2)                                                        
         GOTO1 LOADER,DMCB,DUB,0,0 GET  LENGTH                                  
         OC    DMCB+4(4),DMCB+4    WAS  LOAD OK ?                               
         BZ    MODNFND             NO,  ERROR                                   
*                                                                               
         ICM   R0,15,0(R1)         SAVE LENGTH    IN   R0                       
         AH    R0,=H'100'          GET  A    LITTLE    EXTRA                    
         GOTO1 LOADER,DMCB,DUB,X'FFFFFFFF',0     DELETE                         
*                                                                               
         GETMAIN R,LV=(0)          GET  SPACE     FOR  MODULE                   
         LTR   RF,RF               SPACE     GOTTEN ?                           
         BZ    *+6                 YES, CONTINUE                                
         DC    H'0'                                                             
*                                                                               
         LR    R0,R1               SAVE LOAD MOD  ADDRESS                       
         ST    R0,LDIADDR               IN   TABLE                              
*                                  LOAD THE  MODULE                             
         GOTO1 LOADER,DMCB,DUB,(R0),(C'A',(R0))                                 
         OC    DMCB+4(4),DMCB+4    WAS  LOAD OK ?                               
         BZ    MODNFND             NO,  ERROR                                   
*                                  LOAD THE  MODULE                             
         MVC   LDILEN,0(R1)        SAVE THE  LENGTH                             
         L     RE,LDIADDR          START     ADDRESS                            
         A     RE,LDILEN           PLUS      LENGTH                             
         BCTR  RE,0                MINUS     ONE                                
         ST    RE,LDIENDAD         EQUALS    END  OF   MODULE                   
*                                                                               
         USING MAPID,R1                                                         
         L     R1,LDIMAP           ->   LOAD MOD  MAP                           
         SR    RF,RF                                                            
         L     R3,LDIADDR          ->   LOAD MOD  START                         
         L     R4,LDIENDAD         ->   LOAD MOD  END                           
         LR    R5,R3               ->   BOOK START                              
*                                                                               
LOAD10   DS    0H                  FIND END  OF   MODULE                        
         CLC   MAPBOOKC-MAPBOOKC(L'MAPBOOKC,R3),=C'BOOK='                       
         BNE   LOAD20              NO,  NOT  STD  MAP  IDENTIFICATION           
         CLC   MAPLEVC-MAPBOOKC(L'MAPLEVC,R3),=C' LEVEL='                       
         BNE   LOAD20              NO,  NOT  STD  MAP  IDENTIFICATION           
         CLC   MAPDATEC-MAPBOOKC(L'MAPBOOKC,R3),=C' DATE='                      
         BNE   LOAD20              NO,  NOT  STD  MAP  IDENTIFICATION           
*                                  SAVE STD  MAP  IDENTIFICATION                
         MVC   MAPBOOKC(MAPIDLQ1),0(R3)                                         
         ST    R5,MAPBKSTA         SAVE BOOK      START      ADDRESS            
         ST    R3,MAPBKIDS         SAVE BOOK ID   START      ADDRESS            
         LA    R3,MAPIDLQ1(,R3)    ->   END  OF   BOOK                          
         LR    RE,R3               CALCULATE THIS BOOK LENGTH                   
         SR    RE,R5                    END       OF   BOOK MINUS               
         ST    RE,MAPBKLEN              START     OF   BOOK                     
*                                                                               
         LA    R1,MAPIDLNQ(,R1)    ->   NEXT MAP  ADDRESS                       
*                                                                               
*                                  GET  NEXT BOOK ADDRESS                       
         LA    R3,7(,R3)                ROUNDED   TO   DOUBLE     WORD          
         N     R3,=X'FFFFFFF8'               BOUNDARY                           
         LR    R5,R3               SAVE NEXT BOOK START     ADDRESS             
         BCTR  R3,0                BACK UP   1    BYTE                          
         LA    RF,1(,RF)           KEEP COUNT     OF   BOOKS                    
         CH    RF,=Y(MAPMX)        TOO  MANY BOOKS ?                            
         BNH   *+6                 NO,  SKIP                                    
         DC    H'0'                MAP  TABLE     FULL                          
*                                                                               
LOAD20   DS    0H                  GET  NEXT CHAR                               
         LA    R3,1(,R3)           ->   NEXT BYTE IN   MODULE                   
         CR    R3,R4               ANY  MORE DATA IN   MODULE ?                 
         BL    LOAD10              YES, CONTINUE                                
         B     XIT                 NO,  RETURN    TO   CALLER                   
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
* PRINT SOURCE MAP                                                    *         
***********************************************************************         
         SPACE 1                                                                
OLDM     USING MAPID,R2                                                         
NEWM     USING MAPID,R3                                                         
         SPACE 1                                                                
PMAP     NTR1  ,                                                                
         GOTO1 PRINTER                                                          
         MVC   P+1(L'COLD),COLD                                                 
         MVC   P+51(L'CNEW),CNEW                                                
         GOTO1 PRINTER                                                          
         MVC   P+1(L'COLD),DASHES                                               
         MVC   P+51(L'CNEW),DASHES                                              
         GOTO1 PRINTER                                                          
         L     R2,OLDMAP           ->   OLD  MOD  MAP                           
         L     R3,NEWMAP           ->   NEW  MOD  MAP                           
*                                                                               
PMAP10   DS    0H                  TEST ANY  OLD  BOOK TO   PRINT               
         OC    OLDM.MAPBKSTA(MAPIDLNQ),OLDM.MAPBKSTA                            
         BZ    PMAP20                                                           
         MVC   P+1(MAPIDLQ1),OLDM.MAPBOOKC                                      
         LA    R2,MAPIDLNQ(,R2)                                                 
*                                                                               
PMAP20   DS    0H                  TEST ANY  NEW  BOOK TO   PRINT               
         OC    NEWM.MAPBKSTA(MAPIDLNQ),NEWM.MAPBKSTA                            
         BZ    PMAP30                                                           
         MVC   P+51(MAPIDLQ1),NEWM.MAPBOOKC                                     
         LA    R3,MAPIDLNQ(,R3)                                                 
*                                                                               
PMAP30   DS    0H                  TEST END  OF   BOTH TABLES                   
         CLC   P,SPACES                                                         
         BE    PMAP40                                                           
         GOTO1 PRINTER                                                          
         B     PMAP10                                                           
*                                                                               
PMAP40   DS    0H                  SKIP LINE AFTER     ALL  BOOKS               
         GOTO1 PRINTER                                                          
         B     XIT                 RETURN    TO   CALLER                        
         DROP  OLDM,NEWM                                                        
         EJECT ,                                                                
***********************************************************************         
* COMPARE THE CODE                                                    *         
***********************************************************************         
         SPACE 1                                                                
COMP     NTR1  ,                                                                
         L     RE,NEWAD            NEW  MODULE    ADDR MINUS                    
         S     RE,OLDAD            OLD  MODULE    ADDR                          
         ST    RE,RELOF            SAVE RELOCATION     FACTOR                   
*                                                                               
         MVC   P+1(L'OLD),OLD      PRINT     NAMES     AND  LENGTHS             
         GOTO1 HEXOUT,DMCB,OLDLN,P+11,4,0,0                                     
*                                                                               
         MVC   P+51(L'NEW),NEW                                                  
         GOTO1 HEXOUT,DMCB,NEWLN,P+61,4,0,0                                     
         GOTO1 PRINTER                                                          
*                                                                               
         L     R3,OLDLN            OLD  MODULE    LENGTH                        
         L     R4,NEWLN            NEW  MODULE    LENGTH                        
         L     R5,OLDAD            ->   OLD  MODULE                             
         L     R6,NEWAD            ->   NEW  MODULE                             
*                                                                               
         MVI   ANYBOOKS,NO         ASSUME    NO   BOOKS                         
         OC    OLDBOOK,OLDBOOK     ANY  OLD  BOOK NAME ?                        
         BZ    COMP150             NO,  SKIP                                    
         MVC   P+1(L'OLDBOOK),OLDBOOK                                           
         L     R7,OLDMAP           ->   OLD  MOD  MAP                           
*                                                                               
         USING MAPID,R7                                                         
COMP050  DS    0H                  FIND OLD  BOOK                               
         CLC   OLDBOOK,MAPBOOK     SAME BOOK ?                                  
         BE    COMP100             YES, PROCESS                                 
         LA    R7,MAPIDLNQ(,R7)    ->   NEXT BOOK                               
         OC    MAPBKSTA,MAPBKSTA   ANY  MORE BOOKS                              
         BNZ   COMP050             YES, LOOP                                    
*                                                                               
*                                  BOOK NOT  FOUND                              
         MVC   P+12(L'BNOTFND),BNOTFND                                          
         GOTO1 PRINTER                                                          
         B     XIT                 STOP RUN                                     
*                                                                               
COMP100  DS    0H                  USE  OLD  BOOK                               
         MVI   ANYBOOKS,YES        FOUND     A    BOOK                          
         GOTO1 HEXOUT,DMCB,MAPBKLEN,P+11,4,0,0                                  
         L     R3,MAPBKIDS         OLD  BOOK ID   START                         
         L     R5,MAPBKSTA         OLD  BOOK START                              
         SR    R3,R5               LENGTH    TO   ID   START                    
*                                                                               
COMP150  DS    0H                                                               
         OC    NEWBOOK,NEWBOOK     ANY  NEW  BOOK NAME ?                        
         BZ    COMP300             NO,  SKIP                                    
         MVC   P+51(L'NEWBOOK),NEWBOOK                                          
         L     R7,NEWMAP           ->   NEW  MOD  MAP                           
*                                                                               
COMP200  DS    0H                  FIND NEW  BOOK                               
         CLC   NEWBOOK,MAPBOOK     SAME BOOK ?                                  
         BE    COMP250             YES, PROCESS                                 
         LA    R7,MAPIDLNQ(,R7)    ->   NEXT BOOK                               
         OC    MAPBKSTA,MAPBKSTA   ANY  MORE BOOKS                              
         BNZ   COMP200             YES, LOOP                                    
*                                                                               
*                                  BOOK NOT  FOUND                              
         MVC   P+62(L'BNOTFND),BNOTFND                                          
         GOTO1 PRINTER                                                          
         B     XIT                 STOP RUN                                     
*                                                                               
COMP250  DS    0H                  USE  NEW  BOOK                               
         MVI   ANYBOOKS,YES        FOUND     A    BOOK                          
         GOTO1 HEXOUT,DMCB,MAPBKLEN,P+61,4,0,0                                  
         L     R4,MAPBKIDS         NEW  BOOK ID   START                         
         L     R6,MAPBKSTA         NEW  BOOK START                              
         SR    R4,R6               LENGTH    TO   ID   START                    
         DROP  R7                                                               
*                                                                               
COMP300  DS    0H                  END  BOOK LOGIC                              
         CLI   ANYBOOKS,YES        ANY  BOOK FOUND ?                            
         BNE   COMP350             NO,  SKIP                                    
         GOTO1 PRINTER             PRINT     BOOK LINE                          
*                                                                               
COMP350  DS    0H                  **** DO   THE  COMPARES                      
         CLC   0(1,R5),0(R6)       OLD  MOD  CHAR =    NEW  MOD ?               
         BNE   COMP400             NO,  PROCESS   DIFFERENCES                   
         LA    R5,1(,R5)           ->   NEXT OLD  MOD  BYTE                     
         LA    R6,1(,R6)           ->   NEXT NEW  MOD  BYTE                     
         BCTR  R3,0                NUM  OF   BYTES     LEFT                     
         BCTR  R4,0                NUM  OF   BYTES     LEFT                     
         LTR   R3,R3               ANY  BYTES     LEFT ?                        
         BZ    COMPX               NO,  EXIT                                    
         LTR   R4,R4               ANY  BYTES     LEFT ?                        
         BZ    COMPX               NO,  EXIT                                    
         B     COMP350             LOOP FOR  NEXT BYTE                          
*                                                                               
COMP400  DS    0H                  TEST FOUR BYTE ADCON                         
         ICM   RF,15,0(R6)         RF=  NEW  DATA                               
         ICM   RE,15,0(R5)         RE=  OLD  DATA                               
         SR    RF,RE               GET  DIFFERENCE                              
         C     RF,RELOF            SAME AS   RELO FACTOR ?                      
         BNE   COMP450             NO,  NEXT CHECK     FOR  EQUIVALENT          
         LA    R5,4(,R5)           ->   NEXT OLD  MOD  BYTE                     
         LA    R6,4(,R6)           ->   NEXT NEW  MOD  BYTE                     
         SH    R3,=H'4'            NUM  OF   BYTES     LEFT                     
         SH    R4,=H'4'            NUM  OF   BYTES     LEFT                     
         BNP   COMPX               NONE,     EXIT                               
         LTR   R3,R3               ANY  BYTES     LEFT ?                        
         BNP   COMPX               NO,  EXIT                                    
         B     COMP350             YES, TEST NEXT BYTE                          
*                                                                               
COMP450  DS    0H                  TEST 3    BYTE ADCON                         
         SR    RF,RF                                                            
         ICM   RF,7,0(R6)          RF=  NEW  DATA                               
         SR    RE,RE                                                            
         ICM   RE,7,0(R5)          RE=  OLD  DATA                               
         SR    RF,RE               GET  DIFFERENCE                              
         C     RF,RELOF            SAME AS   RELO FACTOR ?                      
         BNE   COMP500             NO,  PRINT     DIFFERENCE                    
         LA    R5,3(,R5)           ->   NEXT OLD  MOD  BYTE                     
         LA    R6,3(,R6)           ->   NEXT NEW  MOD  BYTE                     
         SH    R3,=H'3'            NUM  OF   BYTES     LEFT                     
         SH    R4,=H'3'            NUM  OF   BYTES     LEFT                     
         BNP   COMPX               NONE,     EXIT                               
         LTR   R3,R3               ANY  BYTES     LEFT ?                        
         BNP   COMPX               NO,  EXIT                                    
         B     COMP350             YES, TEST NEXT BYTE                          
*                                                                               
COMP500  DS    0H                  TEST OF   3    BYTE ADCON                    
         LR    RF,R6                    WHEN PREV BYTE IS   USED                
         BCTR  RF,0                                                             
         SR    R0,R0                                                            
         ICM   R0,7,0(RF)          RF=  NEW  DATA                               
         LR    RF,R5                                                            
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         ICM   RE,7,0(RF)          RE=  OLD  DATA                               
         SR    R0,RE               GET  DIFFERENCE                              
         C     R0,RELOF            SAME AS   RELO FACTOR ?                      
         BNE   COMP550             NO,  PRINT     DIFFERENCE                    
         LA    R5,2(,R5)           ->   NEXT OLD  MOD  BYTE                     
         LA    R6,2(,R6)           ->   NEXT NEW  MOD  BYTE                     
         SH    R3,=H'2'            NUM  OF   BYTES     LEFT                     
         SH    R4,=H'2'            NUM  OF   BYTES     LEFT                     
         BNP   COMPX               NONE,     EXIT                               
         LTR   R3,R3               ANY  BYTES     LEFT ?                        
         BNP   COMPX               NO,  EXIT                                    
         B     COMP350             YES, TEST NEXT BYTE                          
*                                                                               
COMP550  DS    0H                  **** NOT  A    MATCH                         
         GOTO1 PRINTER             SKIP ONE  LINE ON   PRINTOUT                 
*                                                                               
*                                  CALC NUM  OF   BYTES     TO   PRINT          
         LA    R2,1                AT   LEAST     ONE  BYTE                     
*                                                                               
COMP600  DS    0H                  FIND NUM  BYTES     TO   PRINT               
         LR    RE,R5               GET  OLD  ADDR                               
         AR    RE,R2               PLUS COUNT                                   
         LR    RF,R6               GET  NEW  ADDR                               
         AR    RF,R2               PLUS COUNT                                   
         LA    R1,19               MAX  BYTES     -1   TO   DISPLAY             
         SR    R1,R2               NUM  OF   BYTES     TO   COMPARE             
         EXCLC R1,0(RE),0(RF)      REST OF   LINE THE  SAME ?                   
         BE    COMP650             YES, GOT  NUMBER                             
         LA    R2,1(,R2)           NEED ONE  MORE BYTE                          
         CH    R2,=H'20'           MAX  BYTES     TO   DISPLAY ?                
         BL    COMP600             TRY  NEXT BYTE                               
*                                                                               
COMP650  DS    0H                  PRINT     THE  DATA                          
         MVC   P+1(L'COLD),COLD                                                 
         LR    RE,R5               GET  OLD  ADDR                               
         L     RF,OLDAD            GET  OLD  MOD  START     ADDRESS             
         SR    RE,RF               GET  DISPLACEMENT   INTO MODULE              
         ST    RE,FULL             SAVE DISPLACEMENT                            
         GOTO1 HEXOUT,DMCB,FULL+1,P+5,3,0,0                                     
         GOTO1 HEXOUT,DMCB,(R5),P+12,(R2),0,0                                   
         GOTO1 PRINTER                                                          
*                                                                               
         MVC   P+1(L'CNEW),CNEW                                                 
         LR    RE,R6               GET  NEW  ADDR                               
         L     RF,NEWAD            GET  NEW  MOD  START     ADDRESS             
         SR    RE,RF               GET  DISPLACEMENT   INTO MODULE              
         ST    RE,FULL             SAVE DISPLACEMENT                            
         GOTO1 HEXOUT,DMCB,FULL+1,P+5,3,0,0                                     
         GOTO1 HEXOUT,DMCB,(R6),P+12,(R2),0,0                                   
         GOTO1 PRINTER                                                          
*                                                                               
         AR    R5,R2               ->   NEXT OLD  MOD  BYTE                     
         AR    R6,R2               ->   NEXT NEW  MOD  BYTE                     
         SR    R3,R2               NUM  OF   BYTES     LEFT                     
         SR    R4,R2               NUM  OF   BYTES     LEFT                     
         BNP   COMPX               NONE,     EXIT                               
         LTR   R3,R3               ANY  BYTES     LEFT ?                        
         BNP   COMPX               NO,  EXIT                                    
         CH    R2,=AL2(20-6)       ANY  INSTRUCTION    MATCHES ?                
         BNH   COMP350             YES, TEST NEXT BYTE                          
*                                                                               
*                                  SEE  IF   WE   CAN  FIND ADJUSTMENT          
         STM   R3,R6,SAVR3         FIND NEXT CODE THAT MATCHES                  
*                                                                               
         SR    R1,R1               NUM  BYTES     TO   SKIP                     
         LA    R2,K                MAX  BYTES     TO   CHECK                    
         CR    R2,R4               1K   BYTES     LEFT ?                        
         BNH   COMP700             YES, COMPARE                                 
         LR    R2,R4               USE  BYTES     LEFT                          
*                                                                               
COMP700  DS    0H                  COMPARE   1ST  OLD  BYTES    VARYING         
*                                            NEW  BYTES                         
         CLC   0(12,R5),0(R6)      SOME INSTRUCTIONS   MATCH ?                  
         BE    COMP800             YES, ADJUST                                  
         LA    R6,1(,R6)           TRY  NEXT BYTE OF   NEW  MODULE              
         LA    R1,1(,R1)           SKIP NUMBER                                  
         BCT   R2,COMP700                                                       
*                                  OLD  MOD  HAS  NO   INSERTED                 
*                                       INSTRUCTION                             
         LM    R3,R6,SAVR3         RESTORE   REGISTERS                          
         SR    R1,R1               NUM  BYTES     TO   SKIP                     
         LA    R2,K                MAX  BYTES     TO   CHECK                    
         CR    R2,R3               1K   BYTES     LEFT ?                        
         BNH   COMP750             YES, COMPARE                                 
         LR    R2,R3               USE  BYTES     LEFT                          
*                                                                               
COMP750  DS    0H                  COMPARE   1ST  NEW  BYTES    VARYING         
*                                            OLD  BYTES                         
         CLC   0(12,R6),0(R5)      SOME INSTRUCTIONS   MATCH ?                  
         BE    COMP900             YES, ADJUST                                  
         LA    R5,1(,R5)           TRY  NEXT BYTE OF   OLD  MODULE              
         LA    R1,1(,R1)           SKIP NUMBER                                  
         BCT   R2,COMP750                                                       
*                                  NEW  MOD  HAS  NO   INSERTED                 
*                                       INSTRUCTION                             
         LM    R3,R6,SAVR3         RESTORE   REGISTERS                          
         B     COMP350             TRY  NEXT SET                                
*                                                                               
*                                  SOME BYTES     SKIPPED                       
COMP800  DS    0H                       IN   NEW  MODULE                        
         LTR   R1,R1               ANY  BYTES     SKIPPED ?                     
         BZ    COMP850             NO,  CONTINUE                                
         STH   R1,HALF             SAVE REGISTER                                
*                                  XXX  BYTES     SKIPPED                       
*                                       IN   NEW  MODULE                        
         GOTO1 HEXOUT,DMCB,HALF,P+1,2,0,0                                       
         MVI   P+1,C' '                                                         
         MVC   P+6(L'NSKIPN),NSKIPN                                             
         GOTO1 PRINTER                                                          
         LH    R1,HALF                                                          
*                                                                               
COMP850  DS    0H                  FIX  UP   REGS FOR  SKIP                     
         LM    R3,R6,SAVR3         RESTORE   REGISTERS                          
         AR    R6,R1               SKIP BYTES     IN   NEW  MODULE              
         SR    R4,R1               ADJUST    BYTES     LEFT                     
         LTR   R4,R4               ANY  BYTES     LEFT ?                        
         BH    COMP350             YES, CONTINUE                                
         B     COMPX               NO,  SHOULD    NOT  OCCUR                    
*                                                                               
*                                  SOME BYTES     SKIPPED                       
COMP900  DS    0H                       IN   OLD  MODULE                        
         LTR   R1,R1               ANY  BYTES     SKIPPED ?                     
         BZ    COMP950             NO,  CONTINUE                                
         STH   R1,HALF             SAVE REGISTER                                
*                                  XXX  BYTES     SKIPPED                       
*                                       IN   OLD  MODULE                        
         GOTO1 HEXOUT,DMCB,HALF,P+1,2,0,0                                       
         MVI   P+1,C' '                                                         
         MVC   P+6(L'NSKIPO),NSKIPO                                             
         GOTO1 PRINTER                                                          
         LH    R1,HALF                                                          
*                                                                               
COMP950  DS    0H                  FIX  UP   REGS FOR  SKIP                     
         LM    R3,R6,SAVR3         RESTORE   REGISTERS                          
         AR    R5,R1               SKIP BYTES     IN   OLD  MODULE              
         SR    R3,R1               ADJUST    BYTES     LEFT                     
         LTR   R3,R3               ANY  BYTES     LEFT ?                        
         BH    COMP350             YES, CONTINUE                                
         B     COMPX               NO,  SHOULD    NOT  OCCUR                    
*                                                                               
COMPX    DS    0H                  **** END  OF   COMPARE                       
         GOTO1 PRINTER                                                          
         CR    R3,R4               BOTH VERSIONS  COMPLETE ?                    
         BE    COMPXX              YES, DONE                                    
*                                  WE   DID  NOT  COMPLETE                      
         MVC   P+1(L'MNOTOK),MNOTOK     BOTH VERSIONS                           
         GOTO1 PRINTER                                                          
*                                                                               
COMPXX   DS    0H                  SAY  COMPARE   COMPLETE                      
         MVC   P+1(L'MDONE),MDONE  COMPARE   OBJ  COMPLETE                      
         GOTO1 PRINTER                                                          
*                                                                               
XIT      DS    0H                  RETURN    TO   CALLER                        
         XIT1  ,                        I.E. EXIT                               
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* TERMINATION  ERROR MESSAGES                                         *         
***********************************************************************         
         SPACE 1                                                                
MISSING  DS    0H                                                               
         BCTR  R2,0                SUB  ONE  FOR  EXECUTE                       
         EXMVC R2,P+1,0(R3)        MOVE WORD                                    
         LA    R3,P+3(R2)          CARD MISSING                                 
         MVC   0(L'CARDMISS,R3),CARDMISS                                        
         GOTO1 PRINTER                                                          
         B     XIT                 EXIT                                         
*                                                                               
MODNFND  DS    0H                  MODULE    NOT  FOUND                         
         GOTO1 PRINTER                                                          
         MVC   P+1(L'DUB),DUB                                                   
         MVC   P+10(L'MNOTFND),MNOTFND                                          
         GOTO1 PRINTER                                                          
         XMOD1 2                        I.E. EXIT                               
         EJECT ,                                                                
***********************************************************************         
* EQUATES                                                             *         
***********************************************************************         
         SPACE 1                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
K        EQU   1024                                                             
*                                                                               
MAPMX    EQU   100                 MAX  NUM  OF   MAPS SUPPORTED                
         EJECT ,                                                                
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
CARDS    DC    V(CARDS)                                                         
HEXOUT   DC    V(HEXOUT)                                                        
LOADER   DC    V(LOADER)                                                        
PRINT    DC    V(PRINT)                                                         
PRINTER  DC    V(PRINTER)                                                       
SAVEREGS DC    V(REGSAVE)                                                       
STXITER  DC    V(STXITER)                                                       
CPRINT   DC    V(CPRINT)                                                        
         EJECT ,                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
CNEW     DC    C'NEW'                                                           
COLD     DC    C'OLD'                                                           
DASHES   DC    (L'CNEW)C'-'                                                     
*                                                                               
COLDEQ   DC    C'OLD='                                                          
CNEWEQ   DC    C'NEW='                                                          
COLDBEQ  DC    C'OLDB='                                                         
CNEWBEQ  DC    C'NEWB='                                                         
*                                                                               
CARDMISS DC    C'CARD MISSING'                                                  
*                                                                               
MNOTOK   DC    C'WE DID NOT COMPLETE BOTH VERSIONS'                             
MDONE    DC    C'COMPARE OBJ COMPLETED'                                         
*                                                                               
NSKIPO   DC    C'BYTES SKIPPED IN OLD MODULE'                                   
NSKIPN   DC    C'BYTES SKIPPED IN NEW MODULE'                                   
*                                                                               
BNOTFND  DC    C'BOOK NOT FOUND'                                                
MNOTFND  DC    C'MODULE NOT FOUND'                                              
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* WORK AREA                                                           *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
DUB      DS    D                                                                
*                                                                               
DMCB     DS    6F                                                               
FULL     DS    F                                                                
SAVR3    DS    F                                                                
SAVR4    DS    F                                                                
SAVR5    DS    F                                                                
SAVR6    DS    F                                                                
*                                                                               
RELO     DS    F                                                                
RELOF    DS    F                                                                
*                                                                               
LENGTH   DS    F                   INIT TO   K*K                                
*                                                                               
HALF     DS    H                                                                
*                                                                               
WORK     DS    XL64                                                             
CARD     DS    CL80                                                             
*                                                                               
ANYBOOKS DS    CL1                 ANY  BOOK FOUND (Y/N)                        
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0F                  OLD  MODULE    INFORMATION    TABLE          
OLD      DS    CL8                 LOADLIB   NAME                               
OLDLN    DS    F                   LENGTH    OF   CODE                          
OLDAD    DS    F                   ADDRESS   OF   LOAD MODULE                   
OLDEND   DS    F                   END  OF   LOAD MODULE                        
OLDMAP   DS    A                   ADDRESS   OF   MAPO                          
OLDBOOK  DS    CL10                OLD  BOOK NAME                               
         DS    CL2                 RESERVED                                     
*                                                                               
         DS    0F                  NEW  MODULE    INFORMATION    TABLE          
NEW      DS    CL8                 LOADLIB   NAME                               
NEWLN    DS    F                   LENGTH    OF   CODE                          
NEWAD    DS    F                   ADDRESS   OF   LOAD MODULE                   
NEWEND   DS    F                   END  OF   LOAD MODULE                        
NEWMAP   DS    A                   ADDRESS   OF   MAPN                          
NEWBOOK  DS    CL10                NEW  BOOK NAME                               
         DS    CL2                 RESERVED                                     
*                                                                               
         DS    0F                  BOUNDARY  ALIGNMENT                          
MAPO     DS    (MAPIDLNQ*MAPMX)X                                                
         DS    0F                  BOUNDARY  ALIGNMENT                          
MAPN     DS    (MAPIDLNQ*MAPMX)X                                                
*                                                                               
LWSX     EQU   *                                                                
         EJECT ,                                                                
***********************************************************************         
* LOAD MODULE INFORMATION TABLE DSECT                                 *         
***********************************************************************         
         SPACE 1                                                                
LOADINFO DSECT                                                                  
LDINAME  DS    CL8                 LOADLIB   NAME                               
LDILEN   DS    F                   LENGTH    OF   CODE                          
LDIADDR  DS    F                   ADDRESS   OF   LOAD MODULE                   
LDIENDAD DS    F                   END  OF   LOAD MODULE                        
LDIMAP   DS    A                   LOAD MODULE    MAP                           
LDIBOOK  DS    CL10                BOOK NAME                                    
         DS    CL2                 RESERVED                                     
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* STANDARD MODULE MAP IDENTIFICATION                                  *         
***********************************************************************         
         SPACE 1                                                                
MAPID    DSECT                                                                  
MAPBKSTA DS    A                   START    OF   BOOK                           
MAPBKIDS DS    A                   START    OF   BOOK ID                        
MAPBKLEN DS    F                   SIZE     OF   BOOK                           
*                                                                               
MAPBOOKC DC    CL5'BOOK='                                                       
MAPBOOK  DS    CL10                                                             
MAPLEVC  DC    CL7' LEVEL='                                                     
MAPLEVEL DS    CL3                                                              
MAPDATEC DC    CL6' DATE='                                                      
MAPDATE  DS    CL8                                                              
MAPIDLQ1 EQU   *-MAPBOOKC                                                       
         DS    CL1                 RESERVED                                     
MAPIDLNQ EQU   *-MAPID                                                          
         EJECT ,                                                                
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDCOMPOBJ 05/01/02'                                      
         END                                                                    
