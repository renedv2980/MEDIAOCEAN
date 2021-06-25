*          DATA SET RELFM20    AT LEVEL 004 AS OF 05/01/02                      
*          DATA SET RELFM20    AT LEVEL 015 AS OF 09/12/95                      
*PHASE T80420A,*                                                                
*INCLUDE UNBOOK                                                                 
         TITLE 'T80420 - RELFM20 - REP FILE DEMOS OVERLAY'                      
*                                                                               
*******************************************************************             
*                                                                 *             
*        RELFM20 --- REP INVENTORY DEMO RECORD DEMOS SCREEN       *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* OCT08/90 (MRR) --->DEMO COLUMN DATA IS NOW 8                    *             
*                   >ONE DECIMAL OUTPUT/INPUT                     *             
*                                                                 *             
* JUL23/91 (BU ) --->CHANGE FORMAT OF THE OVERRIDE ELEMENT TO NEW *             
*                    USE 'WORK3' RATHER THAN DUB TO SET IT UP     *             
*                                                                 *             
* OCT03/91 (BU ) --->FIX ERROR IN NEWEL4 TO INCREASE TEST FOR     *             
*                    'RATING' VS 'IMPS'                           *             
*                                                                 *             
* MAR25/92 (BU ) --->PERMIT A NON-HIT ON INVENTORY DATE TO FIND   *             
*                    THE RECORD WITH THE APPROPRIATE EFFECTIVE    *             
*                    DATE RANGE                                   *             
*                                                                 *             
* OCT20/93 (BU ) --->ADD NEW DEMOS TO DEMO PRESET GROUPS.         *             
*                                                                 *             
* FEB10/95 (BU ) --->ADDRESS OVERRIDE CARRYOVER PROBLEM           *             
*                                                                 *             
*                     ***  END TOMBSTONE  ***                     *             
*******************************************************************             
*                                                                               
T80420   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80420,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         EJECT                                                                  
         TM    INCBKSH+4,X'20'     IF BOOKS CHANGE                              
         BO    *+8                                                              
         MVI   BFMTSW,0            FORCE MODE TO DISPLAY                        
         SPACE 1                                                                
         TM    INCDEMOH+4,X'20'    IF DEMOS CHANGE                              
         BO    *+8                                                              
         MVI   BFMTSW,0            FORCE MODE TO DISPLAY                        
*              GET THE MARKET RECORD                                            
         SPACE 1                                                                
INV100   LA    R2,LFMKEYH                                                       
         FOUT  INCMRKTH,SPACES,25                                               
         SPACE 1                                                                
         GOTO1 VPAVSTA,DMCB,8(R2),WORK   GET STATION AND MEDIA                  
         MVC   INVSTA,WORK                                                      
         MVC   INVMED,WORK+5                                                    
         SPACE 1                                                                
         GOTO1 VMKTNME,DMCB,WORK,INCMRKTH                                       
         EJECT                                                                  
*              BUILD INVENTORY RECORD KEY                                       
         SPACE 1                                                                
         USING RINVD,R4                                                         
INV200   EQU   *                                                                
         L     R4,AIOAREA                                                       
         XC    RINVREC(34),RINVREC                                              
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,REPALPHA                                                
         MVC   RINVKSTA,INVSTA                                                  
         SPACE 1                                                                
         LA    R2,LFMKEYH                                                       
         LA    R3,131              X'83' - ASK FOR DISPLACEMENTS                
*                                    FOR UP TO 3 SUBFIELDS                      
         GOTO1 CSCANNER,DMCB,(R2),((R3),WORK2)                                  
         MVC   HALF(1),WORK2+68    SAVE DISPLACEMENT OF 3RD FIELD               
         LA    R3,INVERR                                                        
         XR    R5,R5                                                            
         IC    R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         SPACE 1                                                                
         CH    R5,=H'2'                                                         
         BL    ERROR               NO INV. IN KEY                               
         LA    R6,WORK2+32                                                      
*        BAS   R8,INVEDT           EDIT INV. NUMBER                             
*****************                                                               
         XC    RINVKINV,RINVKINV                                                
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RINVKINV(0),12(R6)                                               
         OC    RINVKINV(4),=4X'40'                                              
*****************                                                               
         SPACE 1                                                                
         CH    R5,=H'3'                                                         
         BL    INV230              NO DATE                                      
         LA    R6,32(R6)                                                        
         BAS   R8,DATEDT                                                        
         SPACE 1                                                                
INV230   BAS   RE,HEADER           GET THE HEADER RECORD                        
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         SPACE 1                                                                
         XC    INVTOBK,INVTOBK                                                  
         LA    R3,INVERR                                                        
         LA    R2,INCBKSH          BOOKS                                        
         CLI   5(R2),0                                                          
         BE    INV250                                                           
         GOTO1 VBOOKVAL,DMCB,(R2),(5,INVTOBK),CSCANNER                          
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         B     INV265                                                           
         SPACE 1                                                                
*              NO BOOK INPUT - GET 3 LATEST                                     
*                                                                               
INV250   XC    WORK3(250),WORK3                                                 
         LA    R6,WORK3                                                         
         XR    R3,R3                                                            
         MVC   KEY,REC2                                                         
         BAS   RE,HIGH             READ THE HEADER KEY                          
         SPACE 1                                                                
INV251   BAS   RE,SEQ                                                           
         CLC   KEY(24),KEYSAVE                                                  
         BNE   INV255              CHANGE OF INV. NO                            
         CLI   KEY+24,X'FF'                                                     
         BE    INV255              RATIONAL                                     
         LA    R3,1(R3)            COUNT ITEMS                                  
         MVC   0(3,R6),KEY+24      SOURCE AND BOOK                              
         LA    R6,3(R6)                                                         
         B     INV251                                                           
         SPACE 1                                                                
INV255   CH    R3,=H'2'                                                         
         BL    INV260                                                           
         GOTO1 VXSORT,DMCB,(1,WORK3),(R3),3,2,1                                 
         XC    WORK3+15(3),WORK3+15                                             
         SPACE 1                                                                
INV260   LA    R5,INVTOBK          CONVERT FROM KSRC TO BOOKVAL FORMAT          
         LA    R6,WORK3                                                         
         SPACE 1                                                                
INV262   OC    0(3,R6),0(R6)                                                    
         BZ    INV265                                                           
         LA    RE,SVCLST           CONVERSION TABLE                             
         SPACE 1                                                                
INV263   CLC   2(1,RE),0(R6)                                                    
         BE    INV263K                                                          
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   INV263                                                           
         DC    H'0'                                                             
INV263K  MVC   0(1,R5),3(RE)                                                    
         SPACE 1                                                                
INV264   MVC   1(2,R5),1(R6)       YEAR/MONTH                                   
         LA    R6,3(R6)                                                         
         LA    R5,3(R5)                                                         
         B     INV262                                                           
         SPACE 1                                                                
INV265   LA    R5,INVTOBK                                                       
         XC    WORK,WORK           SAVE DISK ADDRESSES IN WORK                  
         LA    R8,WORK             R8 POINTS TO D/A LIST                        
         SPACE 1                                                                
INV268   OC    0(3,R5),0(R5)                                                    
         BZ    INV270                                                           
         MVC   RINVKEY,REC2                                                     
         MVC   RINVKSRC(3),0(R5)   SOURCE/BOOK                                  
         SPACE 1                                                                
         LA    RE,SVCLST           CONVERSION TABLE                             
         SPACE 1                                                                
INV268C  CLC   3(1,RE),0(R5)       FROM BOOKVAL FORMAT TO KSRC FORMAT           
         BE    INV268G                                                          
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   INV268C                                                          
         DC    H'0'                                                             
INV268G  MVC   RINVKSRC,2(RE)                                                   
         SPACE 1                                                                
         MVC   KEY,RINVKEY                                                      
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+12                                                             
         LA    R3,ERRNF                                                         
         B     ERROR                                                            
         SPACE 1                                                                
         MVC   0(4,R8),KEY+28      SAVE D/A                                     
         BAS   RE,GETREC                                                        
         SPACE 1                                                                
INV269   LA    R5,3(R5)                                                         
         LA    R8,4(R8)            NEXT POSITION FOR A D/A                      
         B     INV268                                                           
         SPACE 1                                                                
INV270   MVC   INCBKS,SPACES                                                    
         OC    INVTOBK,INVTOBK                                                  
         BZ    INV271                                                           
         SPACE 1                                                                
         GOTO1 =V(UNBOOK),DMCB,(5,INVTOBK),(R2),0,0,RR=YES                      
INV271   FOUT  INCBKSH                                                          
         EJECT                                                                  
*              EDIT DEMO LIST                                                   
         SPACE 1                                                                
INV300   LA    R2,INCDEMOH                                                      
         CLI   5(R2),0        NO INPUT DEMOS                                    
         BNE   INV310                                                           
         MVC   INCDEMO,STDEM  STANDARD LIST TO SCREEN                           
         MVI   5(R2),40                                                         
         FOUT  INCDEMOH                                                         
         SPACE 1                                                                
INV310   XC    TRFIELD(40),TRFIELD                                              
         CLI   8(R2),C'/'          TEST FOR EDIT OVERRIDE PREFIX                
         BNE   INV320                                                           
         CLC   8(2,R2),=C'M='      TEST FOR DEMO MENU ELEMENTS                  
         BE    INV320                                                           
         MVI   BYTE,C'T'           SET DEFAULT MODIFIER                         
         MVC   BYTE2,9(R2)         SET KEY LETTER - 2 BYTE INPUT                
*                                                                               
         CLI   BYTE2,C'0'          TEST FOR A NUMBER                            
         BL    *+12                                                             
         CLI   BYTE2,C'9'          AND IF FOUND SEARCH FOR                      
         BNH   INV319              A DEMO NUMBER                                
*                                                                               
         CLI   5(R2),3             TEST IF MODIFIER IS THERE                    
         BH    INV325              DO REGULAR EDIT                              
         BL    INV315              NO- 2 BYTE INPUT                             
         LA    RE,MODLIST                                                       
         LA    R0,MODS                                                          
         CLC   9(1,R2),0(RE)       VALIDATE MODIFIER                            
         BE    INV312                                                           
         LA    RE,1(RE)                                                         
         BCT   R0,*-14                                                          
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 1                                                                
INV312   MVC   BYTE,0(RE)          EXTRACT ACTUAL MODIFIER                      
         MVC   BYTE2,10(R2)        EXTRACT KEY LETTER INPUT                     
         SPACE 1                                                                
INV315   MVC   TRFIELD(L'HOMES),HOMES                                           
         MVI   TOTWGHT,(L'HOMES-1)/3 SET NUMBER OF DEMOS                        
         CLI   BYTE2,C'H'          TEST FOR VALID KEY LETTER                    
         BE    INV318                                                           
*                                                                               
         MVC   TRFIELD(L'ADULTS),ADULTS                                         
         MVI   TOTWGHT,(L'ADULTS-1)/3                                           
         CLI   BYTE2,C'A'                                                       
         BE    INV318                                                           
*                                                                               
         MVC   TRFIELD(L'VIEWERS),VIEWERS                                       
         MVI   TOTWGHT,(L'VIEWERS-1)/3                                          
         CLI   BYTE2,C'V'                                                       
         BE    INV318                                                           
*                                                                               
         MVC   TRFIELD(L'MEN),MEN                                               
         MVI   TOTWGHT,(L'MEN-1)/3                                              
         CLI   BYTE2,C'M'                                                       
         BE    INV318                                                           
*                                                                               
         MVC   TRFIELD(L'WOMEN),WOMEN                                           
         MVI   TOTWGHT,(L'WOMEN-1)/3                                            
         CLI   BYTE2,C'W'                                                       
         BE    INV318                                                           
*                                                                               
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 1                                                                
INV318   CLI   BYTE,C'T'           TEST FOR IMPS                                
         BE    INV330              ALL DONE                                     
         LA    RE,TRFIELD          POINT TO DEMO LIST                           
         CLI   0(RE),X'FF'         TEST FOR END OF LIST                         
         BE    INV330                                                           
         MVC   1(1,RE),BYTE        REPLACE IMPS W USER MODIFIER                 
         LA    RE,3(RE)            NEXT DEMO                                    
         B     *-18                                                             
         SPACE 1                                                                
INV319   DS    0H                                                               
         CLI   5(R2),4             TEST FOR 3 DIGIT LIMIT                       
         BH    INV325                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                SUBTRACT FOR SLASH                           
         LR    R0,R1               SAVE LENGTH OF NUMBER                        
         LA    RE,9(R2)            POINT TO DATA AFTER SLASH                    
*                                                                               
         CLI   0(RE),C'0'          CHECK FOR NUMERIC DATA                       
         BL    INV325                                                           
         CLI   0(RE),C'9'                                                       
         BH    INV325                                                           
         LA    RE,1(RE)                                                         
         BCT   R1,*-20                                                          
*                                                                               
         LR    R1,R0               RESTORE LENGTH OF NUMBER                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,9(0,R2)         DEMO NUMBER                                  
         CVB   R0,DUB                                                           
         LA    R1,MODS             COUNTER OF MODIFIERS                         
         LA    RE,MODLIST          POINT AT MODIFIER TABLE                      
         XC    TRFIELD,TRFIELD                                                  
         LA    RF,TRFIELD          RF POINTS AT INTERNAL DEMO LIST              
*                                                                               
         MVC   1(1,RF),0(RE)       MODIFIER CODE                                
         STC   R0,2(RF)            DEMO NUMBER                                  
         LA    RE,1(RE)            NEXT MODIFIER                                
         LA    RF,3(RF)            NEXT DEMO ENTRY                              
         BCT   R1,*-18                                                          
         MVI   0(RF),X'FF'         END OF LIST                                  
         MVI   TOTWGHT,MODS        SET NUMBER OF DEMOS                          
         B     INV330                                                           
*                                                                               
         DROP  R4                                                               
*                                                                               
INV320   CLC   8(2,R2),=C'M='                                                   
         BNE   INV325              TEST FOR OVERRIDE PREFIX                     
*                                                                               
         CLI   5(R2),4             TEST FOR INPUT VALUE OF FOUR                 
         BE    *+12                                                             
         LA    R3,INVINP           INVALID DEMO MENU INPUT LENGTH               
         B     ERROR                                                            
*                                                                               
         LA    R4,KEY                                                           
         USING RDEMKEY,R4                                                       
*                                                                               
         XC    RDEMKEY,RDEMKEY     CLEAR RECORD FIELDS                          
*                                                                               
         MVI   RDEMKTYP,X'23'      BUILD KEY FOR DEMO MENU RECORD               
         MVC   RDEMKREP,REPALPHA                                                
         MVC   RDEMKDEM,10(R2)                                                  
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     CHECK TO SEE IF DEMO MENU WANTED             
*                                                                               
         BE    INV321                                                           
         LA    R3,INCDEM           INCORRECT DEMO MENU CODE                     
         B     ERROR               ERROR ROUTINE                                
*                                                                               
INV321   MVC   FULL,AIOAREA        SAVE OLD AIOAREA                             
         LA    R8,WORK3            SET ADDRESS OF NEW IOAREA                    
         LA    R8,1000(R8)                                                      
         ST    R8,AIOAREA                                                       
         USING RDEMREC,R8                                                       
*                                                                               
         BAS   RE,GETREC                                                        
         ZIC   R0,RDEMNUM                                                       
         CH    R0,=H'10'           MAXINUM NUMBER DEMOS ALLOWED                 
         BL    *+8                                                              
         LA    R0,10                                                            
         STC   R0,TOTWGHT          SAVE DEMO COUNT                              
*                                                                               
         GOTO1 VGETEL,DMCB,(X'02',AIOAREA),DMCB+8      DEMO ELEMENT             
         CLI   DMCB+8,X'FF'        BAD DEMO RECORD                              
         BNE   *+6                                                              
         DC    H'0'                DIE ON BAD DEMO RECORD                       
         L     R6,DMCB+8                                                        
         LA    R6,RDEMDEM-RDEMDEL(R6)                                           
         USING RDEMDEM,R6          DEMO ELEMENT DATA                            
         ZIC   R1,TOTWGHT                                                       
         MH    R1,=H'3'             TOTAL NUMBER BYTES TO BE MOVED              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRFIELD(0),0(R6)    MOVE TO DEMO OUTPUT FIELD                    
*                                                                               
         DROP  R6                                                               
         DROP  R8                                                               
         DROP  R4                                                               
*                                                                               
         MVC   AIOAREA,FULL        RESTORE OLD AIOAREA                          
*                                                                               
         B     INV330                                                           
*                                                                               
INV325   LA    R8,TRDEMOB                                                       
         USING DEMOD,R8                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         ST    R7,DBCOMFCS                                                      
         MVI   DBSELMED,C'T'                                                    
         SPACE 1                                                                
         GOTO1 CDEMOVAL,DMCB,(1,(R2)),(10,TRFIELD),DBLOCK                       
         LA    R3,INVERR                                                        
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         MVC   TOTWGHT,DMCB+4      SAVE DEMO COUNT                              
         DROP  R8                                                               
         SPACE 1                                                                
INV330   L     R4,AIOAREA          RESTORE R4                                   
         USING RINVD,R4                                                         
         ZIC   R1,TOTWGHT                                                       
         MH    R1,=H'3'            USE ZERO AS AN END OF LIST MARKER            
         LA    RE,TRFIELD(R1)                                                   
         MVI   0(RE),0                                                          
         SPACE 1                                                                
         CLI   BFMTSW,3            DISPLAY AFTER CHANGE?                        
         BE    INV340              YES                                          
         CLI   BFMTSW,0            DISPLAY                                      
         BNE   INVCHA                                                           
INV340   EQU   *                                                                
         BAS   RE,DISPLAY                                                       
         OC    INVTOBK(3),INVTOBK                                               
         BZ    EXXMOD              NO BOOKS                                     
         OI    LFMKEYH+4,X'20'                                                  
         OI    INCBKSH+4,X'20'                                                  
         OI    INCDEMOH+4,X'20'                                                 
         CLI   BFMTSW,3            DISPLAY AFTER CHANGE?                        
         BNE   EXXMOD              NO                                           
         MVI   BFMTSW,1            RESET TO CHANGE                              
         B     EXXMOD                                                           
         SPACE 1                                                                
INVCHA   CLI   BACT,C'C'           CHANGE                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,CHANGE                                                        
         MVI   BFMTSW,3            SET TO DISPLAY AFTER CHANGE                  
         B     INV200              GO BACK AND DISPLAY                          
         EJECT                                                                  
*              DISPLAY ROUTINE                                                  
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         GOTO1 VFOUTBLK,DMCB,INCEDTEH,INCLAST     CLEAR UNPROTECTED             
         MVC   INCEDTE,SPACES                                                   
         MVC   INCDAYS,SPACES                                                   
         MVC   INCTIME,SPACES                                                   
         MVC   INCPROG,SPACES                                                   
         SPACE 1                                                                
         FOUT  INCEDTEH                                                         
         FOUT  INCDAYSH                                                         
         FOUT  INCTIMEH                                                         
         FOUT  INCPROGH                                                         
         SPACE 1                                                                
         LA    R3,5                                                             
         LA    R2,INCBK1H                                                       
         XR    R1,R1                                                            
         SPACE 1                                                                
DISP05   MVC   8(8,R2),SPACES      CLEAR BOOK FILEDS                            
         FOUT  (R2)                                                             
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,DISP05                                                        
         SPACE 1                                                                
         LA    R4,REC2             HEADER IS IN REC2                            
         LA    R2,INCEDTE                                                       
         GOTO1 VDATCON,DMCB,(2,RINVPEFF),(5,0(R2))                              
         SPACE 1                                                                
         LA    R3,RINVPEFF+2                                                    
         OC    0(2,R3),0(R3)                                                    
         BZ    DISP07              NO END                                       
         LA    R2,8(R2)                                                         
         MVI   0(R2),C'-'                                                       
         GOTO1 VDATCON,DMCB,(2,0(R3)),(5,1(R2))                                 
         SPACE 1                                                                
DISP07   GOTO1 VGETEL,DMCB,(X'02',(R4)),DMCB+8      DAY/TIME ELEMENT            
         CLI   DMCB+8,X'FF'        BAD DEMO RECORD                              
         BNE   *+6                                                              
         DC    H'0'                NO DAY/TIME ELEMENT                          
         L     R3,DMCB+8                                                        
         GOTO1 VUNDAY,DMCB,2(R3),INCDAYS                                        
         GOTO1 VUNTIME,DMCB,3(R3),INCTIME                                       
         SPACE 1                                                                
         CLI   1(RA),C'*'          TEST FOR DDS TERMINAL                        
         BE    *+14                                                             
         MVC   INCPROG,RINVPROG    PROGRAM NAME FOR CLIENT TERMINAL             
         B     DISP09                                                           
         SPACE 1                                                                
         LA    R5,L'INCPROG/9      PUT OUT FIRST 3 D/A'S                        
         LA    R8,WORK             POINT TO D/A LIST                            
         LA    R6,INCPROG          POINT TO SCREEN FIELD                        
         SPACE 1                                                                
DISP08   OC    0(4,R8),0(R8)       TEST FOR E-O-L                               
         BZ    DISP09              DONE                                         
         GOTO1 CHEXOUT,DMCB,(R8),(R6),4,0                                       
         LA    R8,4(R8)            POINT TO NEXT D/A                            
         LA    R6,9(R6)            BUMP OUTPUT POINTER                          
         BCT   R5,DISP08                                                        
         SPACE 1                                                                
DISP09   BAS   RE,DCAT             FOUT OUT THE CATEGORY                        
         LA    R3,1                COLUMN NUMBER                                
         LA    R5,INVTOBK                                                       
         OC    0(3,R5),0(R5)                                                    
         BZ    EXXMOD              NO BOOKS TO DISPLAY                          
         LA    R2,INCBK1H                                                       
         SPACE 1                                                                
DISP10   BAS   RE,DISPDEM          DISPLAY DEMOS                                
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    R5,3(R5)            NEXT BOOK                                    
         OC    0(3,R5),0(R5)                                                    
         BZ    EXXMOD                                                           
         LA    R3,1(R3)                                                         
         CH    R3,=H'5'            MAX 5 BOOKS                                  
         BH    EXXMOD                                                           
         B     DISP10                                                           
         EJECT                                                                  
*              DISPLAY DEMO CATEGORY                                            
         SPACE 1                                                                
DCAT     NTR1                                                                   
         MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         MVC   VDEMOCON,DMCB                                                    
         LA    R4,TRFIELD          DEMO LIST FROM DEMVAL                        
         LA    R2,INCCATH                                                       
         LA    R3,10                                                            
*                                                                               
         LA    R8,TRDEMOB          INITIALIZE DBLOCK FOR DEMOCON                
         USING DEMOD,R8                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         ST    R7,DBCOMFCS                                                      
         MVI   DBSELMED,C'U'                                                    
         SPACE 1                                                                
DCAT1    MVC   8(8,R2),SPACES                                                   
         FOUT  (R2)                                                             
         OC    0(3,R4),0(R4)       NO MORE DEMOS                                
         BZ    DCAT2                                                            
         SPACE 1                                                                
         GOTO1 VDEMOCON,DMCB,(0,(R4)),(6,8(R2)),DBLOCK                          
         CLC   8(6,R2),=C'PHOMES'                                               
         BNE   *+10                                                             
         MVC   8(6,R2),=C'HUT   '                                               
         CLC   8(6,R2),=C'SHOMES'                                               
         BNE   *+10                                                             
         MVC   8(6,R2),=C'SHARE '                                               
         SPACE 1                                                                
DCAT2    BCT   R3,*+8                                                           
         B     EXXMOD                                                           
         SPACE 1                                                                
         LA    R4,3(R4)            NEXT DEMO                                    
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         TM    1(R2),X'20'         NEXT PROTECTED FIELD                         
         BNO   *-10                                                             
         B     DCAT1                                                            
         DROP  R8                                                               
         EJECT                                                                  
*              DISPLAY THE DEMOS                                                
         SPACE 1                                                                
DISPDEM  NTR1                                                                   
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   RINVKEY,REC2                                                     
         MVC   RINVKSRC(3),0(R5)   BOOK                                         
         SPACE 1                                                                
         LA    RE,SVCLST           CONVERSION TABLE                             
         SPACE 1                                                                
DD10     CLC   3(1,RE),0(R5)       FROM BOOKVAL FORMAT TO SRC FORMAT            
         BE    DD20                                                             
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   DD10                                                             
         DC    H'0'                                                             
DD20     MVC   RINVKSRC,2(RE)                                                   
         SPACE 1                                                                
DISPD1   BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETREC                                                        
         SPACE 1                                                                
         L     R4,AIOAREA                                                       
         GOTO1 =V(UNBOOK),DMCB,(1,(R5)),(R2),0,0,RR=YES                         
         SPACE 1                                                                
         LA    R2,INCCTH           CODE/TEXT LINE                               
         LR    R5,R3               COLUMN NUMBER                                
         BCTR  R5,0                                                             
         MH    R5,=H'2'                                                         
         XR    R1,R1                                                            
         SPACE 1                   FIND CODE FIELD                              
         LTR   R5,R5                                                            
         BZ    *+14                                                             
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R5,*-6                                                           
         SPACE 1                                                                
         USING RINVCEL,R6                                                       
         GOTO1 VGETEL,DMCB,(X'CD',AIOAREA),DMCB+8                               
         L     R6,DMCB+8                                                        
         CLI   DMCB,X'FF'                                                       
         BE    DISPD2                                                           
         SPACE 1                                                                
         MVC   8(2,R2),RINVCODE                                                 
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         EDIT  (B2,RINVCTXT),(4,8(R2)),ZERO=BLANK,ALIGN=LEFT                    
         SPACE 1                                                                
DISPD2   LA    R2,INCDATAH         DATA FIELD                                   
         LR    R5,R3                                                            
         BCTR  R5,0                                                             
         XR    R1,R1                                                            
         SPACE 1                                                                
         LTR   R5,R5                                                            
         BZ    *+14                                                             
         IC    R1,0(R2)            FIND FIRST DATA FIELD                        
         AR    R2,R1                                                            
         BCT   R5,*-6                                                           
         SPACE 1                                                                
         BAS   RE,DEMOUT           DISPLAY THE DEMOS                            
         B     EXXMOD                                                           
         EJECT                                                                  
*              DISPLAY THE DEMOS                                                
*              R2   A(FIRST DATA FIELD)                                         
*              TRFIELD HAS DEMO LIST - 3 BYTE RAVL FORMAT                       
*              WORK3 IS FILLED WITH VALUES BY DEMOUT                            
         SPACE 1                                                                
DEMOUT   NTR1                                                                   
         GOTO1 VGETEL,DMCB,(X'5E',AIOAREA),DMCB+8                               
         CLI   DMCB,X'FF'          TEST FOR UNCONVERTED INVENTORY               
         BNE   DEMOUT10            ITS OK                                       
         XC    DUB,DUB             BUILD A DUMMY BOOK ELEMENT                   
         MVC   DUB(2),=X'5E07'                                                  
         MVC   DUB+2(3),=C'PTN'                                                 
         MVC   DUB+5(2),=X'5202'                                                
         GOTO1 VADDELEM,DMCB,AIOAREA,DUB                                        
         SPACE 1                                                                
DEMOUT10 EQU   *                                                                
         XC    BYTE2,BYTE2         USE BYTE2 AS A COMBO FLAG                    
         GOTO1 VGETEL,DMCB,(X'CD',AIOAREA),DMCB+8                               
         CLI   DMCB,X'FF'                                                       
         BE    DEMOUT15                                                         
         USING RINVCEL,R6                                                       
         L     R6,DMCB+8                                                        
         TM    RINVCTYP,X'80'                                                   
         BNO   DEMOUT15                                                         
         MVI   BYTE2,C'Y'                                                       
         SPACE 1                                                                
DEMOUT15 EQU   *                                                                
         XC    TRBKLIST,TRBKLIST   MOVE DEMO LIST TO ANOTHER AREA               
         MVC   TRBKLIST,TRFIELD    AND INSERT END-OF-LIST MARKER.               
         LA    R5,TRBKLIST                                                      
         ZIC   R1,TOTWGHT                                                       
         MH    R1,=H'3'                                                         
         AR    R1,R5                                                            
         MVI   0(R1),X'FF'                                                      
         XC    WORK3(40),WORK3                                                  
         LA    R4,WORK3            POINT TO VALUE AREA                          
         LA    R6,TRDEMOB                                                       
         USING DEMOD,R6                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         L     RE,AIOAREA                                                       
         ST    RE,DBAREC                                                        
         LA    RE,34(RE)           POINT TO FIRST EL                            
         ST    RE,DBAQUART                                                      
         L     R7,ACOMFACS                                                      
         ST    R7,DBCOMFCS                                                      
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
*                                                                               
         SPACE                                                                  
         GOTO1 CDEMOUT,DMCB,(C'L',(R5)),(R6),(R4)                               
         LA    R8,10                                                            
         SPACE 1                                                                
DEMOUT20 EQU   *                                                                
         CLI   0(R5),X'FF'                                                      
         BE    EXXMOD              END-OF-LIST                                  
*                                                                               
         CLI   BYTE2,C'Y'                                                       
         BNE   DEMOUT25                                                         
         CLI   1(R5),C'S'                                                       
         BE    DEMOUT22                                                         
         CLI   1(R5),C'P'                                                       
         BNE   DEMOUT25                                                         
DEMOUT22 EQU   *                                                                
         MVC   8(7,R2),=C'   N   '                                              
         B     DEMOUT60                                                         
*                                                                               
DEMOUT25 EQU   *                                                                
         MVC   FULL,0(R4)                                                       
         EDIT  (B4,FULL),(7,8(R2)),1,ALIGN=LEFT,ZERO=BLANK                      
*                                                                               
         OI    4(R2),X'20'                                                      
         GOTO1 DEELEM,DMCB,(R5)    LOOK FOR 'DE' ELEMENT                        
         BZ    DEMOUT60            NOT FOUND                                    
         MVC   DUB(6),8(R2)                                                     
         MVI   8(R2),C'*'          INSERT STAR TO FLAG OVERRIDE VALUE           
         MVC   9(5,R2),DUB                                                      
         SPACE 1                                                                
DEMOUT60 BCT   R8,*+8                                                           
         B     EXXMOD                                                           
         SPACE 1                                                                
         LA    R5,3(R5)            NEXT DEMO                                    
         LA    R4,4(R4)            NEXT VALUE                                   
         LA    R3,6                GET NEXT LINE                                
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,*-6                                                           
         B     DEMOUT20                                                         
         DROP  R6                                                               
         EJECT                                                                  
*              ACTION IS CHANGE                                                 
         SPACE 1                                                                
CHANGE   NTR1                                                                   
         MVI   BYTE3,C'E'          FIRST TIME IS EDIT                           
CHANGE1  LA    R5,INVTOBK                                                       
         LA    R3,1                COLUMN                                       
         SPACE 1                                                                
CHANGE2  BAS   RE,BLDREC           BUILD A NEW RECORD IN REC                    
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         SPACE 1                                                                
         CLI   BYTE3,C'E'                                                       
         BE    *+8                 STILL EDIT - DON'T WRITE                     
         BAS   R8,FLADD                                                         
         SPACE 1                                                                
         LA    R5,3(R5)            NEXT BOOK                                    
         CLI   BYTE3,C'E'          IN CASE THEY INPUT                           
         BE    *+14                AND NO BOOK DISPLAYED                        
         OC    0(3,R5),0(R5)                                                    
         BZ    CHANGE3                                                          
         SPACE 1                                                                
         LA    R3,1(R3)            NEXT COLUMN                                  
         CH    R3,=H'5'            MAX 5 BOOKS                                  
         BH    CHANGE3                                                          
         B     CHANGE2                                                          
         SPACE 1                                                                
CHANGE3  CLI   BYTE3,C'W'          DID I DO THE WRITE                           
         BE    CHANGE4                                                          
         MVI   BYTE3,C'W'                                                       
         B     CHANGE1             NOW GO DO IT                                 
         SPACE 1                                                                
CHANGE4  LA    R4,REC                                                           
         XC    RINVKSRC(3),RINVKSRC                                             
         BAS   R8,FLCHA                                                         
         B     EXXMOD                                                           
         EJECT                                                                  
*              BUILD A NEW RECORD IN IOAREA                                     
*              R3  HAS COLUMN NUMBER                                            
*              R5  A(BOOK)                                                      
*              TRFIELD HAS THE DEMO LIST                                        
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         CLI   BYTE3,C'E'                                                       
         BE    BLDREC1                                                          
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   RINVKEY,REC2                                                     
         MVC   RINVKSRC(3),0(R5)    BOOK                                        
         SPACE 1                                                                
         LA    RE,SVCLST           CONVERSION TABLE                             
         SPACE 1                                                                
BR10     CLC   3(1,RE),0(R5)       FROM BOOKVAL FORMAT TO SRC FORMAT            
         BE    BR20                                                             
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   BR10                                                             
         DC    H'0'                                                             
BR20     MVC   RINVKSRC,2(RE)                                                   
         SPACE 1                                                                
BLDREC0  BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,REC                                                           
         BAS   RE,GETREC                                                        
         SPACE 1                                                                
BLDREC1  LA    R2,INCCTH           CODE/TEXT LINE                               
         LR    R6,R3               COLUMN NUMBER                                
         BCTR  R6,0                                                             
         MH    R6,=H'2'                                                         
         XR    R1,R1                                                            
         SPACE 1                                                                
         LTR   R6,R6                                                            
         BZ    *+14                                                             
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R6,*-6                                                           
         SPACE 1                                                                
         USING RINVCEL,R6                                                       
         LA    R6,WORK                                                          
         XC    WORK,WORK                                                        
         GOTO1 VGETEL,DMCB,(X'CD',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    BLDREC2                                                          
         SPACE 1                                                                
         L     R6,DMCB+8                                                        
         XR    R1,R1                                                            
         IC    R1,RINVCLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),RINVCEL                                                  
         GOTO1 VDELELEM,DMCB,(X'CD',REC)                                        
         SPACE 1                                                                
BLDREC2  LA    R6,WORK                                                          
         MVC   RINVCCOD(2),=X'CD0A'                                             
         MVC   RINVCODE,8(R2)                                                   
         OC    RINVCODE,SPACES                                                  
         SPACE 1                                                                
         CLI   5(R2),0                                                          
         BE    *+22                                                             
         OC    0(3,R5),0(R5)                                                    
         BNZ   *+12                                                             
         LA    R3,INVERR           INPUT A CODE                                 
         B     ERROR               NO BOOK DISPLAYED                            
         SPACE 1                                                                
         MVI   RINVCSET,C' '                                                    
         TM    0(R5),X'20'                                                      
         BNO   *+8                                                              
         MVI   RINVCSET,C'E'       ESTIMATED BOOK                               
         SPACE 1                                                                
         TM    0(R5),X'04'                                                      
         BNO   *+8                                                              
         MVI   RINVCSET,C'P'       PROJECTED BOOK                               
         SPACE 1                                                                
         TM    0(R5),X'02'                                                      
         BNO   *+8                                                              
         MVI   RINVCSET,C'S'       SPECIAL SURVEY BOOK                          
         SPACE 1                                                                
BLDREC4  XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1               TEXT                                         
         BAS   RE,PACK                                                          
         STH   R0,HALF                                                          
         MVC   RINVCTXT,HALF                                                    
         CLI   5(R2),0                                                          
         BE    *+28                                                             
         OC    0(3,R5),0(R5)                                                    
         BZ    *+10                                                             
         LTR   R0,R0                                                            
         BNZ   *+12                                                             
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 1                                                                
         CLI   BYTE3,C'E'                                                       
         BE    BLDREC5                                                          
         GOTO1 VADDELEM,DMCB,REC,WORK                                           
         SPACE 2                                                                
*              ADD THE DEMO ELEMENTS                                            
BLDREC5  LA    R2,INCDATAH         DATE FIELD                                   
         LR    R6,R3               COLUMN NUMBER                                
         BCTR  R6,0                                                             
         XR    R1,R1                                                            
         SPACE 1                                                                
         LTR   R6,R6                                                            
         BZ    *+14                                                             
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R6,*-6                                                           
         SPACE 1                                                                
         BAS   RE,DEDIT            EDIT THE INPUT DEMOS- GET VALUE              
         CLI   ERRAREA,0           IN WORK                                      
         BNE   EXXMOD                                                           
         CLI   BYTE3,C'E'                                                       
         BE    EXXMOD                                                           
         SPACE 1                                                                
         BAS   RE,NEWEL            BUILD OVERRIDE ELEMENTS                      
         B     EXXMOD                                                           
         EJECT                                                                  
*              EDIT THE INPUT DEMOS                                             
*              RETURNS VALUE IN 4BYTE FIELD  IN WORK -RELATIVE TO               
*              INPUT DEMO LIST                                                  
*              TRFIELD HAS THE DEMO LIST                                        
*              R2  A(FIRST DATA FIELD)                                          
         SPACE 1                                                                
DEDIT    NTR1                                                                   
*                                                                               
         LA    R6,TRFIELD                                                       
         XC    WORK,WORK                                                        
         LA    R7,WORK                                                          
         LA    R8,10                                                            
         SPACE 1                                                                
DEDIT1   EQU   *                                                                
         CLI   5(R2),0             NO INPUT                                     
         BE    DEDIT10                                                          
         OC    0(3,R5),0(R5)       INPUT BUT NO DEMO                            
         BZ    DEMERR              ON THIS LINE                                 
*                                                                               
         ZIC   R1,5(R2)            LENGTH                                       
         ST    R1,FULL                                                          
         LA    R3,8(R2)            DATA                                         
*                                                                               
         CLI   0(R3),C'*'                                                       
         BNE   DEDIT2                                                           
         SH    R1,=H'1'            STRIP OUT A LEAD STAR                        
         ST    R1,FULL                                                          
         BZ    DEDIT10             IGNORE A FIELD THAT ONLY HAS A STAR          
         STC   R1,5(R2)            ADJUST FIELD LENGTH                          
         MVC   0(7,R3),1(R3)                                                    
         MVI   7(R3),C' '                                                       
         NI    4(R2),X'FF'-X'20'   FOR OVERRIDES, TURN OFF PREVALID             
*                                                                               
DEDIT2   EQU   *                                                                
         CH    R1,=H'7'                                                         
         BH    DEMERR                                                           
         TM    4(R2),X'20'                                                      
         BO    DEDIT10             NOT CHANGED                                  
         CLI   1(R6),C'P'          PUTS                                         
         BE    DEDIT3              CAN BE CHANGED                               
         CLI   1(R6),C'R'          RATING                                       
         BE    DEDIT3                                                           
         CLI   1(R6),C'S'          SHARE                                        
         BE    DEDIT3                                                           
         CLI   1(R6),C'T'          TSA                                          
         BNE   DEMERR                                                           
*                                                                               
DEDIT3   EQU   *                                                                
         MVC   DMCB+4(4),FULL                                                   
         GOTO1 VCASHVAL,DMCB,(1,0(R3))                                          
         CLI   DMCB,X'FF'                                                       
         BE    DEMERR                                                           
         MVC   0(4,R7),DMCB+4                                                   
*                                                                               
DEDIT10  EQU   *                                                                
         LA    RF,6                GET NEXT LINE                                
DEDIT20  EQU   *                                                                
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   RF,DEDIT20                                                       
*                                                                               
         LA    R6,3(R6)                                                         
         LA    R7,4(R7)                                                         
         BCT   R8,DEDIT1           10 ON SCREEN                                 
         B     EXXMOD                                                           
         SPACE 1                                                                
DEMERR   LA    R3,INVERR                                                        
         B     ERROR                                                            
         EJECT                                                                  
*              BUILD OVERRIDE ELEMENTS - ADD THEM TO REC                        
*              WORK   HAS THE INPUT VALUE                                       
*              TRFIELD  HAS THE DEMO LIST                                       
*              WORK3  HAS THE BOOK VALUES                                       
         SPACE 1                                                                
NEWEL    NTR1                                                                   
         LA    R4,WORK                                                          
         LA    R5,TRFIELD                                                       
         XC    WORK3(80),WORK3     DUPLICATE LIST OF DEMOS IN                   
         LA    R3,WORK3            WORK3 FOR A DEMOUT CALL.                     
         ZIC   R1,TOTWGHT                                                       
         MH    R1,=H'3'                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK3(0),TRFIELD                                                 
         LA    R1,1(R1)                                                         
         AR    R1,R3                                                            
         MVI   0(R1),X'FF'                                                      
         SPACE                                                                  
* DELETE ANY OVERRIDE ELEMENTS FOR DEMOS IN THE LIST                            
*                                                                               
NEWEL1   EQU   *                                                                
         LA    R8,TRFIELD          POINT TO LIST                                
         ZIC   R2,TOTWGHT          COUNTER OF ENTRIES IN LIST                   
         L     R7,ACOMFACS                                                      
         PRINT GEN                                                              
NEWEL2   EQU   *                                                                
         PRINT GEN                                                              
         GOTO1 DEELEM,DMCB,(R8)    LOOK FOR 'DE' ELEMENT                        
         PRINT NOGEN                                                            
         BZ    NEWEL2A                                                          
*                                                                               
*    IF ELEMENT IS FOUND, THE ADDITIONAL SEARCH ARGUMENT (P3) IS                
*      SET IN THE 'DEELEM' ROUTINE FOR THE 'HELLO' CALL                         
*                                                                               
         GOTO1 CHELLO,DMCB,(C'D',=C'REPFILE '),(X'DE',REC)                      
         PRINT NOGEN                                                            
         SPACE                                                                  
         USING RINVD,R4                                                         
         LR    R0,R4               SAVE R4                                      
         LA    R4,REC              AND REPAIR THE RECORD LENGTH                 
         MVC   HALF,RINVLEN        AFTER HELLO CALL                             
         LH    R6,HALF                                                          
         BCTR  R6,0                                                             
         STCM  R6,3,RINVLEN                                                     
         LR    R4,R0               RESTORE R4                                   
NEWEL2A  EQU   *                                                                
         LA    R8,3(R8)            NEXT DEMO                                    
         BCT   R2,NEWEL2                                                        
         SPACE 2                                                                
         LA    R6,TRDEMOB          INITIALIZE BLOCK                             
         USING DEMOD,R6                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         LA    RE,REC                                                           
         ST    RE,DBAREC                                                        
         LA    RE,34(RE)                                                        
         ST    RE,DBAQUART                                                      
         ST    R7,DBCOMFCS                                                      
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
*                                                                               
         DROP  R6                                                               
         SPACE                                                                  
         GOTO1 CDEMOUT,DMCB,(C'L',(R3)),(R6),40(R3)                             
         SPACE                                                                  
NEWEL3   EQU   *                                                                
         ZIC   R2,TOTWGHT          COUNTER-ENTRIES IN LIST                      
         LA    R3,40(R3)           POINT TO BOOK VALUES                         
         SPACE 2                                                                
NEWEL4   EQU   *                                                                
         CLC   0(4,R4),0(R3)       SCREEN VALUE VS. BOOK VALUE                  
         BE    NEWEL6              NO OVERRIDE NEEDED IF SAME                   
         OC    0(4,R4),0(R4)       ZERO OR NO INPUT MEANS                       
         BZ    NEWEL6              ONLY DELETE CORRESPONDING OVERRIDE           
         SPACE                                                                  
         LA    R6,WORK3+100        SETUP SPACE FOR ELEMENT                      
         XC    0(12,R6),0(R6)                                                   
         MVC   0(2,R6),=X'DE0C'    INSERT ELEMENT CODE/LENGTH                   
         MVC   4(2,R6),1(R5)       TYPE AND CATEGORY                            
         MVI   7(R6),X'81'         FOR RATINGS: DECIMAL PRECISION               
         CLI   4(R6),C'R'          IS DEMO A RATING?                            
         BE    NEWEL4Q             YES                                          
         CLI   4(R6),C'P'          IS DEMO A RATING?                            
         BE    NEWEL4Q             YES                                          
         CLI   4(R6),C'S'          IS DEMO A RATING?                            
         BE    NEWEL4Q             YES                                          
         MVI   7(R6),X'42'         FOR IMPS: HUNDREDS                           
NEWEL4Q  EQU   *                                                                
         MVC   10(2,R6),2(R4)      SCREEN VALUE                                 
         GOTO1 VADDELEM,DMCB,REC,(R6)                                           
         SPACE 2                                                                
NEWEL6   EQU   *                                                                
         LA    R4,4(R4)            UPDATE SCREEN VALUE POINTER                  
         LA    R3,4(R3)            NEXT VALUE FROM RECORD                       
         LA    R5,3(R5)            NEXT DEMO                                    
         BCT   R2,NEWEL4                                                        
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*  THIS ROUTINE:                                                                
*    FIRST LOOKS FOR OVERRIDE (X'DE') ELEMENTS                                  
*    IF ELEMENT IS FOUND, THEN CHECKS TO SEE IF OVERRIDE IS FOR                 
*        CORRECT DEMOGRAPHIC.  OLD AND NEW FORMATS ARE CONSIDERED.              
*    IF CORRECT DEMOGRAPHIC IS FOUND, P3 IS SET UP FOR 'HELLO'                  
*        CALL TO CORRECTLY DELETE THE OLD OR THE NEW FORMAT                     
*    NOTE:  THE TWO BYTES FOLLOWING THE ELEMENT LENGTH OF THE NEW               
*        FORMAT ARE SET TO ZERO.  THIS PERMITS 'HELLO' TO FIND AND              
*        DELETE THE ELEMENT.                                                    
*                                                                               
DEELEM   NTR1                                                                   
         L     R8,0(R1)            A(DEMO IN LIST)                              
         LA    R4,REC              A(RECORD)                                    
*                                                                               
*    R4 IS COVERED BY RINVD FROM AN EARLIER USING                               
*                                                                               
         LR    R3,R4               CALCULATE END OF RECORD                      
         ZICM  RF,RINVLEN,2        RECORD LENGTH                                
         AR    R3,RF               A(END OF RECORD)                             
         BCTR  R3,0                                                             
         LA    R2,RINVPEL          A(1ST ELEMENT IN RECORD)                     
DEEL0002 EQU   *                                                                
         CR    R2,R3               END OF RECORD?                               
         BNL   DEEL0097            YES - ELEMENT NOT FOUND                      
         CLI   0(R2),X'DE'         X'DE' ELEMENT FOUND?                         
         BE    DEEL0008            YES                                          
DEEL0004 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         B     DEEL0002                                                         
DEEL0008 EQU   *                                                                
         CLI   1(R2),6             LENGTH OF ELEMENT = 6 (OLD STYLE?)           
         BNE   DEEL0010            NO                                           
         CLC   1(2,R8),2(R2)       YES - CHECK DEMO CODE                        
         BNE   DEEL0004            NOT CORRECT DEMO                             
         XC    DUB,DUB             SET UP FOR POSSIBLE DELETE                   
         MVC   DUB(2),1(R8)        DEMO MOD/CODE                                
         LA    RF,DUB              A(ADDITIONAL SEARCH ARGUMENT)                
         ST    RF,DMCB+8           A(DUB) IN P3                                 
         MVI   DMCB+8,2            L(ADDITIONAL SEARCH ARGUMENT)                
         B     DEEL0098            EXIT WITH 'FOUND'                            
DEEL0010 EQU   *                   NEW STYLE ELEMENT                            
         CLI   1(R2),12            LENGTH OF ELEMENT = 12 (NEW STYLE?)          
         BNE   DEEL0004            NO - UNKNOWN LENGTH - SKIP IT                
         CLC   1(2,R8),4(R2)       CHECK DEMO CODE                              
         BNE   DEEL0004            NOT CORRECT DEMO                             
         XC    DUB,DUB             SET UP FOR POSSIBLE DELETE                   
         MVC   DUB+2(2),1(R8)      DEMO MOD/CODE                                
         LA    RF,DUB              A(ADDITIONAL SEARCH ARGUMENT)                
         ST    RF,DMCB+8           A(DUB) IN P3                                 
         MVI   DMCB+8,4            L(ADDITIONAL SEARCH ARGUMENT)                
         B     DEEL0098            EXIT WITH 'FOUND'                            
DEEL0097 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO: NOT FOUND                     
         B     DEEL0099                                                         
DEEL0098 EQU   *                                                                
         LTR   RC,RC               SET CC NOT = ZERO: FOUND                     
DEEL0099 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*              EDIT INVENTORY NUMBER                                            
         SPACE 1                                                                
INVEDT   LA    R3,INVERR                                                        
         CLI   0(R6),3             INVENTORY NUMBER MUST BE THREE               
         BL    ERROR                                                            
         CLI   0(R6),4                                                          
         BH    ERROR                                                            
         CLI   12(R6),C'0'                                                      
         BL    ERROR                                                            
         CLI   12(R6),C'9'                                                      
         BH    ERROR                                                            
         CLI   13(R6),C'0'                                                      
         BL    ERROR                                                            
         CLI   13(R6),C'9'                                                      
         BH    ERROR                                                            
         SPACE 1                                                                
         PACK  DUB(8),12(2,R6)     QUARTER HOUR CODE                            
         CVB   R0,DUB                                                           
         STC   R0,RINVKQTR                                                      
         MVC   RINVKDAY,14(R6)     DAY CODE                                     
         MVI   RINVKLEN,C'0'       LENGTH                                       
         CLI   0(R6),4                                                          
         BNE   *+10                                                             
         MVC   RINVKLEN,15(R6)                                                  
         BR    R8                                                               
         SPACE 2                                                                
*              EDIT DATE                                                        
         SPACE 1                                                                
DATEDT   LA    R3,INVERR                                                        
         GOTO1 VDATVAL,DMCB,(0,12(R6)),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(3,RINVKSTD)                               
         BR    R8                                                               
         EJECT                                                                  
*              GET A HEADER RECORD                                              
         SPACE 1                                                                
*   ROUTINE ATTEMPTS TO FIND AN INVENTORY HEADER FOR THE REQUEST.               
*     IF A DATE WAS ENTERED, A FIRST PASS TRIES TO FIND THE EXACT               
*     DATE.  IF NOT FOUND, THE DATE IS USED TO TEST THE EFFECTIVE               
*     DATE RANGES OF THE AVAILABLE INVENTORY.  IF THE REQUEST DATE              
*     FALLS WITHIN A SET OF EFFECTIVE DATES, THE DATE ON THE KEY                
*     LINE IS CHANGED TO REFLECT THE EFFECTIVE START DATE.  THIS                
*     ENABLES THE DATE TO BE USED BY OTHER RECORD TYPES.                        
* *** NOTE:  AT THIS POINT, 'WORK2' STILL CONTAINS THE 'SCANNER'                
*     OUTPUT DESCRIBING THE BREAKDOWN OF THE KEY.                               
*                                                                               
HEADER   NTR1                                                                   
         LA    RE,REC2             GET HEADER IN REC2                           
         ST    RE,AIOAREA                                                       
         MVC   KEY,RINVREC                                                      
         XC    KEY+24(3),KEY+24    RINVKSRC/RINVKTXT                            
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   HREC0004            NOT FOUND                                    
         BAS   RE,GETREC           FOUND A MATCH                                
         B     HREC0030                                                         
HREC0004 EQU   *                                                                
         LA    R3,ERRNF                                                         
         OC    RINVKSTD,RINVKSTD                                                
         BNZ   HREC0014            INPUT A DATE - LOOK FOR EFF DATE             
*                                                                               
         CLC   KEYSAVE(21),KEY     DID I FIND INVENTORY NUMBER                  
         BNE   ERROR                                                            
*                                                                               
         OC    KEY+24(3),KEY+24    MAKE SURE IT'S HEADER                        
         BZ    HREC0008                                                         
         DC    H'0'                PROBLEM WITH FILE                            
         SPACE 1                                                                
HREC0008 BAS   RE,GETREC                                                        
         L     R4,AIOAREA          ESTABLISH DSECT 'USING' REG                  
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    HREC0030            TAKE ONE WITH NO END                         
HREC0012 EQU   *                                                                
         BAS   RE,SEQ              IF ALL HAVE END TAKE HIGHEST                 
         CLC   KEYSAVE(21),KEY                                                  
         BNE   HREC0030                                                         
         OC    KEY+24(3),KEY+24                                                 
         BZ    HREC0008                                                         
         B     HREC0012            NOT A HEADER                                 
         SPACE 1                                                                
*                                                                               
*   INVENTORY W/DATE ENTERED.  EXACT HIT NOT MADE.  SCAN FOR INV                
*      WITH EFFECTIVE DATE ENCOMPASSING THE ENTERED DATE WILL BE                
*      MADE.  IF DATE OUTSIDE ALL INVENTORY EFFECTIVE RANGES, A                 
*      'NO FOUND' ERROR WILL BE TAKEN.                                          
*                                                                               
HREC0014 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,RINVKSTD),(2,FULL)                               
         MVC   KEY,RINVREC         RESET KEY AFTER HIGH READ                    
*                                  MAY HAVE PASSED INVENTORY COMPLETELY         
         XC    KEY+21(6),KEY+21    BLANK OUT END OF KEY                         
HREC0016 EQU   *                                                                
         L     R4,AIOAREA          ESTABLISH DSECT 'USING' REG                  
         BAS   RE,HIGH             RETRIEVE 1ST/NEXT RECORD                     
         CLC   KEYSAVE(21),KEY     SAME INVENTORY #?                            
         BNE   ERROR               OUTSIDE ALL RANGES                           
         BAS   RE,GETREC           RETRIEVE RECORD                              
         MVC   DUB,RINVPEFF        UNLOAD EFFECTIVE DATES                       
         OC    DUB+2(2),DUB+2      ANY END DATE?                                
         BNZ   HREC0020            YES                                          
         MVC   DUB+2(2),=X'FFFF'   NO  - SET TO HIGH VALUES                     
HREC0020 EQU   *                                                                
         CLC   FULL(2),DUB         DATE INPUT VS EFFECTIVE START                
         BL    ERROR               EARLIER THAN START                           
*                                  WILL NEVER BE FOUND                          
         CLC   DUB+2(2),FULL       EFFECTIVE END VS DATE INPUT                  
         BL    HREC0024            END < DATE INPUT: KEEP GOING                 
         B     HREC0028            FOUND: NOT < START, NOT > END                
HREC0024 EQU   *                                                                
         MVC   KEY+24(3),=X'FFFFFF' SET KEY TO SKIP TRACKS, RATL                
         B     HREC0016            GO BACK FOR NEXT                             
HREC0028 EQU   *                                                                
         ZIC   RE,WORK2+64         L(DATE FIELD INPUT)                          
         LA    R2,LFMKEY           A(KEY FIELD)                                 
         ZIC   RF,HALF             DISP(DATE FIELD INTO KEY FIELD)              
         AR    R2,RF               A(KEY FIELD) + DISP = A(DATE)                
         EX    RE,HREC0032         WIPE OUT DATE BY LENGTH                      
         GOTO1 VDATCON,DMCB,(2,DUB),(5,(R2))                                    
*                                  REPLACE DATE INPUT WITH EFFECTIVE-           
*                                    DATE START DATE                            
         LA    R2,LFMKEYH          A(HEADER OF KEY FIELD)                       
         FOUT  (R2)                TURN ON TRANSMIT BIT                         
         OI    4(R2),X'20'         SET PREVIOUSLY VALID BIT                     
*                                                                               
HREC0030 LA    RE,REC              RESET I/O AREA                               
         ST    RE,AIOAREA                                                       
         B     EXXMOD                                                           
*                                                                               
HREC0032 MVC   0(0,R2),SPACES                                                   
         EJECT                                                                  
*              ADD THE RECORD TO FILE                                           
         SPACE 1                                                                
         USING RINVAEL,R6                                                       
FLADD    MVC   KEY,RINVREC                                                      
         SPACE 1                                                                
         LA    R6,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   RINVACOD(2),=X'EF0C'                                             
         MVC   RINVAFST,TODAY                                                   
         MVI   RINVAWHY,C'A'                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    FLPUT                                                            
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,AIOAREA,(R6)                                       
         BAS   RE,ADDREC           ADD THE RECORD                               
         MVC   BSVDA,KEY                                                        
         NI    DMINBTS,X'F7'       TURN OFF PASS DELETES                        
         BR    R8                                                               
         SPACE 1                                                                
FLPUT    TM    KEY+27,X'80'                                                     
         BNO   *+12                                                             
         MVI   KEY+27,0                                                         
         BAS   RE,WRITE            UNDELETE THE POINTER                         
         LA    RE,REC2                                                          
         ST    RE,AIOAREA                                                       
         BAS   RE,GETREC           GET OLD RECORD IN REC2                       
         SPACE 1                                                                
         GOTO1 VGETEL,DMCB,(X'EF',AIOAREA),DMCB+8                               
         L     R6,DMCB+8                                                        
         CLI   DMCB,X'FF'                                                       
         BNE   *+8                                                              
         LA    R6,WORK                                                          
         MVC   RINVALST,TODAY                                                   
         MVI   RINVAWHY,C'C'                                                    
         SPACE 1                                                                
         LA    RE,REC                                                           
         ST    RE,AIOAREA                                                       
         SPACE 1                                                                
         GOTO1 VDELELEM,DMCB,(X'EF',AIOAREA)                                    
         GOTO1 VADDELEM,DMCB,AIOAREA,(R6)                                       
         BAS   RE,PUTREC           WRITE BACK THE NEW                           
         NI    DMINBTS,X'F7'                                                    
         MVC   BSVDA,KEY+28                                                     
         BR    R8                                                               
         SPACE 1                                                                
FLCHA    MVC   KEY,RINVREC                                                      
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         LA    RE,REC                                                           
         ST    RE,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         GOTO1 VGETEL,DMCB,(X'EF',AIOAREA),DMCB+8                               
         L     R6,DMCB+8                                                        
         CLI   DMCB,X'FF'                                                       
         BER   R8                                                               
         MVC   RINVALST,TODAY                                                   
         MVI   RINVAWHY,C'C'                                                    
         SPACE 1                                                                
         BAS   RE,PUTREC           WRITE BACK THE NEW                           
         MVC   BSVDA,KEY+28                                                     
         BR    R8                                                               
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
STDEM    DC    CL40'P1,S1,R2,R1,1,45,42,95,92,125'                              
         SPACE 1                                                                
MODLIST  DC    C'RTSPQXU'                                                       
MODS     EQU   *-MODLIST                                                        
         SPACE 1                                                                
HOMES    DS    0XL10                                                            
         DC    X'00',C'T',X'01'    HOMES                                        
         DC    X'00',C'T',X'02'    META                                         
         DC    X'00',C'T',X'03'    METB                                         
         DC    X'FF'                                                            
         SPACE 1                                                                
ADULTS   DS    0XL19                                                            
         DC    X'00',C'T',AL1(145) AD18+                                        
         DC    X'00',C'T',AL1(142) AD1849                                       
         DC    X'00',C'T',AL1(141) AD1834                                       
         DC    X'00',C'T',AL1(148) AD2554                                       
         DC    X'00',C'T',AL1(191) AD2149                                       
         DC    X'00',C'T',AL1(153) AD3564                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
VIEWERS  DS    0XL19                                                            
         DC    X'00',C'T',AL1(127) V2+                                          
         DC    X'00',C'T',AL1(129) V1234                                        
         DC    X'00',C'T',AL1(128) V1224                                        
         DC    X'00',C'T',AL1(125) V1217                                        
         DC    X'00',C'T',AL1(123) V6-11                                        
         DC    X'00',C'T',AL1(122) V2-11                                        
         DC    X'FF'                                                            
         SPACE 1                                                                
MEN      DS    0XL22                                                            
         DC    X'00',C'T',AL1(095) M18+                                         
         DC    X'00',C'T',AL1(091) M1834                                        
         DC    X'00',C'T',AL1(092) M1849                                        
         DC    X'00',C'T',AL1(098) M2554                                        
         DC    X'00',C'T',AL1(099) M2564                                        
         DC    X'00',C'T',AL1(097) M2549                                        
         DC    X'00',C'T',AL1(115) M2149                                        
         DC    X'FF'                                                            
         SPACE 1                                                                
WOMEN    DS    0XL31                                                            
         DC    X'00',C'T',AL1(045) W18+                                         
         DC    X'00',C'T',AL1(041) W1834                                        
         DC    X'00',C'T',AL1(042) W1849                                        
         DC    X'00',C'T',AL1(047) M2549                                        
         DC    X'00',C'T',AL1(048) W2554                                        
         DC    X'00',C'T',AL1(028) W1224                                        
         DC    X'00',C'T',AL1(049) W2564                                        
         DC    X'00',C'T',AL1(029) W1234                                        
         DC    X'00',C'T',AL1(065) WWRK                                         
         DC    X'00',C'T',AL1(071) W2149                                        
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RELFMGEN                                                       
         EJECT                                                                  
       ++INCLUDE RELFMTWA                                                       
         EJECT                                                                  
       ++INCLUDE RELFMEED                                                       
         EJECT                                                                  
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDBEXTRAD                                                     
*        PRINT OFF                                                              
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
       ++INCLUDE REGENDEM                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
       ++INCLUDE FATWA                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004RELFM20   05/01/02'                                      
         END                                                                    
