*          DATA SET DDPANXREF  AT LEVEL 006 AS OF 01/13/12                      
*PROCESS USING(WARN(15))                                                        
*PHASE PANXREFA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE SORTER                                                                 
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PANIC                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
PANXREF  TITLE 'PANXREF - FAST XREF FROM PAN TAPE'                              
PANXREF  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**XREF**,=V(REGSAVE),R8                                        
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         GOTO1 VSTXITER,DMCB,DUMPLIST                                           
                                                                                
         MVC   TITLE+12(32),=C'Panvalet Library Cross-Reference'                
         MVI   REQOPTS,C':'                                                     
         MVI   GETPAN,C'I'         SET FLAG TO INIT DIR READ FROM TAPE          
*                                                                               
GETCARD  GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   =C'/*',CARD         CHECK FOR END OF SELECTION FILE              
         BNE   GC6                                                              
         CLI   REQOPTS+2,C' '      ANY REQUEST OPTIONS TO PRINT                 
         BE    GC2                 NO                                           
         MVC   MID1+85(15),=C'Request options'                                  
         MVC   MID1+100(L'REQOPTS),REQOPTS                                      
*                                                                               
GC2      CLI   PHASELST,0          TEST PHASE/PROGID ACTION                     
         BNE   GC5                 YES - ALTERNATIVE HEADINGS                   
         MVC   SUB1(24),=C'Book name   Level   Type'                            
         MVC   SUB2(24),=C'---------   -----   ----'                            
         MVC   SUB1+031(14),=C'---Includes---'                                  
         MVC   SUB1+052(14),=C'---Includes---'                                  
         MVC   SUB1+073(14),=C'---Includes---'                                  
         MVC   SUB1+094(14),=C'---Includes---'                                  
         MVC   SUB1+115(14),=C'---Includes---'                                  
         MVC   SUB2+031(14),=C'Name       Seq'                                  
         MVC   SUB2+052(14),=C'Name       Seq'                                  
         MVC   SUB2+073(14),=C'Name       Seq'                                  
         MVC   SUB2+094(14),=C'Name       Seq'                                  
         MVC   SUB2+115(14),=C'Name       Seq'                                  
*                                                                               
         CLI   DICTPRFX,C' '                                                    
         BE    GC3                                                              
         MVC   MID1(42),=C'Books which include XXX dictionary equates'          
         MVC   MID1+20(3),DICTPRFX                                              
         B     GETTAPE                                                          
*                                                                               
GC3      CLI   SCANNING,C'Y'                                                    
         BNE   GC4                                                              
         MVC   MID1(39),=C'Books which include specified string:- '             
         B     GETTAPE                                                          
*                                                                               
GC4      MVC   MID1(28),=C'List of ++INCLUDE statements'                        
         B     GETTAPE                                                          
*                                                                               
GC5      MVC   MID1(36),=C'Phase List/Pan Book Cross-reference'                 
         CLI   PHASELST,2                                                       
         BNE   *+10                                                             
         MVC   MID1(06),=C'Progid'                                              
         MVC   SUB1+026(16),=C'Phase     Source'                                
         MVC   SUB1+047(16),=C'Phase     Source'                                
         MVC   SUB1+068(16),=C'Phase     Source'                                
         MVC   SUB1+089(16),=C'Phase     Source'                                
         MVC   SUB1+110(16),=C'Phase     Source'                                
         CLI   PHASELST,2                                                       
         BNE   GC5A                                                             
         MVC   SUB1+026(06),=C'Progid'                                          
         MVC   SUB1+047(06),=C'Progid'                                          
         MVC   SUB1+068(06),=C'Progid'                                          
         MVC   SUB1+089(06),=C'Progid'                                          
         MVC   SUB1+110(06),=C'Progid'                                          
GC5A     MVC   SUB2+026(16),=C'----------------'                                
         MVC   SUB2+047(16),=C'----------------'                                
         MVC   SUB2+068(16),=C'----------------'                                
         MVC   SUB2+089(16),=C'----------------'                                
         MVC   SUB2+110(16),=C'----------------'                                
         B     GETTAPE                                                          
*                                                                               
GC6      CLC   =C'SCAN=',CARD      SCAN=XYZ (SCAN FOR STRING XYZ)               
         BNE   GC8                                                              
         MVI   SCANNING,C'Y'                                                    
         L     RF,ASCANTAB         FIND NEXT ENTRY IN SCANTAB                   
         SR    RE,RE                                                            
         CLI   0(RF),0                                                          
         BE    *+16                                                             
         IC    RE,0(RF)                                                         
         LA    RF,1(RE,RF)                                                      
         B     *-16                                                             
         LA    RE,CARD+5+SCANMAX-1                                              
         LA    R1,SCANMAX                                                       
         CLI   0(RE),C' '                                                       
         BH    *+14                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         B     GETCARD                                                          
         STC   R1,0(RF)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),CARD+5                                                   
         LA    RF,2(RF,R1)                                                      
         C     RF,ASCANTAX                                                      
         BNH   GETCARD                                                          
         DC    H'0'                TOO MANY SCAN CARDS                          
*                                                                               
GC8      CLC   =C'BOOK=',CARD      BOOK=XYZ (FILTER ON BOOK NAME XYZ)           
         BNE   GC10                                                             
         MVC   BOOKPRFX,CARD+5                                                  
         LA    RF,L'BOOKPRFX                                                    
         LA    RE,BOOKPRFX+L'BOOKPRFX-1                                         
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         STC   RF,BOOKPFIL         KEEP INPUT LENGTH                            
         LA    R1,REQOPTS+L'REQOPTS-1  BUILD REQOPTS ENTRY                      
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'BOOKPRFX+5,R1),CARD                                          
         B     GETCARD                                                          
*                                                                               
GC10     CLC   =C'DISABLED=',CARD  OPTION TO EXCLUDE DISABLED BOOKS             
         BNE   GC12                                                             
         MVC   DISABLED,CARD+9                                                  
         LA    R1,REQOPTS+L'REQOPTS-1  BUILD REQOPTS ENTRY                      
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(10,R1),CARD                                                    
         B     GETCARD                                                          
*                                                                               
GC12     CLC   =C'DICT=',CARD      DICT=AC (ACCOUNT DATA DICT XREF)             
         BNE   GC14                                                             
         MVC   DICTPRFX,CARD+5                                                  
         MVI   DICTPRFX+2,C'#'     SPECIAL DATA DICTIONARY CHARACTER            
         B     GETCARD                                                          
*                                                                               
GC14     CLC   =C'PHASE',CARD      LIST PHASE NAMES/PAN BOOKS                   
         BNE   GC16                                                             
         MVI   PHASELST,1          SET ACTION                                   
         B     GETCARD                                                          
*                                                                               
GC16     CLC   =C'PROGID',CARD     LIST PHASE NAMES/PAN BOOKS                   
         BNE   GC18                                                             
         MVI   PHASELST,2          SET ACTION                                   
         B     GETCARD                                                          
*                                                                               
GC18     CLC   =C'SHOWDATA',CARD   SHOW DATA LINES FOR SCAN                     
         BNE   GETCARD                                                          
         MVI   SHOWDATA,C'Y'                                                    
         B     GETCARD                                                          
         EJECT                                                                  
***********************************************************************         
* PANREAD -    READ PAN TAPE                                          *         
*    INPUT:                                                           *         
*        GETPAN   C'I'  - INITIALIZE. READ/SAVE IN FILE DIR ENTRIES   *         
*                 C'D'  - READ NEXT DIRECTORY ENTRY                   *         
*                 C'L'  - READ NEXT LINE OF PANBOOK.                  *         
*    OUTPUT:                                                          *         
*        DMCB     X'01' - CARD CONTAINS A DIR ENTRY                   *         
*                 X'03' - CARD CONTAINS A LINE OF THE PANBOOK         *         
***********************************************************************         
                                                                                
PANREAD  NTR1  ,                                                                
         CLI   GETPAN,C'I'                                                      
         BE    PANBLD              INIT/BUILD DIR LIST                          
*                                                                               
         CLI   GETPAN,C'D'                                                      
         BE    PANDIR              READ DIR ENTRY                               
*                                                                               
         CLI   GETPAN,C'L'                                                      
         BE    PANLIN              READ LINE OF PANBOOK                         
*                                                                               
         CLI   GETPAN,C'E'                                                      
         BNE   PANRDX              READ LINE OF PANBOOK                         
         LA    RE,1                                                             
         DC    H'0'                                                             
*                                                                               
PANRDX   B     XIT                                                              
                                                                                
***********************************************************************         
* GETPAN=C'I'   BUILD FILE OF DIR ENTRIES                             *         
***********************************************************************         
                                                                                
PANBLD   OPEN  (OUTFILE,OUTPUT)    FIRST READ ALL DIR ENTRIES                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   PANBOOK,=CL10'A'    READ DIR->SAVE IN SEQ FILE                   
PANBLD3  GOTO1 VPANIC,DMCB,(X'40',=C'READ'),(C'T',=C'DIR'),PANBOOK,CARD         
         CLC   =C'/*',CARD         END OF DIRECTORY?                            
         BE    PANBLD4             YES                                          
         PUT   OUTFILE,CARD                                                     
         B     PANBLD3                                                          
*                                                                               
PANBLD4  CLOSE OUTFILE                                                          
         GOTO1 VPANIC,DMCB,=C'CLOSE',0,0,0                                      
         OPEN  (OUTFILE,INPUT)     OPEN DIR LIST FILE                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     PANDIR              READ 1ST DIR ENTRY                           
                                                                                
***********************************************************************         
* GETPAN=C'D'   READ A DIRECTORY ENTRY FROM THE LIST IN THE FILE      *         
***********************************************************************         
                                                                                
PANDIR   GET   OUTFILE,CARD        READ IN DIR ENTRY                            
         MVC   PANBOOK,CARD        SET PANBOOK TO NAME OF BOOK READ             
         MVI   GETPAN,C'L'         NEXT CALL WILL GET A LINE OF PANBOOK         
         MVI   DMCB,X'01'          RETURN CODE: DIR ITEM READ                   
         B     PANRDX              XIT                                          
*                                                                               
PANDIRX  DS    0H                  NO MORE ITEMS IN DIRECTORY                   
         GOTO1 VPANIC,DMCB,=C'CLOSE',0,0,0                                      
         CLOSE OUTFILE                                                          
         MVI   DMCB,X'FF'          RETURN CODE: END OF PAN LIBRARY              
         MVI   GETPAN,C'E'         ERROR IF PANRD CALLED AGAIN                  
         B     PANRDX                                                           
                                                                                
***********************************************************************         
* GETPAN=C'L'   READ A LINE OF THE PANBOOK                            *         
***********************************************************************         
                                                                                
PANLIN   GOTO1 VPANIC,DMCB,=C'READ',(C'T',=C'PAN'),PANBOOK,CARD                 
         TM    DMCB+8,X'80'        END OF BOOK?                                 
         BO    PANDIR              YES, GO READ DIR ENTRY INSTEAD               
         CLI   DMCB+8,0            MAKE SURE NO ERRS RDG FILE                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   GETPAN,C'L'                                                      
         MVI   DMCB,X'03'          RETURN CODE: LINE OF BOOK                    
         B     PANRDX              XIT                                          
         EJECT                                                                  
***********************************************************************         
* READ PAN TAPE AND PRODUCE REPORT ON PAN BOOK ATTRIBUTES             *         
***********************************************************************         
                                                                                
GETTAPE  LA    R5,GT10             R5=A(BRANCH)                                 
GT2      BAS   RE,PANREAD          READ PAN LIBRARY. GETPAN=D,L,I               
         CLI   DMCB,X'03'          TEST CARD IS DIRECTORY ITEM                  
         BH    TAPEEOF                                                          
         CLI   DMCB,X'01'          TEST CARD IS DIRECTORY ITEM                  
         BNER  R5                                                               
         BAS   RE,ANYREP           OUTPUT ANYTHING IN THE PRINT LINE            
         LA    R5,GT2              SET A(BRANCH)                                
         LA    R9,GETTAPE                                                       
         SR    RF,RF                                                            
         ICM   RF,1,BOOKPFIL       TEST BOOK FILTER PREFIX SET                  
         BZ    GT4                                                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BE    GT4                                                              
         CLC   CARD(0),BOOKPRFX                                                 
         B     GT2                                                              
*                                                                               
GT4      CLI   DISABLED,C'N'       TEST DISABLED FILTER SET                     
         BNE   GT6                                                              
         CLI   CARD+25,C'D'        TEST BOOK HAS DISABLED STATUS                
         BE    GT2                                                              
*                                                                               
GT6      LA    R5,GT10             SET A(BRANCH)                                
         ZAP   SEQ,=P'0'           CLEAR SEQUENCE NUMBER                        
         MVC   SAVEDIR,CARD                                                     
         CLC   SAVEDIR+10(3),=C'000'                                            
         BE    GETTAPE             IGNORE SUPERSETS, GO GET NEXT CARD           
         CLI   PHASELST,0          FOR PHASELIST                                
         BNE   GETTAPE             NOTHING ELSE NEEDED YET                      
*                                                                               
         MVC   P(10),SAVEDIR       ENTER BOOK NAME ON PRINT LINE                
         MVC   P+13(3),SAVEDIR+10  ENTER BOOK LEVEL ON PRINT LINE               
         MVI   PANXIND,SCRBKQ      CLEAR IND AND SET SCREEN BOOK                
         MVC   P+20(4),=C'DATA'                                                 
         CLC   =C'ASMB',SAVEDIR+18                                              
         BNE   *+14                                                             
         MVI   PANXIND,ASMBKQ      CLEAR IND AND SET ASSEMBLER BOOK             
         MVC   P+20(4),=C'ASMB'                                                 
         XC    REC,REC                                                          
         MVC   REC(10),SAVEDIR                                                  
         MVC   REC+20(3),SAVEDIR+10                                             
         GOTO1 PUTSRT                                                           
         B     GETTAPE             GO GET NEXT CARD                             
*                                                                               
GT10     CLI   PHASELST,1                                                       
         BE    GT12                                                             
         CLI   PHASELST,2                                                       
         BE    GT18                                                             
         CLI   DMCB,X'03'          SEE IF CARD IS A DATA ITEM                   
         BL    GETTAPE             NO -- IT'S A PAN COMMENT                     
         BH    TAPEEOF             NO -- END OF THE TAPE                        
         CLC   CARD(19),=C'*          DATA SET'                                 
         BE    GETTAPE             IGNORE DATA SETS, GO GET NEXT CARD           
         CLC   SAVEDIR+10(3),=C'000'                                            
         BE    GETTAPE             IGNORE SUPERSETS, GO GET NEXT CARD           
*                                                                               
         AP    SEQ,=P'1'           ADD 1 TO THE SEQUENCE NUMBER                 
         CLI   SCANNING,C'Y'                                                    
         BE    GT20                YES                                          
         CLI   DICTPRFX,C' '       WAS A DICTIONARY PREFIX GIVEN?               
         BNE   GT40                YES                                          
*                                                                               
         CLC   CARD+7(10),=C'++INCLUDE ' NO INPUT CARD SO INCLUDE XREF          
         BNE   GETTAPE             CARD ISNT INCLUDE, GO GET NEXT CARD          
         MVC   REP,SPACES                                                       
         MVC   REP+5(10),CARD+17   GET NAME OF INCLUDE FILE                     
         EDIT  (P4,SEQ),(5,REP+16),ALIGN=LEFT                                   
         BAS   RE,FORMAT           MOVE INFO FROM 'REP' TO PRINT LINE           
         MVC   REC(10),CARD+17     WRITE A SORT RECORD FOR INCLUDE              
         MVC   REC+10(13),SAVEDIR                                               
         MVC   REC+23(4),SEQ                                                    
         GOTO1 PUTSRT                                                           
         B     GETTAPE             GO GET NEXT CARD                             
*                                                                               
GT12     CLC   =C'*PHASE',CARD     TEST PHASE CARD                              
         BNE   GETTAPE                                                          
         XC    REC,REC                                                          
         LA    RF,CARD+7                                                        
         LA    RE,10                                                            
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    RF,1(RF)                                                         
         BCT   RE,*-12                                                          
         MVC   REC(10),0(RF)                                                    
         LA    RF,REC                                                           
         LA    RE,10                                                            
GT14     CLI   0(RF),C','          CLEAR ANYTHING FOLLOWING PHASE NAME          
         BE    GT16                                                             
         CLI   0(RF),C' '                                                       
         BE    GT16                                                             
         LA    RF,1(RF)                                                         
         BCT   RE,GT14                                                          
GT16     MVC   0(10,RF),REC+10                                                  
         MVC   REC+10(10),SAVEDIR                                               
         GOTO1 PUTSRT                                                           
         B     GETTAPE                                                          
*                                  GET PROGID                                   
GT18     CLC   =C'NMOD',CARD+9     TEST NMOD/NBASE CARD                         
         BE    *+14                                                             
         CLC   =C'NBAS',CARD+9                                                  
         BNE   GETTAPE                                                          
         XC    REC,REC                                                          
         LA    RF,CARD+9                                                        
         LA    RE,30                                                            
         CLI   0(RF),C','          LOOK FOR FIRST COMMA                         
         BE    GT19                                                             
         LA    RF,1(RF)                                                         
         BCT   RE,*-12                                                          
         B     GETTAPE             CAN'T FIND PROGID                            
GT19     MVC   REC(8),1(RF)                                                     
         LA    RF,REC                                                           
         LA    RE,8                                                             
GT19A    CLI   0(RF),C','          CLEAR ANYTHING FOLLOWING PROGID              
         BE    GT19B                                                            
         CLI   0(RF),C' '                                                       
         BE    GT19B                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,GT19A                                                         
GT19B    MVC   0(10,RF),REC+10                                                  
         MVC   REC+10(10),SAVEDIR                                               
         GOTO1 PUTSRT                                                           
         B     GETTAPE                                                          
         EJECT                                                                  
***********************************************************************         
* SEARCH FOR 'SCAN=' STRINGS                                          *         
***********************************************************************         
                                                                                
GT20     LA    RF,SCSRCHT          RF=A(LM VALUES TABLE)                        
         CLC   0(1,RF),PANXIND                                                  
         BE    *+12                                                             
         LA    RF,L'SCSRCHT(RF)                                                 
         B     *-14                                                             
*                                                                               
         LM    R2,R5,0(RF)                                                      
         L     RF,ASCANTAB                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RF)          INPUT LENGTH OF SCAN STRING                  
         BNZ   *+6                                                              
         DC    H'0'                HOW DID WE GET HERE?                         
         TM    PANXIND,CONTDQ      TEST THIS IS A CONTINUATION CARD             
         BNO   GT22                NO                                           
         NI    PANXIND,255-CONTDQ  YES - PROCESS SAVED END OF PREVIOUS          
         CLI   0(R4),C' '          TEST CONTINUATION COLUMN                     
         BNH   *+8                                                              
         OI    PANXIND,CONTDQ      SET NEXT CARD IS A CONTINUATION              
         MVC   0(SCSVLNQ,R2),LASTREC  MERGE SAVED INTO CURRENT CARD             
         TM    PANXIND,CONTDQ                                                   
         BNO   GT26                                                             
         B     GT24                                                             
*                                                                               
GT22     CLI   0(R4),C' '          TEST CONTINUATION COLUMN                     
         BNH   GT26                                                             
         OI    PANXIND,CONTDQ      SET NEXT IS A CONTINUATION CARD              
GT24     MVC   LASTREC,SPACES                                                   
         MVC   LASTREC(SCSVLNQ),0(R5) SAVE END OF CARD TO MERGE TO NEXT         
*                                                                               
GT26     BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BNE   GT28                                                             
         CLC   0(0,R2),1(RF)                                                    
         MVC   REC(10),SPACES      FORM A SORT RECORD FOR SCAN WORD             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REC(0),1(RF)                                                     
         MVC   REC+10(13),SAVEDIR                                               
         MVC   REC+23(4),SEQ                                                    
         STM   RE,R1,SAVERER1                                                   
         GOTO1 PUTSRT                                                           
         LM    RE,R1,SAVERER1                                                   
*                                                                               
GT28     LA    RF,2(RF,R1)                                                      
         ICM   R1,1,0(RF)                                                       
         BNZ   GT26                                                             
         L     RF,ASCANTAB                                                      
         IC    R1,0(RF)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,GT26                                                          
         B     GETTAPE             CARD DOESNT CONTAIN WORD  GET NEXT           
         EJECT                                                                  
***********************************************************************         
* SEARCH FOR DATA DICTIONARY EQUATE NAMES                             *         
***********************************************************************         
                                                                                
GT40     LA    RF,DDSRCHT          RF=A(LM VALUES TABLE)                        
         CLC   0(1,RF),PANXIND                                                  
         BE    *+12                                                             
         LA    RF,L'DDSRCHT(RF)                                                 
         B     *-14                                                             
*                                                                               
         LM    R2,R5,0(RF)                                                      
         TM    PANXIND,CONTDQ      TEST THIS IS A CONTINUATION CARD             
         BNO   GT42                NO                                           
         NI    PANXIND,255-CONTDQ  YES - PROCESS SAVED END OF PREVIOUS          
         CLI   0(R4),C' '          TEST CONTINUATION COLUMN                     
         BNH   *+8                                                              
         OI    PANXIND,CONTDQ        SET NEXT CARD IS A CONTINUATION            
         MVC   0(DDSVLNQ,R2),LASTREC MERGE SAVED INTO CURRENT CARD              
         TM    PANXIND,CONTDQ                                                   
         BNO   GT46                                                             
         B     GT44                                                             
*                                                                               
GT42     CLI   0(R4),C' '          TEST CONTINUATION COLUMN                     
         BNH   GT46                                                             
         OI    PANXIND,CONTDQ      SET NEXT IS A CONTINUATION CARD              
GT44     MVC   LASTREC,SPACES                                                   
         MVC   LASTREC(DDSVLNQ),0(R5) SAVE END OF CARD TO MERGE TO NEXT         
GT46     CLC   DICTPRFX,0(R2)      MATCH ON PREFIX?                             
         BE    GT48                YES                                          
         LA    R2,1(R2)                                                         
         BCT   R3,GT46                                                          
         BCTR  R2,0                                                             
         BAS   RE,CHARVAL          TEST LAST COL WAS A VALID DD CHAR            
         BE    GT60                YES - GET NEXT CARD                          
         MVC   LASTREC,SPACES      NO - NO NEED TO MERGE ANY CONT.              
         NI    PANXIND,255-CONTDQ                                               
         B     GT60                GET NEXT CARD                                
*                                                                               
GT48     MVC   REC(10),SPACES                                                   
         LA    R1,REC                                                           
GT50     BAS   RE,CHARVAL          TEST VALID DD CHARACTER AT 0(R2)             
         BNE   GT56                                                             
         MVC   0(1,R1),0(R2)       PUT WHOLE EQUATED SYMBOL INTO RECORD         
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         BCT   R3,GT50                                                          
         TM    PANXIND,CONTDQ      TEST CONTINUED ON NEXT CARD                  
         BO    GT60                YES - GIVE UP, WILL GET IT NEXT TIME         
*                                                                               
GT56     MVC   REP,SPACES                                                       
         MVC   REP+5(10),REC                                                    
         EDIT  (P4,SEQ),(5,REP+16),ALIGN=LEFT                                   
         BAS   RE,FORMAT           PUT 'REP' INFO INTO PRINT LINE               
         MVC   REC+10(13),SAVEDIR  FORM SORT RECORD FOR DICT ITEM               
         MVC   REC+23(4),SEQ                                                    
         GOTO1 PUTSRT                                                           
         B     GT46                                                             
*                                                                               
GT60     B     GETTAPE             GET NEXT                                     
                                                                                
*                                                                               
CHARVAL  CLI   0(R2),C'#'          TEST VALID DD CHARACTERS                     
         BE    CHARVX                                                           
         CLI   0(R2),C'$'                                                       
         BE    CHARVX                                                           
         CLI   0(R2),C'@'                                                       
         BE    CHARVX                                                           
         CLI   0(R2),C'A'                                                       
         BL    CHARVX                                                           
         CR    RE,RE                                                            
CHARVX   BR    RE                  RETURN WITH CC SET EQU/NEQ                   
         EJECT                                                                  
***********************************************************************         
* PRODUCE ON REPORT DICT REFERENCES OR INCLUDE FILES(DEFAULT)         *         
***********************************************************************         
                                                                                
TAPEEOF  BAS   RE,ANYREP           TAPE END - CLEAR OUT PRINT LINE              
         CLI   PHASELST,0                                                       
         BNE   TE4                                                              
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         CLI   SCANNING,C'Y'                                                    
         BE    TE2                                                              
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID1(32),=C'Location of ++INCLUDE statements'                    
         CLI   DICTPRFX,C' '       WAS A DICTIONARY PREFIX GIVEN?               
         BE    TE2                 NO                                           
         MVC   MID1(42),=C'List of XXX data dictionary equates       '          
         MVC   MID1+8(3),DICTPRFX                                               
TE2      CLI   SHOWDATA,C'Y'                                                    
         BNE   TE3                                                              
         MVC   SUB1(18),=C'---Included by----'                                  
         MVC   SUB2(18),=C'Name       Lvl Seq'                                  
         MVC   SUB1+24(13),=C'PAN Statement'                                    
         MVC   SUB2+24(13),=C'-------------'                                    
         B     TE4                                                              
TE3      MVC   SUB1(4),=C'Name'                                                 
         MVC   SUB2(4),=C'----'                                                 
         MVC   SUB1+026(18),=C'---Included by----'                              
         MVC   SUB2+026(18),=C'Name       Lvl Seq'                              
         MVC   SUB1+047(18),=C'---Included by----'                              
         MVC   SUB2+047(18),=C'Name       Lvl Seq'                              
         MVC   SUB1+068(18),=C'---Included by----'                              
         MVC   SUB2+068(18),=C'Name       Lvl Seq'                              
         MVC   SUB1+089(18),=C'---Included by----'                              
         MVC   SUB2+089(18),=C'Name       Lve Seq'                              
         MVC   SUB1+110(18),=C'---Included by----'                              
         MVC   SUB2+110(18),=C'Name       Lvl Seq'                              
TE4      ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'75'                                                      
         XC    REC,REC                                                          
                                                                                
         L     R3,AUNUSED          R3 A(TABLE FOR UNUSED ITEMS)                 
GETSORT  MVC   LASTREC,REC                                                      
         GOTO1 VSORTER,DMCB,SORTGET                                             
         ICM   R2,15,4(R1)                                                      
         BNZ   GS10                SEE IF ANYMORE ITEMS LEFT                    
         BAS   RE,ANYREP           EMPTY OUT REMNANTS FROM PRINT LINE           
         CLI   DICTPRFX,C' '                                                    
         BE    ENDREP                                                           
         MVC   MID1(42),=C'List of unused XXX data dictionary equates'          
         MVC   MID1+15(3),DICTPRFX                                              
         XC    SUB1,SUB1                                                        
         XC    SUB2,SUB2                                                        
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'75'                                                      
         XC    REP,REP                                                          
         MVC   P,SPACES                                                         
         L     R3,AUNUSED          R3=A(START OF UNUSED ITEM TBLE)              
GETUNUSD CLC   0(10,R3),SPACES     SEE IF ANY MORE UNUSED ITEMS                 
         BE    ENDREP              NO-END OF REPORT                             
         MVC   REP(10),0(R3)       YES-PLACE THE SAID ITEM IN REP               
         BAS   RE,FORMAT           FORMAT THE PRINT LINE                        
         LA    R3,10(R3)           BUMP TO NEXT ENTRY                           
         B     GETUNUSD                                                         
ENDREP   BAS   RE,ANYREP           EMPTY OUT REMNANTS FROM PRINTLINE            
         XBASE ,                                                                
*                                                                               
GS10     CLI   PHASELST,0                                                       
         BNE   GS30                                                             
         LH    RE,SORTRECX                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SORTREC(0),0(R2)                                                 
*                                                                               
         CLC   REC(10),LASTREC     FIRST FOR NEW BOOK?                          
         BE    GS20                                                             
         CLI   DICTPRFX,C' '       IF DICT REPORT FORM LIST OF UNUSED           
         BE    GS12                                                             
         CLI   FIRSTLIN,C'Y'       SEE IF ONLY 1 LINE OF BOOKS                  
         BNE   GS12                                                             
         CLC   P+26(21),SPACES     IF SO, SEE IF IN DICTIONARY                  
         BE    GS12                NO - SKIP TO NEXT                            
         CLC   P+47(21),SPACES     YES - SEE IF IN ANY BOOKS                    
         BNE   GS12                                                             
         MVC   P+47(82),NOTUSED    THIS DICT ITEM IS NOT USED                   
         MVC   0(10,R3),LASTREC    STORE ITEM IN NOT USED TABLE                 
         LA    R3,10(R3)           BUMP TO NEXT ENTRY                           
         C     R3,AUNUSEDX                                                      
         BNH   *+6                                                              
         DC    H'0'                MAKE UNUSED TABLE LARGER                     
                                                                                
GS12     BAS   RE,ANYREP                                                        
         MVI   FIRSTLIN,C'Y'                                                    
         MVC   P,SPACES                                                         
         MVC   P(10),REC                                                        
         CLI   SCANNING,C'Y'                                                    
         BNE   GS14                                                             
         MVC   MID1+39(10),REC                                                  
         ZAP   LINE,=P'75'                                                      
                                                                                
GS14     CLI   REC+10,0            SHOULD BE A DIRECTORY HEADER                 
         BE    GETSORT                                                          
         CLI   DICTPRFX,C' '       WAS A DICTIONARY PREFIX GIVEN?               
         BNE   GS20                YES                                          
         CLI   SCANNING,C'Y'                                                    
         BE    GS20                                                             
         MVC   P+11(9),=C'*Missing*'                                            
                                                                                
GS20     MVC   REP,SPACES                                                       
         MVC   REP(10),REC+10      INCLUDE BOOK/LEVEL/SEQ                       
         MVC   REP+11(3),REC+20                                                 
         EDIT  (P4,REC+23),(5,REP+15),ALIGN=LEFT                                
         CLI   SCANNING,C'Y'       ARE WE SCANNING                              
         BNE   GS22                                                             
         CLI   SHOWDATA,C'Y'       AND SHOWING DATA                             
         BNE   GS22                                                             
         MVC   P(21),REP                                                        
         MVC   P+24(L'CARD),CARD                                                
         GOTO1 VPRINTER                                                         
         B     GETSORT                                                          
                                                                                
GS22     BAS   RE,FORMAT                                                        
         B     GETSORT                                                          
*                                                                               
GS30     MVC   REP,SPACES          PHASE/PROGID LIST FORMAT                     
         MVC   REP,0(R2)                                                        
         BAS   RE,FORMAT                                                        
         B     GETSORT                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INSERT DATA TO PRINTLINE AND OUTPUT WHEN FULL            *         
***********************************************************************         
                                                                                
FORMAT   NTR1  ,                                                                
         CLC   P+110(21),SPACES    IS PRINT LINE FULL                           
         BE    FORMAT1                                                          
         L     RF,VPRINTER         YES OUTPUT LINE AND START NEW ONE            
         BASR  RE,RF                                                            
         MVC   P+26(21),REP                                                     
         MVI   FIRSTLIN,C'N'                                                    
         B     XIT                                                              
*                                                                               
FORMAT1  CLC   P+26(21),SPACES                                                  
         BNE   FORMAT2                                                          
         MVC   P+26(21),REP                                                     
         B     XIT                                                              
*                                                                               
FORMAT2  CLC   P+47(21),SPACES                                                  
         BNE   FORMAT3                                                          
         MVC   P+47(21),REP                                                     
         B     XIT                                                              
*                                                                               
FORMAT3  CLC   P+68(21),SPACES                                                  
         BNE   FORMAT4                                                          
         MVC   P+68(21),REP                                                     
         B     XIT                                                              
*                                                                               
FORMAT4  CLC   P+89(21),SPACES                                                  
         BNE   FORMAT5                                                          
         MVC   P+89(21),REP                                                     
         B     XIT                                                              
*                                                                               
FORMAT5  MVC   P+110(21),REP                                                    
         B     XIT                                                              
                                                                                
ANYREP   NTR1  ,                   PRINT A LINE OF DATA                         
         CLC   P+26(84),SPACES                                                  
         BE    XIT                                                              
         GOTO1 VPRINTER            OUTPUT PRINT LINE                            
         BASR  RE,RF               OUTPUT BLANK LINE                            
         B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO PUT RECORD TO SORTER - SORT INITIALISED FIRST TIME IN    *         
***********************************************************************         
                                                                                
PUTSRT   NTR1  ,                                                                
         CLI   SORTSW,0                                                         
         BNE   PUTSRT02                                                         
         MVI   SORTSW,1                                                         
         LA    R0,L'REC                                                         
         CLI   SCANNING,C'Y'                                                    
         BNE   *+8                                                              
         CLI   SHOWDATA,C'Y'                                                    
         BNE   *+8                                                              
         LA    R0,L'REC+L'CARD                                                  
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SORTRECL,DUB                                                     
         BCTR  R0,0                                                             
         STH   R0,SORTRECX                                                      
         GOTO1 VSORTER,DMCB,SORTCARD,SORTRECD                                   
*                                                                               
PUTSRT02 GOTO1 VSORTER,DMCB,SORTPUT,SORTREC                                     
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
ASCANTAB DC    A(SCANTAB)                                                       
ASCANTAX DC    A(SCANTABX)                                                      
AUNUSED  DC    A(UNUSDTAB)                                                      
AUNUSEDX DC    A(UNUSDTBX)                                                      
VSORTER  DC    V(SORTER)                                                        
VPANIC   DC    V(PANIC)                                                         
VSTXITER DC    V(STXITER)                                                       
VCARDS   DC    V(CARDS)                                                         
VPRINTER DC    V(PRINTER)                                                       
VCPRINT  DC    V(CPRINT)                                                        
                                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
SAVERER1 DS    4A                                                               
FIRSTLIN DS    C                                                                
GETPAN   DS    C                   FLAG TO INIT DIR LIST FROM PANTAPE           
SCANNING DC    C'N'                                                             
SHOWDATA DC    C'N'                                                             
SORTSW   DC    AL1(0)                                                           
PANBOOK  DS    CL10                FLAG TO INIT DIR LIST FROM PANTAPE           
DONTUSE  DS    CL10                NEEDED FOR -VE DISP ON CONT. CARDS           
REQOPTS  DC    CL25' '                                                          
WORK     DS    CL32                                                             
                                                                                
SORTREC  DS    0X                                                               
REC      DS    CL28                THIS IS THE SORT RECORD                      
CARD     DS    CL80                AND THE PAN CARD                             
                                                                                
SEQ      DC    PL4'0'                                                           
LASTREC  DS    CL28                                                             
REP      DC    CL21' '                                                          
SAVEDIR  DC    CL80' '                                                          
DICTPRFX DC    CL3' '                                                           
BOOKPRFX DC    CL8' '                                                           
BOOKPFIL DC    X'00'               INPUT LENGTH                                 
PHASELST DC    X'00'                                                            
NOTUSED  DC    CL82'****************** THIS DICTIONARY ENTRY IS NOT CUR>        
               RENTLY USED *******************'                                 
DISABLED DC    CL1' '                                                           
BYTE     DC    X'00'                                                            
                                                                                
PANXIND  DC    X'00'               INDICATOR BYTE                               
ASMBKQ   EQU   X'80'               BOOK IS ASM TYPE                             
SCRBKQ   EQU   X'40'               BOOK IS SCREEN (DATA) TYPE                   
CONTDQ   EQU   X'08'               CONTINUATION CARD                            
                                                                                
SCSVLNQ  EQU   10                  L'SAVED FOR DD ROUTINE CONT. CARDS           
DDSVLNQ  EQU   8                   L'SAVED FOR SCAN ROUTINE CONT. CARDS         
                                                                                
***********************************************************************         
* LOAD MULTIPLE VALUES FOR SEARCH ROUTINES                            *         
* FORMAT OF AN ENTRY: AL1(PANXIND VALUE) - SEE ABOVE                  *         
*                     AL3(START CARD COL FOR SEARCH)                  *         
*                     A(NO. OF COLS IN CARD TO INSPECT)               *         
*                     A(CARD COL) TO INSPECT FOR CONTINUATION CHARACTER         
*                     A(CARD COL) FOR SAVE IF CONTINUATION CARD FOLLOWS         
***********************************************************************         
                                                                                
         DS    0D                                                               
SCSRCHT  DS    0XL16                                                            
         DC    AL1(ASMBKQ),AL3(CARD)                                            
         DC    A(71)                                                            
         DC    A(CARD+71,CARD+71-SCSVLNQ)                                       
*                                                                               
         DC    AL1(ASMBKQ+CONTDQ),AL3(CARD+15-SCSVLNQ)                          
         DC    A(55)                                                            
         DC    A(CARD+71,CARD+71-SCSVLNQ)                                       
*                                                                               
         DC    AL1(SCRBKQ),AL3(CARD+30)                                         
         DC    A(50)                                                            
         DC    A(CARD+29,CARD+80-SCSVLNQ)                                       
*                                                                               
         DC    AL1(SCRBKQ+CONTDQ),AL3(CARD+30-SCSVLNQ)                          
         DC    A(58)                                                            
         DC    A(CARD+29,CARD+80-SCSVLNQ)                                       
*                                                                               
DDSRCHT  DS    0XL16                                                            
         DC    AL1(ASMBKQ),AL3(CARD)                                            
         DC    A(71)                                                            
         DC    A(CARD+71,CARD+71-SCSVLNQ)                                       
*                                                                               
         DC    AL1(ASMBKQ+CONTDQ),AL3(CARD+15-DDSVLNQ)                          
         DC    A(55)                                                            
         DC    A(CARD+71,CARD+71-DDSVLNQ)                                       
*                                                                               
         DC    AL1(SCRBKQ),AL3(CARD+30)                                         
         DC    A(50)                                                            
         DC    A(CARD+29,CARD+80-DDSVLNQ)                                       
*                                                                               
         DC    AL1(SCRBKQ+CONTDQ),AL3(CARD+30-DDSVLNQ)                          
         DC    A(58)                                                            
         DC    A(CARD+29,CARD+80-DDSVLNQ)                                       
                                                                                
SORTCARD DC    C'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1 '                        
SORTRECD DC    C'RECORD TYPE=F,LENGTH='                                         
SORTRECL DC    C'XXX'                                                           
SORTFILL DC    C' '                                                             
SORTPUT  DC    C'PUT'                                                           
SORTGET  DC    C'GET'                                                           
SORTRECX DS    H                   L'SORT RECORD-1 IN BINARY                    
                                                                                
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,RECFM=FB,MACRF=(GM,PM),         X        
               BLKSIZE=3120,LRECL=80,EODAD=PANDIRX                              
                                                                                
DUMPLIST DS    0F                                                               
         DC    A(PANXREF)                                                       
         DC    V(DUMMY)                                                         
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
                                                                                
SCANMAX  EQU   10                  MAX WIDTH OF SCAN STRING                     
SCANTAB  DC    512X'00'                                                         
SCANTABX EQU   *                                                                
                                                                                
UNUSDTAB DC    1500CL10' '         TABLE OF UNUSED DICTIONARY ITEMS             
UNUSDTBX EQU   *                                                                
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DDPANXREF 01/13/12'                                      
         END                                                                    
