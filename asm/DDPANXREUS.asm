*          DATA SET DDPANXREUS AT LEVEL 028 AS OF 05/01/02                      
*PHASE PANXREFA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE PANREAD                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SIXUNPK                                                                
*INCLUDE SORTER                                                                 
*INCLUDE STXITER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
PANXREF  TITLE 'PANXREF - FAST XREF FROM PAN TAPE'                              
PANXREF  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**XREF**,=V(REGSAVE)                                           
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         MVC   TITLE+12(32),=C'PANVALET LIBRARY CROSS-REFERENCE'                
         MVC   MID1(28),=C'LIST OF ++INCLUDE STATEMENTS'                        
         MVC   SUB1(17),=C'BOOK NAME   LEVEL'                                   
         MVC   SUB2(17),=C'---------   -----'                                   
         MVC   SUB1+031(14),=C'---INCLUDES---'                                  
         MVC   SUB1+052(14),=C'---INCLUDES---'                                  
         MVC   SUB1+073(14),=C'---INCLUDES---'                                  
         MVC   SUB1+094(14),=C'---INCLUDES---'                                  
         MVC   SUB1+115(14),=C'---INCLUDES---'                                  
         MVC   SUB2+031(14),=C'NAME       SEQ'                                  
         MVC   SUB2+052(14),=C'NAME       SEQ'                                  
         MVC   SUB2+073(14),=C'NAME       SEQ'                                  
         MVC   SUB2+094(14),=C'NAME       SEQ'                                  
         MVC   SUB2+115(14),=C'NAME       SEQ'                                  
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         SPACE 3                                                                
GETCARD  GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         CHECK FOR END OF SELECTION FILE              
         BE    GETTAPE             YES- READ THE PAN BACKUP TAPE                
*                                                                               
         CLC   =C'SCAN=',CARD      SCAN=XYZ (SCAN FOR STRING XYZ)               
         BNE   *+14                                                             
         MVC   SCANWORD,CARD+5                                                  
         B     GETCARD                                                          
*                                                                               
         CLC   =C'DICT=',CARD      DICT=SP (SPOT DATA DICTIONARY XREF)          
         BNE   GETCARD                                                          
         MVC   DICTPRFX,CARD+5                                                  
         MVI   DICTPRFX+2,C'#'     SPECIAL DATA DICTIONARY CHARACTER            
         MVC   MID1(42),=C'BOOKS WHICH INCLUDE XXX DICTIONARY EQUATES'          
         MVC   MID1+20(3),DICTPRFX                                              
         B     GETCARD                                                          
         EJECT                                                                  
***********************************************************************         
*        READ PAN TAPE AND PRODUCE REPORT ON PAN BOOK ATTRIBUTES      *         
***********************************************************************         
         SPACE 1                                                                
GETTAPE  GOTO1 =V(PANREAD),DMCB,CARD                                            
         CLI   DMCB,X'01'          SEE IF CARD IS DIRECTORY ITEM                
         BNE   GT10                                                             
         BAS   RE,ANYREP           OUTPUT ANYTHING IN THE PRINT LINE            
         ZAP   SEQ,=P'0'           CLEAR SEQUENCE NUMBER                        
         MVC   SAVEDIR,CARD                                                     
         CLC   SAVEDIR+10(3),=C'000'                                            
         BE    GETTAPE             IGNORE SUPERSETS, GO GET NEXT CARD           
*                                                                               
         MVC   P(10),SAVEDIR       ENTER BOOK NAME ON PRINT LINE                
         MVC   P+13(3),SAVEDIR+10  ENTER BOOK LEVEL ON PRINT LINE               
         XC    REC,REC                                                          
         MVC   REC(10),SAVEDIR                                                  
         MVC   REC+20(3),SAVEDIR+10                                             
         GOTO1 =V(SORTER),DMCB,=C'PUT',REC                                      
         B     GETTAPE             GO GET NEXT CARD                             
*                                                                               
GT10     CLI   DMCB,X'03'          SEE IF CARD IS A DATA ITEM                   
         BL    GETTAPE             NO -- IT'S A PAN COMMENT                     
         BH    TAPEEOF             NO -- END OF THE TAPE                        
         CLC   CARD(19),=C'*          DATA SET'                                 
         BE    GETTAPE             IGNORE DATA SETS, GO GET NEXT CARD           
         CLC   SAVEDIR+10(3),=C'000'                                            
         BE    GETTAPE             IGNORE SUPERSETS, GO GET NEXT CARD           
*                                                                               
         AP    SEQ,=P'1'           ADD 1 TO THE SEQUENCE NUMBER                 
         CLC   SCANWORD,SPACES     WAS AN ALTERNATE SCAN STRING GIVEN?          
         BNE   GT20                YES                                          
         CLC   DICTPRFX,SPACES     WAS A DICTIONARY PREFIX GIVEN?               
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
         GOTO1 =V(SORTER),DMCB,=C'PUT',REC                                      
         B     GETTAPE             GO GET NEXT CARD                             
         EJECT                                                                  
GT20     LA    R2,SCANWORD+19      OPTION TO LOOK FOR WORD                      
         LA    R3,19               FIND L'WORD                                  
*                                                                               
         CLI   0(R2),C' '          SEE IF CHARACTER IS BLANK                    
         BNE   *+10                NO- THIS IS THE END OF THE WORD              
         BCTR  R3,0                YES- DECREMENT THE LENGTH BY ONE             
         BCT   R2,*-10                  REPEAT THIS FOR THE PREV CHAR           
*                                                                               
         LA    R2,CARD             NOW LOOK FOR WORD                            
         LA    R4,79                                                            
         SR    R4,R3               R4 = HOW MUCH OF THE CARD TO CHECK           
*                                                                               
GT30     EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SCANWORD                                                 
         BE    *+16                THE CARD CONTAINS THE WORD                   
         LA    R2,1(R2)                                                         
         BCT   R4,GT30                                                          
         B     GETTAPE             CARD DOESNT CONTAIN WORD, GET NEXT           
*                                                                               
         MVC   REC(10),SCANWORD    FORM A SORT RECORD FOR SCAN WORD             
         MVC   REC+10(13),SAVEDIR                                               
         MVC   REC+23(4),SEQ                                                    
         GOTO1 =V(SORTER),DMCB,=C'PUT',REC                                      
         B     GETTAPE             GO GET NEXT CARD                             
         EJECT                                                                  
GT40     LA    R2,CARD                                                          
         LA    R4,76               80 MINUS 4 FOR 'DD#X'                        
*                                                                               
         CLC   DICTPRFX,0(R2)      MATCH ON PREFIX?                             
         BE    *+16                YES                                          
         LA    R2,1(R2)                                                         
         BCT   R4,*-14                                                          
         B     GETTAPE             NO MATCH, GO GET THE NEXT CARD               
*                                                                               
         MVC   REC(10),SPACES                                                   
         LA    R4,REC                                                           
GT50     MVC   0(1,R4),0(R2)       PUT WHOLE EQUATED SYMBOL INTO RECORD         
         LA    R2,1(R2)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R2),C'#'          '#' IS PART OF SYMBOL                        
         BE    GT50                                                             
         CLI   0(R2),C'$'          '$' IS PART OF SYMBOL                        
         BE    GT50                                                             
         CLI   0(R2),C'@'          '@' IS PART OF SYMBOL                        
         BE    GT50                                                             
         CLI   0(R2),C'A'          ALPHANUMERICS ARE PART OF SYMBOL             
         BNL   GT50                                                             
*                                                                               
         MVC   REP,SPACES                                                       
         MVC   REP+5(10),REC                                                    
         EDIT  (P4,SEQ),(5,REP+16),ALIGN=LEFT                                   
         BAS   RE,FORMAT           PUT 'REP' INFO INTO PRINT LINE               
         MVC   REC+10(13),SAVEDIR  FORM SORT RECORD FOR DICT ITEM               
         MVC   REC+23(4),SEQ                                                    
         GOTO1 =V(SORTER),DMCB,=C'PUT',REC                                      
         B     GETTAPE             GO GET NEXT RECORD                           
         EJECT                                                                  
***********************************************************************         
*        PRODUCE ON REPORT DICT REFERENCES OR INCLUDE FILES(DEFAULT)  *         
***********************************************************************         
         SPACE 1                                                                
TAPEEOF  BAS   RE,ANYREP           TAPE END - CLEAR OUT PRINT LINE              
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID1(32),=C'LOCATION OF ++INCLUDE STATEMENTS'                    
         CLC   DICTPRFX,SPACES     WAS A DICTIONARY PREFIX GIVEN?               
         BE    *+16                NO                                           
         MVC   MID1(42),=C'LIST OF XXX DATA DICTIONARY EQUATES       '          
         MVC   MID1+8(3),DICTPRFX                                               
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB1(4),=C'NAME'                                                 
         MVC   SUB2(4),=C'----'                                                 
         MVC   SUB1+026(18),=C'---INCLUDED BY----'                              
         MVC   SUB1+047(18),=C'---INCLUDED BY----'                              
         MVC   SUB1+068(18),=C'---INCLUDED BY----'                              
         MVC   SUB1+089(18),=C'---INCLUDED BY----'                              
         MVC   SUB1+110(18),=C'---INCLUDED BY----'                              
         MVC   SUB2+026(18),=C'NAME       LVL SEQ'                              
         MVC   SUB2+047(18),=C'NAME       LVL SEQ'                              
         MVC   SUB2+068(18),=C'NAME       LVL SEQ'                              
         MVC   SUB2+089(18),=C'NAME       LVL SEQ'                              
         MVC   SUB2+110(18),=C'NAME       LVL SEQ'                              
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'75'                                                      
         XC    REC,REC                                                          
         SPACE 3                                                                
         LA    R3,UNUSDTAB         R3 A(TABLE FOR UNUSED ITEMS)                 
GETSORT  MVC   LASTREC,REC                                                      
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BNZ   GS10                SEE IF ANYMORE ITEMS LEFT                    
         BAS   RE,ANYREP           EMPTY OUT REMNANTS FROM PRINT LINE           
         CLC   DICTPRFX,SPACES                                                  
         BE    ENDREP                                                           
         MVC   MID1(42),=C'LIST OF UNUSED XXX DATA DICTIONARY EQUATES'          
         MVC   MID1+15(3),DICTPRFX                                              
         XC    SUB1,SUB1                                                        
         XC    SUB2,SUB2                                                        
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'75'                                                      
         XC    REP,REP                                                          
         MVC   P,SPACES                                                         
         LA    R3,UNUSDTAB         R3=A(START OF UNUSED ITEM TBLE)              
GETUNUSD CLC   0(10,R3),SPACES     SEE IF ANY MORE UNUSED ITEMS                 
         BE    ENDREP              NO-END OF REPORT                             
         MVC   REP(10),0(R3)       YES-PLACE THE SAID ITEM IN REP               
         BAS   RE,FORMAT           FORMAT THE PRINT LINE                        
         LA    R3,10(R3)           BUMP TO NEXT ENTRY                           
         B     GETUNUSD                                                         
ENDREP   BAS   RE,ANYREP           EMPTY OUT REMNANTS FROM PRINTLINE            
         XBASE                                                                  
*                                                                               
GS10     MVC   REC,0(R2)           SORT RECORD                                  
         CLC   REC(10),LASTREC     FIRST FOR NEW BOOK?                          
         BE    GS20                                                             
         CLC   DICTPRFX,SPACES     IF DICT REPORT FORM LIST OF UNUSED           
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
GS12     BAS   RE,ANYREP                                                        
         MVI   FIRSTLIN,C'Y'                                                    
         MVC   P,SPACES                                                         
         MVC   P(10),REC                                                        
         CLI   REC+10,0            SHOULD BE A DIRECTORY HEADER                 
         BE    GETSORT                                                          
         CLC   DICTPRFX,SPACES     WAS A DICTIONARY PREFIX GIVEN?               
         BNE   GS20                YES                                          
         MVC   P+11(9),=C'*MISSING*'                                            
*                                                                               
GS20     MVC   REP,SPACES                                                       
         MVC   REP(10),REC+10      INCLUDE BOOK                                 
         MVC   REP+11(3),REC+20            LEVEL                                
*                                          SEQUENCE                             
         EDIT  (P4,REC+23),(5,REP+15),ALIGN=LEFT                                
         BAS   RE,FORMAT                                                        
         B     GETSORT                                                          
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INSERT DATA TO PRINTLINE AND OUTPUT WHEN FULL     *         
***********************************************************************         
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         CLC   P+110(21),SPACES    IS PRINT LINE FULL                           
         BE    *+24                                                             
         L     RF,=V(PRINTER)      YES OUTPUT LINE AND START NEW ONE            
         BASR  RE,RF                                                            
         MVC   P+26(21),REP                                                     
         MVI   FIRSTLIN,C'N'                                                    
         B     XIT                                                              
*                                                                               
         CLC   P+26(21),SPACES                                                  
         BNE   *+14                                                             
         MVC   P+26(21),REP                                                     
         B     XIT                                                              
*                                                                               
         CLC   P+47(21),SPACES                                                  
         BNE   *+14                                                             
         MVC   P+47(21),REP                                                     
         B     XIT                                                              
*                                                                               
         CLC   P+68(21),SPACES                                                  
         BNE   *+14                                                             
         MVC   P+68(21),REP                                                     
         B     XIT                                                              
*                                                                               
         CLC   P+89(21),SPACES                                                  
         BNE   *+14                                                             
         MVC   P+89(21),REP                                                     
         B     XIT                                                              
*                                                                               
         MVC   P+110(21),REP                                                    
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO PRINT                                                 
         SPACE 2                                                                
ANYREP   NTR1                                                                   
         CLC   P+26(84),SPACES                                                  
         BE    XIT                                                              
         GOTO1 =V(PRINTER)         OUTPUT PRINT LINE                            
         GOTO1 (RF)                OUTPUT BLANK LINE                            
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
FIRSTLIN DS    C                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
CARD     DS    CL80                                                             
WORK     DS    CL32                                                             
REC      DS    CL28                                                             
SCANWORD DC    CL20' '                                                          
SEQ      DC    PL4'0'                                                           
LASTREC  DS    CL28                                                             
REP      DC    CL21' '                                                          
SAVEDIR  DC    CL80' '                                                          
DICTPRFX DC    CL3' '                                                           
NOTUSED  DC    CL82'****************** THIS DICTIONARY ENTRY IS NOT CUR>        
               RENTLY USED *******************'                                 
         SPACE 3                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=28'                                    
         SPACE 1                                                                
DUMPLIST DS    0F                                                               
         DC    A(PANXREF)                                                       
         DC    V(DUMMY)                                                         
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
UNUSDTAB DC    500CL10' '          TABLE OF UNUSED DICTIONARY ITEMS             
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028DDPANXREUS05/01/02'                                      
         END                                                                    
