*          DATA SET ACPRO36    AT LEVEL 013 AS OF 09/12/02                      
*PHASE T60B36A,*                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B36 - PANEL FIELD MAINTENANCE'                               
T60B36   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B36**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R2,RELO                                                          
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BE    FIELD2                                                           
         CLI   MODE,VALREC                                                      
         BE    FIELD4                                                           
         B     XIT                                                              
*                                                                               
* VALKEY LOGIC                                                                  
*                                                                               
FIELD2   LA    RE,LOCAL                                                         
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VALHED                                                        
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES                                          
         B     XIT                                                              
*                                                                               
* VALREC LOGIC-DISPLAY OR EDIT                                                  
*                                                                               
FIELD4   GOTO1 CALLOV,DMCB,0,X'D9000A5D'  GET V(TSAR)                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTSAR,0(R1)                                                      
*                                                                               
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    FIELD6                                                           
*                                                                               
         BAS   RE,PROCPF           INSPECT THE PF KEYS                          
         BAS   RE,TSTEDT           TEST ANYTHING TO EDIT                        
         BE    FIELD30             YES                                          
*                                                                               
         CLI   PFKEY,PF4           TEST PF4=SAVE                                
         BE    FIELD40             YES                                          
         CLI   PFKEY,PF9           TEST PF9=ERASE MOVE/COPY DETAILS             
         BE    FIELD50                                                          
         CLI   PFKEY,PF10          TEST PF10=CHECK SCREEN SPECIFICATION         
         BE    FIELD60                                                          
*                                                                               
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
*                                                                               
* DISPLAY LOGIC                                                                 
*                                                                               
FIELD6   GOTO1 VCLEARF,DMCB,PROSEL1H,PROLAST                                    
         GOTO1 (RF),(R1),(1,PROSEL1H),PROPFH                                    
         LA    R2,PROSEL1H         R2=A(FIELD HEADER)                           
         LA    R1,PROPFH-1         R1=BXLE LIMIT                                
         SR    R0,R0               R0=INCREMENT=FIELD LENGTH                    
         OI    4(R2),X'20'         TURN ON ALL PREV VALID BITS                  
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,*-8                                                        
*                                                                               
         MVI   LNRECS,0                                                         
         LA    RE,LSELTAB                                                       
         LA    RF,L'LSELTAB                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING LINKD,R5                                                         
         CLI   INTMODE,DISLIST                                                  
         BE    FIELD8                                                           
*                                                                               
* LOGIC FOR FIRST TIME FOR PANEL                                                
*                                                                               
FIELD7   BAS   RE,INIT             INITIALIZE THE TSAR DATASET                  
         LA    R5,LINKLST          START DISPLAY AT BEGINNING                   
         B     FIELD15                                                          
*                                                                               
FIELD8   BAS   RE,REST             RESTORE TSAR DATASET                         
*                                                                               
* LOGIC FOR ENTER PRESSED                                                       
*                                                                               
         CLI   PFKEY,0             TEST FOR ENTER                               
         BNE   FIELD9              NO                                           
         GOTO1 FINDENT,STARTREC                                                 
         B     FIELD15                                                          
*                                                                               
* LOGIC FOR PF5=TOP                                                             
*                                                                               
FIELD9   CLI   PFKEY,PF5           TEST PF5=TOP                                 
         BNE   FIELD10             NO                                           
         LA    R5,LINKLST          R5=A(ANCHOR)                                 
         B     FIELD15             DO DISPLAY                                   
*                                                                               
* LOGIC FOR PF8=NEXT PAGE                                                       
*                                                                               
FIELD10  CLI   PFKEY,PF8           TEST PF8=NEXT                                
         BNE   FIELD13             NO                                           
*                                                                               
         GOTO1 FINDENT,LASTREC                                                  
         SR    R4,R4                                                            
         ICM   R4,1,SCROLL                                                      
         BZ    FIELD12             NO SCROLL AMOUNT GIVEN                       
         GOTO1 FINDENT,STARTREC                                                 
*                                                                               
FIELD11  CLI   LINKNEXT,1          TEST NEXT ENTRY=ANCHOR                       
         BE    FIELD12                                                          
         ZIC   RE,LINKNEXT                                                      
         BCTR  RE,0                ADVANCE AHEAD BY SCROLL AMOUNT               
         MH    RE,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(RE)                                                   
         BCT   R4,FIELD11                                                       
*                                                                               
FIELD12  CLI   LINKNEXT,1          TEST IF NEXT ENTRY=ANCHOR                    
         BNE   FIELD15             NO                                           
         ZIC   RE,LINKPREV         YES-BACK UP ONE ENTRY                        
         BCTR  RE,0                                                             
         MH    RE,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(RE)                                                   
         B     FIELD15             NOW DO DISPLAY                               
*                                                                               
* LOGIC FOR PF6=BOTTOM AND PF7=PREVIOUS                                         
*                                                                               
FIELD13  LA    R5,LINKLST          R5=A(ANCHOR)                                 
         LA    R0,MAXRECS+1        R0=LOOP COUNTER                              
         CLI   PFKEY,PF6           TEST PF6=BOTTOM                              
         BE    FIELD14             YES                                          
*                                                                               
         GOTO1 FINDENT,STARTREC    MUST BE PF7=PREV                             
         OC    LINKRNUM,LINKRNUM   TEST FOR ANCHOR                              
         BZ    FIELD15             YES-CANNOT GO BACK PAST IT                   
         SR    R0,R0                                                            
         ICM   R0,1,SCROLL         COUNTER=SCROLL AMOUNT                        
         BNZ   *+8                                                              
         LA    R0,MAXRECS          DEFAULT=MAXRECS                              
*                                                                               
FIELD14  ZIC   RE,LINKPREV                                                      
         BCTR  RE,0                                                             
         MH    RE,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(RE)                                                   
         OC    LINKRNUM,LINKRNUM                                                
         BZ    FIELD15                                                          
         BCT   R0,FIELD14                                                       
*                                                                               
FIELD15  GOTO1 DISP,PARAS,LINKD                                                 
         BNE   FIELD17                                                          
*                                                                               
         CLI   INTMODE,FSTLIST     TEST FIRST TIME FOR PANEL                    
         BE    FIELD16             YES                                          
         LA    R2,PROPANH                                                       
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'NONEMSG),NONEMSG                                       
         B     FIELDX                                                           
*                                                                               
FIELD16  LA    R2,PROSEL1H                                                      
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'ENTERMSG),ENTERMSG                                     
         B     FIELDX                                                           
*                                                                               
* SET OUTPUT MESSAGE FOR DISPLAY OF SOME ITEMS                                  
*                                                                               
FIELD17  LA    R2,PROSEL1H         PUT CURSOR AT FIRST SELECT                   
         ST    R2,ACURFORC                                                      
*                                                                               
         LA    R5,LINKLST                                                       
         SR    R3,R3               R3=COUNT OF ACTUAL ENTRIES                   
*                                                                               
FIELD18  ZIC   RE,LINKNEXT         GET NEXT ENTRY                               
         BCTR  RE,0                                                             
         MH    RE,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(RE)                                                   
         OC    LINKRNUM,LINKRNUM   TEST FOR ANCHOR                              
         BZ    FIELD19             YES-AT THE END                               
         LA    R3,1(R3)            INCREMENT COUNT                              
         CLC   LINKRNUM,FIRSTREC                                                
         BNE   *+8                                                              
         ST    R3,FULL             SAVE N'FIRST ENTRY                           
         B     FIELD18                                                          
*                                                                               
FIELD19  LA    R4,CONHEAD                                                       
         MVC   CONHEAD(5),=C'ENTRY'                                             
         LA    R4,CONHEAD+6                                                     
         CLI   LNRECS,1            TEST FOR ONLY ONE ENTRY                      
         BE    *+14                                                             
         MVC   CONHEAD(7),=C'ENTRIES'                                           
         LA    R4,CONHEAD+8                                                     
*                                                                               
         L     R0,FULL                                                          
         EDIT  (R0),(3,(R4)),ALIGN=LEFT                                         
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
*                                                                               
         CLI   LNRECS,1            TEST ONLY ONE ENTRY                          
         BE    FIELD20             YES                                          
         BCTR  R4,0                                                             
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         ZIC   R0,LNRECS                                                        
         A     R0,FULL             ADD IN FIRST ENTRY                           
         BCTR  R0,0                                                             
         EDIT  (R0),(3,(R4)),ALIGN=LEFT                                         
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
*                                                                               
FIELD20  MVC   0(2,R4),=C'OF'                                                   
         LA    R4,3(R4)                                                         
         EDIT  (R3),(3,(R4)),ALIGN=LEFT                                         
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         MVC   0(9,R4),=C'DISPLAYED'                                            
         LA    R4,10(R4)                                                        
         OC    MOVEDETS,MOVEDETS   TEST PENDING MOVE/COPY                       
         BZ    FIELD22             NO                                           
         MVC   0(L'PENDMSG,R4),PENDMSG                                          
*                                                                               
FIELD22  B     FIELDX                                                           
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
FIELD30  BAS   RE,REST             RESTORE THE TSAR DATASET                     
         BAS   RE,EDT              EDIT THE SCREEN                              
         BAS   RE,REDISP           RE-DISPLAY SCREEN/INCLUDE INSERTS            
*                                                                               
FIELD32  OC    MOVEDETS,MOVEDETS   TEST FOR PENDING MOVE/COPY                   
         BNZ   *+8                 YES                                          
         BAS   RE,REORG            NO-OK TO REORGANIZE                          
         LA    R2,PROSEL1H                                                      
         MVC   CONHEAD(L'EDTMSG),EDTMSG                                         
*                                                                               
FIELD35  ST    R2,ACURFORC                                                      
         B     FIELDX                                                           
*                                                                               
* LOGIC FOR PF4=SAVE DATASET                                                    
*                                                                               
FIELD40  LA    R2,PROPANH                                                       
         ST    R2,ACURFORC                                                      
         BAS   RE,REST             RESTORE THE DATASET                          
         MVI   ERROR,SAVEERR                                                    
         OC    MOVEDETS,MOVEDETS   TEST PENDING MOVE                            
         BNZ   ERREND                                                           
*                                                                               
         BAS   RE,REORG                                                         
         GOTO1 =A(SAVE),DMCB,(RC),RR=RELO                                       
         MVC   CONHEAD(L'SAVEMSG),SAVEMSG                                       
         B     FIELDX                                                           
*                                                                               
* LOGIC FOR PF9=ERASE MOVE/COPY DETAILS                                         
*                                                                               
FIELD50  BAS   RE,REST                                                          
         MVI   ERROR,NOMOVERR                                                   
         LA    R2,PROPANH                                                       
         OC    MOVEDETS,MOVEDETS   TEST ANY MOVE/COPY DETAILS                   
         BZ    ERREND              NO                                           
*                                                                               
         XC    MOVEDETS,MOVEDETS                                                
         BAS   RE,REDISP           RE-DISPLAY SCREEN                            
         MVC   CONHEAD(L'ERASEMSG),ERASEMSG                                     
         LA    R2,PROSEL1H                                                      
         ST    R2,ACURFORC                                                      
         B     FIELDX                                                           
*                                                                               
* LOGIC FOR PF10=CHECK SCREEN SPECIFICATION                                     
*                                                                               
FIELD60  BAS   RE,REST                                                          
         LA    R5,LINKLST                                                       
         USING LINKD,R5                                                         
         CLI   LINKNEXT,1          TEST FOR EMPTY LIST                          
         BNE   FIELD62             NO                                           
         MVC   CONHEAD(L'NONEMSG),NONEMSG                                       
         B     FIELD65                                                          
*                                                                               
FIELD62  MVC   REMUSER,TWAALIAS    USE FIRST 3 CHARS OF PERSON                  
         GOTO1 OPENPQ                                                           
         BAS   RE,CHECK            PRINT REPORT/PERFORM CHECK                   
         MVI   SPMODE,X'FF'        CLOSE REPORT                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
FIELD64  LA    R4,CONHEAD                                                       
         MVC   0(2,R4),=C'**'                                                   
         MVC   2(3,R4),SPOOLID                                                  
         MVI   5(R4),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,6(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         LA    R4,7(R4)            UPDATE POINTER                               
*                                                                               
         MVC   0(7,R4),=C'SPOOLED'                                              
         MVC   7(2,R4),=C'--'                                                   
         LA    R4,9(R4)                                                         
*                                                                               
         MVC   0(14,R4),=C'SCREEN IS OK**'                                      
         CLI   ERRSW,C'Y'          TEST FOR ANY ERROR                           
         BNE   *+10                                                             
         MVC   0(21,R4),=C'SCREEN HAS AN ERROR**'                               
*                                                                               
FIELD65  LA    R2,PROPANH                                                       
         ST    R2,ACURFORC                                                      
         B     FIELDX                                                           
*                                                                               
FIELDX   XMOD1 1                                                                
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                                   
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'         INITIALIZE KEY FIELD CHANGE SWITCH           
         MVI   OPTION,0            NO NAME FIELDS TO BE SHOWN                   
*                                                                               
VALHED2  LA    R2,PROPANH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 ANY                                                              
         MVC   QPANEL,WORK                                                      
         LA    R4,KEY                                                           
         USING ACPNKEY,R4                                                       
         XC    ACPNKEY,ACPNKEY                                                  
         MVI   ACPNRTYP,ACPNEQU                                                 
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   ACPNCODE,QPANEL                                                  
         GOTO1 READ                                                             
*                                                                               
VALHED4  LA    R2,PROSCRLH         EDIT SCROLL FIELD                            
         CLI   5(R2),0                                                          
         BE    VALHED6                                                          
         GOTO1 VALINUM                                                          
         MVC   SCROLL,ACTUAL                                                    
*                                                                               
VALHED6  OI    PROPANH+4,X'20'     TURN ON VALID BIT                            
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO RESTORE THE TSAR DATASET                                       
*                                                                               
REST     ST    RE,SAVERE                                                        
         LA    RE,TSARBLK                                                       
         LA    RF,L'TSARBLK                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSNBUF,1                                                         
         LA    RE,BUFF                                                          
         ST    RE,TSABUF                                                        
         MVC   TSAREC,AIO2                                                      
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGL,1            LOWEST TWA SAVE PAGE                         
         MVI   TSPAGN,2            N'TWA SAVE PAGES                             
         MVI   TSKEYL,2            PASS KEY LENGTH                              
         MVI   TSACTN,TSARES       ACTION=RESTORE                               
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                BLOW UP ON ANY ERROR RESTORING               
*                                                                               
RESTX    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                            
*                                                                               
* ON EXIT, CC=EQ TO EDIT, CC=NEQ TO CONTINUE DISPLAY                            
*                                                                               
TSTEDT   ST    RE,SAVERE                                                        
         LA    R2,PROSEL1H         R2=A(FIRST SELECT FIELD)                     
*                                                                               
TSTEDT2  TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BZ    TSTEDTY             YES                                          
         BAS   RE,BUMPTOUN         NEXT UNPROTECTED FIELD                       
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   TSTEDT2                                                          
*                                                                               
TSTEDTN  LTR   RB,RB               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CR    RB,RB               SET CC=EQ                                    
*                                                                               
TSTEDTX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO PREVIEW THE PFKEYS                                             
*                                                                               
PROCPF   CLI   PFKEY,PF4           TEST PF4-PF10                                
         BL    *+12                                                             
         CLI   PFKEY,PF10                                                       
         BNH   *+8                                                              
         MVI   PFKEY,0             CLEAR OUT PFKEY INPUT                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO PERFORM A DISPLAY                                    *         
* AT ENTRY, P1=A(LINKED LIST ENTRY BEFORE DISPLAY START)              *         
* ON EXIT, CC=EQ IF NO ENTRIES DISPLAYED, NEQ IF ONE OR MORE DISPLAYED*         
***********************************************************************         
         SPACE 1                                                                
DISP     NTR1  ,                                                                
         L     R5,0(R1)            GET A(LINKED LIST ENTRY)                     
         USING LINKD,R5                                                         
         MVC   STARTREC,LINKRNUM                                                
         XC    FIRSTREC,FIRSTREC                                                
         XC    LASTREC,LASTREC                                                  
         LA    R2,PROSEL1H                                                      
         ST    R2,ATHISLIN                                                      
         LA    R3,LSELTAB                                                       
         USING SELTABD,R3                                                       
*                                                                               
DISP2    ZIC   RE,LINKNEXT         GET N'NEXT LIST ENTRY                        
         BCTR  RE,0                                                             
         MH    RE,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(RE)                                                   
         OC    LINKRNUM,LINKRNUM   TEST FOR ANCHOR                              
         BZ    DISP8               YES-STOP DISPLAY                             
*                                                                               
         L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN           SET ADCONS                                   
         BAS   RE,DISSEL                                                        
*                                                                               
DISP4    L     R4,AIO1             BUILD TSAR RECORD KEY                        
         XC    0(4,R4),0(R4)                                                    
         MVC   2(2,R4),LINKRNUM                                                 
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH                                                    
         ST    R4,TSAREC                                                        
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(DISFLD),PARAS,(RC),(R4),RR=RELO                               
*                                                                               
DISP6    MVC   ATHISLIN,ANEXTSEL   RESET FOR NEXT LINE                          
         OC    FIRSTREC,FIRSTREC   TEST IF FIRST RECORD                         
         BNZ   *+10                                                             
         MVC   FIRSTREC,LINKRNUM   YES                                          
         MVC   LASTREC,LINKRNUM                                                 
         MVI   SELACT,C' '                                                      
         MVC   SELKEY,LINKRNUM                                                  
         ZIC   R1,LNRECS           INCREMENT COUNT OF RECS ON SCREEN            
         LA    R1,1(R1)                                                         
         STC   R1,LNRECS                                                        
         CLI   LNRECS,MAXRECS      TEST IF SCREEN IS FILLED                     
         BE    DISP8               YES                                          
         LA    R3,SELTABL(R3)                                                   
         B     DISP2                                                            
*                                                                               
DISP8    CLI   LNRECS,0            SET CC ON EXIT                               
*                                                                               
DISPX    B     XIT                                                              
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO PERFORM A CHANGE                                *              
* AT ENTRY, P1=A(LINKED LIST ENTRY), ELEM CONTAINS NEW FIELD     *              
*           ELEMENT                                              *              
******************************************************************              
         SPACE 1                                                                
CHANGE   NTR1  ,                                                                
         L     R5,0(R1)            GET A(LINKED LIST ENTRY)                     
         USING LINKD,R5                                                         
         LA    R6,ELEM                                                          
         USING ACFDD,R6                                                         
         L     R4,AIO1             R4=A(TSAR RECORD TO WRITE)                   
         MVC   2(2,R4),LINKRNUM    SET RECORD KEY                               
*                                                                               
CHANGE2  LA    RE,4(R4)            RE=A(ELEMENT POSITION IN RECORD)             
         ZIC   RF,ACFDLEN          RF=ELEMENT LENGTH                            
         LR    R2,RF               SAVE ELEM LENGTH IN R2                       
         LR    R0,R6               MOVE SOURCE                                  
         LR    R1,RF                                                            
         MVCL  RE,R0               SLOT ELEMENT INTO POSITION                   
         LA    R2,4(R2)            COMPUTE TSAR REC LENGTH                      
         STH   R2,0(R4)                                                         
*                                                                               
CHANGE4  LA    R1,TSARBLK          WRITE RECORD BACK TO DATASET                 
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAWRT                                                    
         ST    R4,TSAREC                                                        
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHANGEX  B     XIT                                                              
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO ADD A RECORD TO TSAR DATASET AND TO UPDATE    *                
* THE LINKED LIST ACCORDINGLY.                                 *                
* AT ENTRY, P1=A(LINKED LIST ENTRY TO ADD AFTER), ELEM         *                
*           CONTAINS NEW DATA ELEMENT                          *                
****************************************************************                
         SPACE 1                                                                
ADDR     NTR1  ,                                                                
         L     R5,0(R1)            GET A(AFTER LINKED LIST ENTRY)               
         USING LINKD,R5                                                         
         ST    R5,AAFTER           SAVE IT                                      
         LA    R6,ELEM                                                          
         USING ACFDD,R6                                                         
         L     R4,AIO1             R4=A(TSAR RECORD)                            
         LH    R3,HIREC            GET HIGHEST RECORD NUMBER                    
         LA    R3,1(R3)            INCREMENT IT                                 
         STH   R3,2(R4)            SET RECORD KEY                               
*                                                                               
ADD2     LA    RE,4(R4)            RE=A(FIELD DATA ELEMENT POSITION)            
         ZIC   RF,ACFDLEN                                                       
         LR    R2,RF               SAVE ELEMENT LENGTH IN R2                    
         LR    R0,R6                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0               SLOT ELEMENT IN RECORD                       
         LA    R2,4(R2)            COMPUTE RECORD LENGTH                        
         STH   R2,0(R4)                                                         
*                                                                               
ADD4     LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAADD                                                    
         ST    R4,TSAREC                                                        
         GOTO1 VTSAR,TSARD                                                      
         BE    ADD6                                                             
         CLI   TSERRS,TSEEOF       TEST IF FILE IS FILLED                       
         BE    ADDE                YES                                          
         DC    H'0'                TAKE A HIT IS ANYTHING ELSE                  
*                                                                               
ADD6     ZIC   R2,HIENT            GET HIGHEST ENTRY NUMBER                     
         LR    R5,R2                                                            
         LA    R2,1(R2)            NEXT HIGH ENTRY NUMBER                       
         CH    R2,=Y(MAXLST)       TEST AGAINST LINKED LIST OVERFLOW            
         BH    ADDE2               YES                                          
         MH    R5,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(R5)      R5=A(NEW LINKED LIST POSITION)               
         STC   R2,HIENT            SET NEW HIGHEST ENTRY NUMBER                 
         STH   R3,HIREC            SET NEW HIGHEST REC NUMBER                   
         STH   R3,LINKRNUM         RECORD NUM FOR NEW ENTRY                     
*                                                                               
ADD8     L     RE,AAFTER           RE=A(LINKED LIST TO ADD AFTER)               
         ZIC   R4,LINKNEXT-LINKD(RE)                                            
         STC   R4,LINKNEXT         SET NEXT POINTER IN NEW ENTRY                
         STC   R2,LINKNEXT-LINKD(RE) SET NEXT IN ADD AFTER                      
*                                                                               
         BCTR  R4,0                                                             
         MH    R4,=Y(L'LINKREC)                                                 
         LA    R4,LINKLST(R4)                                                   
         MVC   LINKPREV,LINKPREV-LINKD(R4) EXTRACT PREV FROM NEXT               
         STC   R2,LINKPREV-LINKD(R4) SET NEW ENTRY AS PREV IN NEXT              
*                                                                               
ADDX     B     XIT                                                              
*                                                                               
ADDE     MVI   ERROR,TEMPERR       FILE IS FILLED                               
         B     ADDEX                                                            
*                                                                               
ADDE2    MVI   ERROR,LINKERR       LINKED LIST IS FILLED                        
*                                                                               
ADDEX    L     R2,ASEL                                                          
         B     ERREND                                                           
         EJECT                                                                  
*******************************************************                         
* SUB-ROUTINE TO PERFORM A DELETE                     *                         
* AT ENTRY, P1=A(LINKED LIST ENTRY TO DELETE)         *                         
*******************************************************                         
         SPACE 1                                                                
DELETE   NTR1  ,                                                                
         L     R5,0(R1)            GET A(LINK LIST ENTRY TO DELETE)             
         USING LINKD,R5                                                         
         MVC   PREV,LINKPREV       EXTRACT ITS PREVIOUS AND                     
         MVC   NEXT,LINKNEXT       NEXT POINTERS                                
         L     R4,AIO1                                                          
         XC    0(4,R4),0(R4)                                                    
         MVC   2(2,R4),LINKRNUM    SET RECORD KEY                               
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSADEL       TO FORCE DELETE ON KEY,                      
         XC    TSRNUM,TSRNUM       ZERO REC NUM                                 
         ST    R4,TSAREC                                                        
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELETE2  ZIC   R5,PREV                                                          
         BCTR  R5,0                                                             
         MH    R5,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(R5)      A(PREVIOUS LINK ENTRY)                       
         MVC   LINKNEXT,NEXT       REMOVE NEXT POINTER TO DELETED ENTRY         
*                                                                               
         ZIC   R5,NEXT                                                          
         BCTR  R5,0                                                             
         MH    R5,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(R5)      A(NEXT LINK ENTRY)                           
         MVC   LINKPREV,PREV       ADJUST ITS BACKWARD POINTER                  
*                                                                               
DELETEX  B     XIT                                                              
         EJECT                                                                  
************************************************************                    
* SUB-ROUTINE TO PERFORM A COPY--CALLED FROM MAIN LINE     *                    
* OR MOVE LOGIC                                            *                    
************************************************************                    
         SPACE 1                                                                
COPY     NTR1  ,                                                                
         GOTO1 FINDENT,FROM        CHECK FROM ENTRY                             
         L     R4,AIO1                                                          
         XC    0(4,R4),0(R4)                                                    
         MVC   2(2,R4),FROM        FROM RECORD NUMBER                           
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH                                                    
         ST    R4,TSAREC                                                        
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COPY2    XC    ELEM,ELEM                                                        
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),4(R4)       EXTRACT ELEMENT FROM REC                     
*                                                                               
COPY4    GOTO1 FINDENT,LOCATION                                                 
         USING LINKD,R5                                                         
         CLI   POSITION,C'A'       TEST IF COPYING AFTER                        
         BE    COPY6               YES                                          
         ZIC   RE,LINKPREV         NO-COPYING BEFORE                            
         BCTR  RE,0                                                             
         MH    RE,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(RE)                                                   
*                                                                               
COPY6    GOTO1 ADDR,PARAS,LINKD                                                 
*                                                                               
COPYX    B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO FIND A LINKED LIST ENTRY GIVEN A RECORD NUMBER                 
*                                                                               
* ON ENTRY, R1=A(RECORD NUMBER)                                                 
*                                                                               
FINDENT  ST    RE,SAVERE                                                        
         LA    R5,LINKLST                                                       
         USING LINKD,R5                                                         
         ZIC   RE,HIENT            RE=LOOP COUNTER                              
*                                                                               
FINDENT2 CLC   LINKRNUM,0(R1)      MATCH ON RECORD NUMBER                       
         BE    FINDENTX                                                         
         LA    R5,L'LINKREC(R5)                                                 
         BCT   RE,FINDENT2                                                      
         DC    H'0'                                                             
*                                                                               
FINDENTX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
************************************************************                    
* SUB-ROUTINE TO PERFORM A MOVE                            *                    
************************************************************                    
         SPACE 1                                                                
MOVE     NTR1  ,                                                                
         USING LINKD,R5                                                         
         CLC   FROM,STARTREC       IS STARTREC BEING MOVED                      
         BNE   MOVE2               NO                                           
         GOTO1 FINDENT,STARTREC                                                 
         ZIC   RE,LINKPREV                                                      
         BCTR  RE,0                BACK UP ONE ENTRY FOR                        
         MH    RE,=Y(L'LINKREC)    NEW DISPLAY START                            
         LA    R5,LINKLST(RE)                                                   
         MVC   STARTREC,LINKRNUM                                                
*                                                                               
MOVE2    BAS   RE,COPY             COPY THE FROM RECORD FIRST                   
         GOTO1 FINDENT,FROM                                                     
         GOTO1 DELETE,PARAS,(R5)                                                
*                                                                               
MOVEX    B     XIT                                                              
         EJECT                                                                  
**********************************************************                      
* SUB-ROUTINE TO DISPLAY THE SELECT FIELD                *                      
* AT ENTRY, R5=A(LINKED LIST ENTRY) + SETLIN CALLED      *                      
**********************************************************                      
         SPACE 1                                                                
DISSEL   ST    RE,FULL                                                          
         USING LINKD,R5                                                         
         L     R2,ASEL                                                          
         OI    4(R2),X'20'         TURN ON PREVIOUSLY VALIDATED                 
         OC    MOVEDETS,MOVEDETS   TEST FOR PENDING MOVE DETAILS                
         BZ    DISSELX                                                          
         MVC   LISTAR,SPACES                                                    
         MVI   LISTAR,C'*'                                                      
         CLC   LINKRNUM,FROM       TEST IF MOVE/COPY FROM ITEM                  
         BE    DISSEL2                                                          
         CLC   LINKRNUM,LOCATION   TEST IF TO LOCATION                          
         BE    DISSEL4                                                          
         B     DISSELX                                                          
*                                                                               
DISSEL2  MVI   LISTAR+1,C'M'                                                    
         CLI   MOVECOPY,C'M'       TEST FOR MOVE                                
         BE    *+8                                                              
         MVI   LISTAR+1,C'C'       NO-ITS A COPY                                
         B     DISSEL6                                                          
*                                                                               
DISSEL4  MVC   LISTAR+1(1),POSITION                                             
*                                                                               
DISSEL6  BAS   RE,MOVEFLD                                                       
*                                                                               
DISSELX  L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
         SPACE 2                                                                
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
*                                                                               
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     MOVEFLDX                                                         
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO RE-DISPLAY THE SCREEN AND INCORPORATE INSERTS *                
* ON EXIT, CC=NEQ IF ANY RECORDS DISPLAYED, CC=EQ IF NONE      *                
****************************************************************                
         SPACE 1                                                                
REDISP   NTR1  ,                                                                
         GOTO1 VCLEARF,DMCB,PROSEL1H,PROLAST                                    
         GOTO1 (RF),(R1),(1,PROSEL1H),PROPFH                                    
         LA    R2,PROSEL1H         R2=A(FIELD HEADER)                           
         LA    R1,PROPFH-1         R1=BXLE LIMIT                                
         SR    R0,R0               R0=INCREMENT=FIELD LENGTH                    
         OI    4(R2),X'20'         TURN ON ALL PREV VALID BITS                  
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,*-8                                                        
*                                                                               
REDISP1  GOTO1 FINDENT,STARTREC                                                 
         USING LINKD,R5                                                         
         LA    R6,DISTAB           R6=A(DISPLAY TABLE)                          
         LA    R4,MAXRECS          R4=LOOP COUNTER                              
*                                                                               
* BUILD A DISPLAY TABLE AT DISTAB                                               
*                                                                               
REDISP2  ZIC   RE,LINKNEXT                                                      
         BCTR  RE,0                BUMP TO NEXT ENTRY                           
         MH    RE,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(RE)                                                   
         OC    LINKRNUM,LINKRNUM   TEST FOR ANCHOR                              
         BZ    REDISP10            YES-READY TO DO DISPLAY                      
*                                                                               
         MVC   0(L'LINKRNUM,R6),LINKRNUM SLOT IN TABLE ENTRY                    
         LA    R6,L'LINKRNUM(R6)                                                
         BCT   R4,REDISP6                                                       
*                                                                               
REDISP4  CLI   NINS,1              TEST FOR ONLY ONE INSERTION                  
         BNE   REDISP10            NO-DO DISPLAY NOW                            
         BAS   RE,LOOKINS          LOOK FOR INSERTION                           
         BNE   REDISP10            NO-NOT AFTER LAST ITEM                       
*                                                                               
         LA    RE,DISTAB                                                        
         LA    R1,DISTAB+2                                                      
         LA    R0,MAXRECS-1                                                     
         MVC   0(2,RE),0(R1)       SHIFT THE ENTRIES ONE                        
         LA    RE,2(RE)            POSITION TO THE LEFT                         
         LA    R1,2(R1)                                                         
         BCT   R0,*-14                                                          
         XC    0(2,RE),0(RE)       CLEAR LAST ENTRY                             
         B     REDISP10                                                         
*                                                                               
REDISP6  BAS   RE,LOOKINS          LOOK FOR INSERTION AFTER                     
         BNE   REDISP2             NO                                           
         LA    R6,L'LINKRNUM(R6)   YES-ADD EMPTY ENTRY                          
         BCT   R4,REDISP2                                                       
*                                                                               
* DISPLAY USING DISTAB                                                          
*                                                                               
REDISP10 XC    FIRSTREC,FIRSTREC                                                
         XC    LASTREC,LASTREC                                                  
         LA    R2,PROSEL1H                                                      
         ST    R2,ATHISLIN                                                      
         LA    R3,LSELTAB          R3=A(SELECT TABLE ENTRY)                     
         USING SELTABD,R3                                                       
         XC    SELTABD(MAXRECS*SELTABL),SELTABD                                 
         LA    R4,MAXRECS          R4=LOOP COUNTER                              
         LA    R6,DISTAB           R6=A(DISPLAY TABLE)                          
         MVI   LNRECS,0                                                         
*                                                                               
REDISP12 L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN           SET LINE ADDRESSES                           
*                                                                               
         OC    0(L'LINKRNUM,R6),0(R6) TEST FOR INSERTION                        
         BZ    REDISP15            YES                                          
*                                                                               
REDISP14 GOTO1 FINDENT,(R6)                                                     
         MVC   SELKEY,LINKRNUM     SET RECORD NUMBER                            
         BAS   RE,DISSEL           DISPLAY SELECT FIELD                         
         L     RE,AIO1                                                          
         XC    0(4,RE),0(RE)                                                    
         MVC   2(2,RE),LINKRNUM                                                 
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH                                                    
         ST    RE,TSAREC                                                        
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(DISFLD),PARAS,(RC),AIO1,RR=RELO  DISPLAY RECORD               
*                                                                               
         OC    FIRSTREC,FIRSTREC   TEST IF FIRST RECD                           
         BNZ   *+10                NO                                           
         MVC   FIRSTREC,LINKRNUM                                                
         MVC   LASTREC,LINKRNUM                                                 
         ZIC   R1,LNRECS           INCREMENT COUNT OF SCREEN RECS               
         LA    R1,1(R1)                                                         
         STC   R1,LNRECS                                                        
*                                                                               
REDISP15 MVC   ATHISLIN,ANEXTSEL                                                
         LA    R3,SELTABL(R3)                                                   
         LA    R6,L'LINKRNUM(R6)                                                
         BCT   R4,REDISP12                                                      
*                                                                               
REDISP20 CLI   LNRECS,0            SET CC ON EXIT                               
*                                                                               
REDISPX  B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO LOOK FOR AN INSERTION ENTRY                                    
* ON ENTRY, R6=A(RECORD NUMBER)                                                 
* ON EXIT, CC=EQ FOR INSERTION AFTER, NEQ FOR NO INSERTION AFTER                
*                                                                               
LOOKINS  ST    RE,SAVERE                                                        
         SR    R1,R1                                                            
         ICM   R1,1,NINS                                                        
         BZ    LOOKINSN                                                         
         LA    RE,INSTAB                                                        
*                                                                               
LOOKINS2 CLC   LINKRNUM,0(RE)                                                   
         BE    LOOKINSY                                                         
         LA    RE,L'LINKRNUM(RE)                                                
         BCT   R1,LOOKINS2                                                      
*                                                                               
LOOKINSN LTR   RB,RB               SET CC=NEQ                                   
         B     LOOKINSX                                                         
*                                                                               
LOOKINSY CR    RB,RB                                                            
*                                                                               
LOOKINSX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**************************************************************                  
* SUB-ROUTINE TO EDIT THE SCREEN-CALLED FROM MAIN LINE LOGIC *                  
**************************************************************                  
         SPACE 1                                                                
EDT      NTR1  ,                                                                
         BAS   RE,EDTSEL           EDIT THE SELECT FIELDS FIRST                 
         LA    R2,PROSEL1H                                                      
         ST    R2,ATHISLIN                                                      
         LA    R3,LSELTAB          R3=A(SELECT TABLE)                           
         USING SELTABD,R3                                                       
         LA    R4,MAXRECS          R4=COUNTER                                   
         MVC   PREVREC,STARTREC    INITIALIZE PREVIOUS REC NUM                  
*                                                                               
EDT2     L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         OC    SELKEY,SELKEY       TEST FOR ADD OR CHANGE                       
         BZ    EDT6                ADD-NOTHING THERE BEFORE                     
*                                                                               
         L     R2,AROW             LOOK TO SEE IF ANY FIELDS CHANGED            
         L     R1,ANEXTSEL                                                      
         BCTR  R1,0                R1=BXLE LIMIT                                
         SR    R0,R0               R0=INCREMENT REGISTER                        
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BZ    EDT4                YES                                          
         IC    R0,0(R2)            GET FIELD LENGTH                             
         BXLE  R2,R0,*-12                                                       
         B     EDT15               NOTHING CHANGED                              
*                                                                               
EDT4     GOTO1 =A(EDFLD),PARAS,(RC),RR=RELO   EDIT THE FIELDS                   
         GOTO1 FINDENT,SELKEY                                                   
         GOTO1 CHANGE,PARAS,(R5)                                                
         B     EDT10               WRAP-UP CODE                                 
*                                                                               
EDT6     L     R2,AROW                                                          
         L     R1,ANEXTSEL                                                      
         BCTR  R1,0                                                             
         SR    R0,R0                                                            
         CLI   5(R2),0             TEST IF ANY INPUT                            
         BNE   EDT8                YES-LOOK FOR AN ADD                          
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,*-12                                                       
         B     EDT15               NOTHING INPUT                                
*                                                                               
EDT8     GOTO1 =A(EDFLD),PARAS,(RC),RR=RELO                                     
         GOTO1 FINDENT,PREVREC     GET LINKED LIST FOR ADD POINT                
         GOTO1 ADDR,PARAS,(R5)                                                  
         MVC   SELKEY,HIREC        SAVE REC NUM OF ADDED FIELD                  
*                                                                               
EDT10    L     R2,ASEL             TURN ON THE PREV VALIDATED BITS              
         L     R1,ANEXTSEL                                                      
         BCTR  R1,0                                                             
         SR    R0,R0                                                            
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         XMIT                                         
         IC    R0,0(R2)                                                         
         BXLE  R2,R0,*-12                                                       
*                                                                               
         BAS   RE,SAVTSAR          SAVE THE TSAR BLOCK                          
*                                                                               
EDT15    MVC   ATHISLIN,ANEXTSEL   RESET LINE POINTER                           
         OC    SELKEY,SELKEY       TEST FOR EMPTY FIELD                         
         BZ    *+10                                                             
         MVC   PREVREC,SELKEY      UPDATE PREVIOUS RECORD                       
         LA    R3,SELTABL(R3)                                                   
         BCT   R4,EDT2                                                          
*                                                                               
* NOW HANDLE ANY PENDING DELETES                                                
*                                                                               
         LA    R3,LSELTAB                                                       
         LA    R4,MAXRECS                                                       
EDT16    CLI   SELACT,C'D'                                                      
         BNE   EDT18                                                            
         GOTO1 FINDENT,SELKEY                                                   
         GOTO1 DELETE,PARAS,(R5)                                                
         MVI   SELACT,C' '                                                      
*                                                                               
EDT18    LA    R3,SELTABL(R3)                                                   
         BCT   R4,EDT16                                                         
*                                                                               
* LAST, DEAL WITH ANY MOVE OR COPY                                              
*                                                                               
EDT20    OC    MOVEDETS,MOVEDETS   TEST FOR MOVE DETAILS                        
         BZ    EDT25               NO                                           
         OC    FROM,FROM           TEST FOR FROM RECORD                         
         BZ    EDT25               NO-CANNOT COMPLETE MOVE/COPY                 
         OC    LOCATION,LOCATION   TEST FOR TO RECORD                           
         BZ    EDT25               NO-CANNOT COMPLETE MOVE/COPY                 
         CLI   MOVECOPY,C'C'       TEST FOR COPY                                
         BE    *+12                YES                                          
         BAS   RE,MOVE             PERFORM MOVE                                 
         B     *+8                                                              
*                                                                               
         BAS   RE,COPY                                                          
*                                                                               
         XC    MOVEDETS,MOVEDETS   CLEAR MOVE DETAILS                           
*                                                                               
EDT25    BAS   RE,SAVTSAR          SAVE THE TSAR BLOCK                          
*                                                                               
EDTX     B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
* SUB-ROUTINE TO EDIT THE SELECT FIELDS                     *                   
*************************************************************                   
         SPACE 1                                                                
EDTSEL   NTR1  ,                                                                
         LA    R2,PROSEL1H         R2=A(SELECT FIELD HEADER)                    
         ST    R2,ATHISLIN                                                      
         LA    R3,LSELTAB          R3=A(SELECT TABLE)                           
         USING SELTABD,R3                                                       
         LA    R4,MAXRECS          R4=LOOP COUNTER                              
*                                                                               
EDTSEL2  L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         L     R2,ASEL                                                          
         MVI   SELACT,0            ZERO SELECT ACTION                           
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BO    EDTSEL15            NO-NOTHING TO EDIT                           
*                                                                               
         CLI   5(R2),0             TEST FOR NO INPUT                            
         BE    EDTSEL15                                                         
         CLI   8(R2),C'*'          TEST FOR IGNORE CHARACTER                    
         BE    EDTSEL15                                                         
*                                                                               
EDTSEL4  MVI   ERROR,INVALID                                                    
         CLI   5(R2),1             TEST FOR ONE CHARACTER INPUT                 
         BNE   ERREND                                                           
         OC    SELKEY,SELKEY       TEST IF EMPTY LINE                           
         BZ    ERREND                                                           
         CLI   8(R2),C'I'          TEST FOR 'I'=INSERT                          
         BE    *+12                YES                                          
         CLI   8(R2),C'D'          TEST FOR 'D'=DELETE                          
         BNE   EDTSEL6             NO                                           
*                                                                               
         MVI   ERROR,PENDERR                                                    
         CLC   SELKEY,FROM         TEST IF PENDING MOVE/COPY ITEM               
         BE    ERREND                                                           
         CLC   SELKEY,LOCATION     TEST IF PENDING MOVE/COPY TO ITEM            
         BE    ERREND                                                           
         MVC   SELACT,8(R2)                                                     
         CLI   SELACT,C'D'                                                      
         BE    EDTSEL15                                                         
*                                                                               
         ZIC   RE,NINS                                                          
         LR    R1,RE               FOR INSERTS, KEEP A TABLE                    
         SLL   RE,1                OF RECORD NUMBERS WHERE INSERT               
         LA    RE,INSTAB(RE)       WAS INPUT                                    
         MVC   0(L'INSTAB,RE),SELKEY                                            
         LA    R1,1(R1)                                                         
         STC   R1,NINS                                                          
         B     EDTSEL15                                                         
*                                                                               
EDTSEL6  CLI   8(R2),C'M'          TEST 'M'=MOVE                                
         BE    *+12                                                             
         CLI   8(R2),C'C'          TEST 'C'=COPY                                
         BNE   EDTSEL8             NO                                           
*                                                                               
         MVI   ERROR,DUPMCERR                                                   
         OC    FROM,FROM           TEST IF ALREADY HAVE 'FROM'                  
         BNZ   ERREND                                                           
         MVI   ERROR,INVMCERR      TEST IF SPECIFYING 'TO' LOCATION             
         CLC   SELKEY,LOCATION                                                  
         BE    ERREND                                                           
         MVC   FROM,SELKEY         EXTRACT RECORD NUMBER                        
         MVC   MOVECOPY,8(R2)      EXTRACT ACTION                               
         MVC   SELACT,8(R2)                                                     
         B     EDTSEL15                                                         
*                                                                               
EDTSEL8  CLI   8(R2),C'A'          TEST 'A'=AFTER                               
         BE    *+12                                                             
         CLI   8(R2),C'B'          TEST 'B'=BEFORE                              
         BNE   ERREND                                                           
*                                                                               
         MVI   ERROR,DUPTOERR      DUPLICATE 'TO' SPECIFICATION                 
         OC    LOCATION,LOCATION                                                
         BNZ   ERREND                                                           
         MVI   ERROR,INVTOERR      TEST IF REFERRING TO 'FROM'                  
         CLC   SELKEY,FROM                                                      
         BE    ERREND                                                           
         MVC   LOCATION,SELKEY                                                  
         MVC   POSITION,8(R2)                                                   
*                                                                               
EDTSEL15 MVC   ATHISLIN,ANEXTSEL                                                
         LA    R3,SELTABL(R3)                                                   
         BCT   R4,EDTSEL2                                                       
*                                                                               
EDTSELX  B     XIT                                                              
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO INITIALIZE THE LINKED LIST AND RUN-TIME STACK  *               
* RUN-TIME STACK WILL BE MAINTAINED AS A TSAR DATA SET.         *               
* CALLED FROM MAIN LINE VALREC LOGIC.                           *               
*****************************************************************               
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         XC    HIREC,HIREC         HIGHEST RECORD=0                             
         XC    MOVEDETS,MOVEDETS                                                
         LA    RE,LINKLST          CLEAR LINKED LIST AREA                       
         LA    RF,L'LINKLST                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
INIT2    LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSNBUF,1            ONE CORE BUFFER                              
         LA    RE,BUFF             BUFF=A(CORE BUFFER)                          
         ST    RE,TSABUF                                                        
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGL,1                                                         
         MVI   TSPAGN,2                                                         
         OI    TSRECI,TSRVAR                                                    
         MVI   TSKEYL,2                                                         
         MVC   TSRECL,=AL2(255+2+2)                                             
         MVI   TSACTN,TSAINI       ACTION=INIT                                  
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ANCHOR                                                        
*                                                                               
INIT4    BAS   RE,GETFLD           GET NEXT FIELD DATA ELEM                     
         BNE   INIT6               ALL DONE                                     
         BAS   RE,ADDFLD           ADD TO LINKED LIST/DATA SET                  
         B     INIT4                                                            
*                                                                               
INIT6    BAS   RE,SAVTSAR          MAKE SURE DATASET IS SAVED                   
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO ADD THE ANCHOR ENTRY                                           
*                                                                               
ANCHOR   MVI   HIENT,1             FIRST LINKED LIST ENTRY                      
         LA    R5,LINKLST                                                       
         USING LINKD,R5                                                         
         MVI   LINKFRST,1                                                       
         MVI   LINKLAST,1                                                       
         XC    LINKRNUM,LINKRNUM                                                
         BR    RE                                                               
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO GET THE NEXT FIELD DATA ELEMENT--CALLED FROM   *               
* INIT.  AT ENTRY, LASTNUM CONTAINS THE LAST PANEL DATA RECORD'S*               
* NUMBER AND ALASTF POINTS TO THE LAST ELEMENT RETURNED TO      *               
* CALLER.  ON EXIT, CC=EQ AND R6=A(NEXT ELEMENT) AND CC=NEQ IF  *               
* EOF IS ENCOUNTERED.                                           *               
*****************************************************************               
         SPACE 1                                                                
GETFLD   NTR1  ,                                                                
         ICM   R6,15,ALASTF                                                     
         BNZ   GETFLD4             RESUME SEARCH AFTER LAST ELEMENT             
*                                                                               
GETFLD2  LA    R4,KEY                                                           
         USING ACPNKEY,R4                                                       
         XC    ACPNKEY,ACPNKEY                                                  
         MVI   ACPNRTYP,ACPNEQU                                                 
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   ACPNCODE,QPANEL                                                  
         ZIC   R1,LASTNUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ACPNNUM                                                       
         STC   R1,LASTNUM                                                       
         GOTO1 HIGH                                                             
         CLC   ACPNKEY,KEYSAVE     TEST IF DATA RECORD FOUND                    
         BNE   GETFLDN             NO-EOF                                       
*                                                                               
GETFLD3  MVI   ELCODE,ACFDELQ                                                   
         BAS   RE,GETELIO          GET FIRST FIELD DATA ELEM                    
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACFDD,R6                                                         
         CLI   ACFDSEQ,X'FF'       TEST FOR TRAILER ELEMENT                     
         BE    GETFLDN             YES-EOF                                      
         B     GETFLDY             NO-RETURN THE ELEMENT                        
*                                                                               
GETFLD4  ZIC   R0,ACFDLEN                                                       
         AR    R6,R0               BUMP TO NEXT ELEMENT                         
         CLI   ACFDEL,0            TEST FOR EOR                                 
         BE    GETFLD2             YES-READ NEXT RECORD                         
         CLI   ACFDEL,ACFDELQ      TEST FOR FIELD DATA ELEMENT                  
         BNE   GETFLD4             NO-BUMP TO NEXT ELEMENT                      
         CLI   ACFDSEQ,X'FF'       TEST FOR TRAILER ELEMENT                     
         BE    GETFLDN             YES-EOF                                      
*                                                                               
GETFLDY  ST    R6,ALASTF           SAVE ELEMENT ADDRESS                         
         CR    RB,RB               SET CC=EQ                                    
         XIT1  REGS=(R6)                                                        
*                                                                               
GETFLDN  B     NOXIT                                                            
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO ADD A LINKED LIST ENTRY AND ITS CORRESPONDING   *              
* DATA RECORD.  THE DATA RECORD IS ADDED TO THE RUN-TIME STACK   *              
* MAINTAINED AS A TSAR DATA SET.  CALLED FROM INIT.              *              
*                                                                *              
* AT ENTRY, R6=A(FIELD ELEMENT), HIENT=HIGHEST LINKED LIST ENTRY,*              
* AND HIREC=HIGHEST RECORD NUMBER                                *              
******************************************************************              
         SPACE 1                                                                
ADDFLD   NTR1  ,                                                                
         ZIC   R3,HIENT                                                         
         LR    R5,R3                                                            
         MH    R5,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(R5)      R5=A(NEW ENTRY)                              
*                                                                               
         LH    R2,HIREC                                                         
         LA    R2,1(R2)            INCREMENT HIGHEST RECORD SO FAR              
         STH   R2,LINKRNUM         SET NEW ENTRY REC NUM                        
         STH   R2,HIREC                                                         
         LR    RE,R5               GET PREVIOUS ENTRY                           
         SH    RE,=Y(L'LINKREC)    RE=A(PREVIOUS ENTRY)                         
*                                                                               
         MVI   LINKNEXT,1          NEW ENTRY POINTS AHEAD TO ANCHOR             
         LA    R3,1(R3)            R6=NEW ENTRY'S NUMBER                        
         STC   R3,LINKNEXT-LINKD(RE) POINT PREV ENTRY TO NEW ONE                
*                                                                               
         LA    R4,LINKLST          R4=A(ANCHOR)                                 
         MVC   LINKPREV,HIENT      PREVIOUS ENTRY=HIGHEST                       
         STC   R3,LINKPREV-LINKD(R4) ANCHOR'S PREV ENTRY=NEW ONE                
*                                                                               
         STC   R3,HIENT            UPDATE HIGHEST LINKED LIST ENTRY             
*                                                                               
ADDFLD2  L     R4,AIO2             BUILD TSAR REC AT IO2                        
         MVC   2(2,R4),HIREC       SET RECORD NUMBER IN KEY                     
         LA    RE,4(R4)            RE=A(FIELD ELEM POSITION)                    
         ZIC   RF,ACFDLEN                                                       
         LR    R0,R6                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0               MOVE ELEMENT INTO POSITION                   
         ZIC   R1,ACFDLEN                                                       
         LA    R1,4(R1)            ADD ON RLEN+KEY LEN                          
         STH   R1,0(R4)            SET RECORD LENGTH                            
*                                                                               
ADDFLD4  LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAADD       ADD TO TSAR FILE                             
         ST    R4,TSAREC           SET RECORD ADDRESS                           
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADDFLDX  B     XIT                                                              
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO REORGANIZE THE TSAR DATA SET.  ROUTINE WILL  *                 
* COMPRESS THE DATA SET BY REMOVING ALL GAPS AND WILL RE-     *                 
* BUILD THE LINKED LIST.                                      *                 
***************************************************************                 
         SPACE 1                                                                
REORG    NTR1  ,                                                                
         L     R3,=V(DUMMY)        USE AREA BEHIND PROGRAM TO REORG             
         A     R3,RELO                                                          
         ST    R3,ABUFF            SAVE BUFFER ADDRESS                          
*                                                                               
         LA    R5,LINKLST                                                       
         USING LINKD,R5                                                         
*                                                                               
REORG2   ZIC   R4,LINKNEXT         GET NEXT ENTRY NUMBER                        
         BCTR  R4,0                                                             
         MH    R4,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(R4)      R5=A(NEXT LINKED LIST ENTRY)                 
         OC    LINKRNUM,LINKRNUM   TEST FOR ANCHOR ENTRY                        
         BZ    REORG4              YES-HAVE REACHED EOL                         
*                                                                               
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH                                                    
         ST    R3,TSAREC           READ RECORD INTO BUFFER                      
         MVC   2(2,R3),LINKRNUM    SET RECORD KEY=NUMBER                        
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,0(R3)          GET RECORD LENGTH                            
         AR    R3,R0               POINT TO NEXT BUFFER POSITION                
         B     REORG2                                                           
*                                                                               
* NOW HAVE ENTIRE DATA SET IN LOGICAL ORDER IN BUFFER WITH ALL                  
* DELETES AND CHANGES REMOVED                                                   
*                                                                               
REORG4   XC    0(2,R3),0(R3)       SET AN END-OF-BUFFER MARKER                  
         L     R3,ABUFF            RESTORE BUFFER POINTER                       
         LA    RE,LINKLST                                                       
         LA    RF,L'LINKLST                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
REORG6   LA    RE,TSARBLK          CLEAR TSAR BLOCK                             
         LA    RF,L'TSARBLK                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSNBUF,1            N'CORE BUFFERS                               
         LA    RE,BUFF                                                          
         ST    RE,TSABUF           SET A(CORE BUFFER)                           
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGL,1            FIRST TWA SAVE PAGE=1                        
         MVI   TSPAGN,2            TWO SAVE PAGES                               
         OI    TSRECI,TSRVAR                                                    
         MVI   TSKEYL,2                                                         
         MVC   TSRECL,=AL2(255+2+2)                                             
         MVI   TSACTN,TSAINI       ACTION=INIT                                  
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ANCHOR                                                        
*                                                                               
* RE-ADD THE RECORDS IN SEQUENCE TO NEW LINKED LIST/DATA SET                    
*                                                                               
REORG8   OC    0(2,R3),0(R3)       TEST FOR END-OF-BUFFER                       
         BZ    REORG10                                                          
         BAS   RE,SETFLD           PUT THE RECORD INTO NEW DATASET              
         SR    R0,R0                                                            
         ICM   R0,3,0(R3)          BUMP TO NEXT RECORD                          
         AR    R3,R0                                                            
         B     REORG8                                                           
*                                                                               
REORG10  BAS   RE,SAVTSAR          SAVE REORGANIZED DATA SET                    
*                                                                               
REORGX   B     XIT                                                              
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO SET A TSAR DATA RECORD INTO NEW DATASET AND TO  *              
* GENERATE A NEW LINKED LIST ENTRY FOR IT.  CALLED FROM REORG    *              
*                                                                *              
* AT ENTRY, R3=TSAR RECORD, HIENT=HIGHEST LINKED LIST ENTRY      *              
******************************************************************              
         SPACE 1                                                                
SETFLD   NTR1  ,                                                                
         ZIC   R6,HIENT                                                         
         LR    R5,R6                                                            
         MH    R5,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(R5)      R5=A(NEW ENTRY)                              
*                                                                               
         MVC   LINKRNUM,2(R3)      EXTRACT RECORD NUMBER                        
         LR    RE,R5               GET PREVIOUS ENTRY                           
         SH    RE,=Y(L'LINKREC)    RE=A(PREVIOUS ENTRY)                         
*                                                                               
         MVI   LINKNEXT,1          NEW ENTRY POINTS AHEAD TO ANCHOR             
         LA    R6,1(R6)            R6=NEW ENTRY'S NUMBER                        
         STC   R6,LINKNEXT-LINKD(RE) POINT PREV ENTRY TO NEW ONE                
*                                                                               
         LA    R4,LINKLST          R4=A(ANCHOR)                                 
         MVC   LINKPREV,HIENT      PREVIOUS ENTRY=HIGHEST                       
         STC   R6,LINKPREV-LINKD(R4) ANCHOR'S PREV ENTRY=NEW ONE                
*                                                                               
         STC   R6,HIENT            UPDATE HIGHEST LINKED LIST ENTRY             
*                                                                               
SETFLD2  LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAADD       ADD TO TSAR FILE                             
         ST    R3,TSAREC           SET RECORD ADDRESS                           
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SETFLDX  B     XIT                                                              
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO PRINT OUT THE FIELD DEFINITION ELEMENTS AND TO *               
* CHECK THE SCREEN DEFINITION.  THE SCREEN WILL BE PRINTED IF   *               
* THERE ARE NO LOGICAL ERRORS IN THE DEFINITION                 *               
*****************************************************************               
         SPACE 1                                                                
CHECK    NTR1  ,                                                                
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         MVI   RCSUBPRG,0                                                       
         L     RE,=V(DUMMY)                                                     
         A     RE,RELO                                                          
         ST    RE,ABUFF                                                         
*                                                                               
* USE AREA BEHIND PROGRAM TO BUILD UP THE SCREEN                                
*                                                                               
CHECK2   L     RE,ABUFF            COPY SCREEN HEADER                           
         LA    RF,CONTAGH-T60BFFD                                               
         LR    R1,RF                                                            
         L     R0,ATWA                                                          
         MVCL  RE,R0                                                            
*                                                                               
         MVC   DMCB+4(3),SYSPHASE  LOAD IN TEXT SCREEN AFTER                    
         MVI   DMCB+7,X'C8'        HEADER                                       
         L     RE,ABUFF            FORM LOAD ADDRESS BY ADDING                  
         LA    RE,CONTAGH-T60BFFD(RE)  DISP TO LOAD POINT                       
         ST    RE,DMCB                                                          
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* INITIALIZE FIELD POINTER AND LAST ROW, COLUMN, END OF LAST FIELD              
*                                                                               
         L     R2,ABUFF            R2=A(START OF SCREEN)                        
         LA    R2,TXTTAGH-T60BFFD(R2)                                           
         MVC   HALF,2(R2)          EXTRACT DISP TO SCR START                    
         LH    RF,HALF                                                          
         BCTR  RF,0                BACK UP ONE POSITION                         
         STH   RF,LASTEND                                                       
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         LA    RF,1(RF)            ROW                                          
         LA    RE,1(RE)            COLUMN                                       
         STC   RF,LASTROW                                                       
         STC   RE,LASTCOL                                                       
*                                                                               
* INITIALIZE TWABLD PARMS                                                       
*                                                                               
CHECK4   XC    TWAPARM,TWAPARM                                                  
         LA    R3,TWAPARM          R3=A(TWABLD PARMS)                           
         USING TWAPARMD,R3                                                      
         L     RE,ABUFF                                                         
         ST    RE,TWAPATWA                                                      
         MVC   TWAPAMAX,=F'2304'   DO NOT WANT TO RUN BEYOND +2304              
         ST    R2,TWAPAOUT                                                      
*                                                                               
         LA    R5,LINKLST          R5=A(LINKED LIST ENTRY)                      
         USING LINKD,R5                                                         
*                                                                               
* PROCESS LINKED LIST SEQUENTIALLY                                              
*                                                                               
CHECK6   ZIC   RE,LINKNEXT                                                      
         BCTR  RE,0                                                             
         MH    RE,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(RE)      NEXT LINKED LIST ENTRY                       
         OC    LINKRNUM,LINKRNUM   TEST FOR ANCHOR ENTRY                        
         BZ    CHECK30             YES-WE HAVE REACHED THE END                  
*                                                                               
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH                                                    
         L     R4,AIO1             R4=A(TSAR RECORD)                            
         XC    0(4,R4),0(R4)                                                    
         MVC   2(2,R4),LINKRNUM    SET TSAR RECORD KEY                          
         ST    R4,TSAREC                                                        
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* PRINT THE FIELD DEFINITION--THEN CHECK FOR SCREEN ERRORS                      
*                                                                               
CHECK8   GOTO1 =A(DISFLD),PARAS,(RC),(1,(R4)),RR=RELO                           
         ZIC   RE,NENT             INCREMENT ENTRY COUNTER                      
         LA    RE,1(RE)                                                         
         STC   RE,NENT                                                          
         LR    R0,RE                                                            
         EDIT  (R0),(3,P+1),ALIGN=LEFT                                          
         MVI   ALLOWLIN,2          ALLOW FOR SKIPPED LINE                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 (RF),(R1),(R8)                                                   
*                                                                               
         LA    R6,4(R4)            R6=A(FIELD DEFN ELEMENT)                     
         USING ACFDD,R6                                                         
         CLI   ACFDSROW,0          TEST IF ANY SCREEN INPUT                     
         BE    CHECK25             NO-PRINT ONLY                                
*                                                                               
         CLI   ERRSW,C'Y'          TEST SCREEN ERROR FOUND                      
         BE    CHECK25             YES-SKIP ANY MORE CHECKS                     
*                                                                               
         XC    TWAELEM,TWAELEM                                                  
         LA    R2,TWAELEM          R2=A(TWABLD ELEMENT)                         
         USING TWAELEMD,R2                                                      
         MVI   TWAELCD,X'01'                                                    
         MVI   TWAELLN,TWAELLNQ    INITIALIZE ELEMENT LENGTH                    
         ZIC   RE,ACFDLEN          RE=ELEMENT LENGTH                            
         ZIC   RF,ACFDPDLN         RF=PRINT DATA LENGTH                         
         SR    RE,RF                                                            
         LA    RF,ACFDSCR-ACFDD                                                 
         CR    RE,RF               TEST IF ANY SCREEN DATA                      
         BE    CHECK10             NO                                           
*                                                                               
         SR    RE,RF               RE=L'SCREEN DATA                             
         LR    R1,RE               SAVE L'SCREEN DATA IN R1                     
         LA    RE,TWAELLNQ(RE)     COMPUTE TWABLD ELEM LEN                      
         STC   RE,TWAELLN                                                       
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAEDTA(0),ACFDSCR                                               
*                                                                               
CHECK10  MVC   TWAEATB,ACFDATTR    ATTRIBUTE BYTE                               
         MVC   TWAEFLN,ACFDSLEN    SCREEN LENGTH                                
*                                                                               
         MVC   THISCOL,ACFDSCOL    EXTRACT THIS FIELD'S COLUMN                  
         TM    ACFDSROW,X'80'      TEST FOR AUTO ROW FEATURE                    
         BO    CHECK11             YES                                          
         TM    ACFDSROW,X'40'      TEST FOR PLUS ROW FEATURE                    
         BO    CHECK12                                                          
*                                                                               
         MVC   THISROW,ACFDSROW    EXTRACT ROW                                  
         B     CHECK14                                                          
*                                                                               
CHECK11  ZIC   R1,LASTROW          AUTO ROW                                     
         CLC   THISCOL,LASTCOL     TEST THIS COL > LAST ONE                     
         BH    *+8                 YES-MUST BE ON SAME ROW AS LAST ONE          
         LA    R1,1(R1)                                                         
         STC   R1,THISROW                                                       
         B     CHECK14                                                          
*                                                                               
CHECK12  ZIC   R1,LASTROW                                                       
         MVC   BYTE,ACFDSROW                                                    
         NI    BYTE,X'FF'-X'40'    TURN OFF PLUS BIT                            
         ZIC   R0,BYTE                                                          
         AR    R1,R0               ADD IN +N ROWS                               
         STC   R1,THISROW                                                       
*                                                                               
CHECK14  MVC   TWAECOL,THISCOL     FINISH TWABLD ELEMENT                        
         ZIC   R1,THISROW                                                       
         ZIC   R0,LASTROW                                                       
         SR    R1,R0               COMPUTE RELATIVE ROW NUMBER                  
         STC   R1,TWAERLN                                                       
*                                                                               
         ZIC   RE,THISROW                                                       
         BCTR  RE,0                                                             
         MH    RE,=H'80'          GET SCREEN DISP TO FIELD                      
         ZIC   RF,THISCOL                                                       
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         CH    RE,LASTEND          COMPARE SCREEN DISP TO PREVIOUS ONE          
         BL    CHECK14R            BEFORE THE LAST ONE-ERROR                    
         BH    CHECK15             OK                                           
*                                                                               
         TM    TWAEATB,X'20'       UNPROT ATTB CAN OVERLAP                      
         BO    CHECK14R            PROT ATTB FROM LAST UNPROT FIELD             
         TM    LASTATTB,X'20'                                                   
         BZ    CHECK15                                                          
*                                                                               
CHECK14R MVI   ERRSW,C'Y'                                                       
         MVC   P+10(L'SCRMSG1),SCRMSG1                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     CHECK25                                                          
*                                                                               
CHECK15  ZIC   RF,TWAEFLN          GET FIELD LENGTH                             
         TM    TWAEATB,X'20'       TEST FOR PROTECTED FIELD                     
         BO    *+8                                                              
         LA    RE,2(RE)            NO-ADD IN BEFORE AND AFTER BYTES             
         AR    RE,RF                                                            
         BCTR  RE,0                NEW END OF LAST FIELD                        
         STH   RE,LASTEND                                                       
         MVC   LASTATTB,TWAEATB    SAVE ATTRIBUTE BYTE                          
         MVC   LASTROW(2),THISROW                                               
         CH    RE,=Y(23*80)        TEST END BEFORE ROW 24                       
         BL    CHECK16                                                          
*                                                                               
         MVI   ERRSW,C'Y'                                                       
         MVC   P+10(L'SCRMSG2),SCRMSG2                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     CHECK25                                                          
*                                                                               
CHECK16  OC    ACFDFLD,ACFDFLD     TEST FOR FIELD KEYWORD                       
         BZ    CHECK18             NONE                                         
         BAS   RE,DUPKEY                                                        
         BNE   CHECK18             NO DUPLICATION                               
*                                                                               
         MVI   ERRSW,C'Y'                                                       
         MVC   P+10(L'SCRMSG4),SCRMSG4                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     CHECK25                                                          
*                                                                               
CHECK18  ST    R2,TWAPAFST         SET TWABLD ELEM ADDR                         
         GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0          TEST FOR ANY ERROR                           
         BE    CHECK20             NO                                           
*                                                                               
         MVI   ERRSW,C'Y'                                                       
         CLI   TWAPERRS,TWAPEEMS   TEST TWA MAX BLOWN                           
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT FOR OTHER ERRORS                  
*                                                                               
         MVC   P+10(L'SCRMSG3),SCRMSG3                                          
         LH    R0,TWAPTLEN         GET TOTAL LENGTH SO FAR                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P2+10(4),DUB+5(3)                                                
         MVC   P2+16(17),=C'BYTES USED SO FAR'                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     CHECK25                                                          
*                                                                               
CHECK20  MVC   TWAPAOUT,TWAPANXT                                                
         MVI   SCRSW,C'Y'          NOTE ONE SCREEN FIELD SET                    
*                                                                               
CHECK25  B     CHECK6              NEXT LINKED LIST FIELD                       
*                                                                               
* PRINT OUT THE SCREEN IF NO ERRORS                                             
*                                                                               
CHECK30  CLI   ERRSW,C'Y'          TEST FOR ANY ERRORS                          
         BE    CHECKX              YES-ALL DONE                                 
         CLI   SCRSW,C'Y'          TEST ANY SCREEN FIELDS BUILT                 
         BNE   CHECKX              NO                                           
*                                                                               
* FIRST BUILD A 24X80 MATRIX OF THE SCREEN IN IO3                               
*                                                                               
         L     RE,AIO3                                                          
         LA    R0,24               CLEAR THE MAXTRIX TO SPACES                  
         MVC   0(80,RE),SPACES                                                  
         LA    RE,80(RE)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         L     R2,ABUFF                                                         
         LA    R2,64(R2)           R2=A(SCREEN FIELD HEADER)                    
*                                                                               
CHECK32  CLI   0(R2),0             TEST FOR EOS                                 
         BE    CHECK40             YES                                          
*                                                                               
         MVC   HALF,2(R2)          DISP INTO SCREEN                             
         L     R3,AIO3                                                          
         AH    R3,HALF             R3=A(SCREEN FIELD)                           
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'8'                                                         
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    CHECK34             YES                                          
*                                                                               
         MVI   0(R3),X'6D'         UNDERSCORE CHARACTER                         
         SH    R1,=H'1'                                                         
         BZ    CHECK35             ONE BYTE FIELD                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     CHECK35                                                          
         MVC   1(0,R3),0(R3)       PROPAGATE UNDERSCORE                         
*                                                                               
CHECK34  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     CHECK35                                                          
         MVC   0(0,R3),8(R2)       EXTRACT FIELD DATA                           
*                                                                               
CHECK35  BAS   RE,BUMP                                                          
         B     CHECK32                                                          
*                                                                               
* NOW PRINT THE REPORT USING THE 24X80 MATRIX                                   
*                                                                               
CHECK40  L     R2,AIO3             R2=A(ROW)                                    
         LA    R3,24               R3=LOOP COUNTER                              
         LA    R4,1                R4=ROW NUMBER                                
*                                                                               
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK FOR SCREEN                  
         MVI   P+4,C'*'                                                         
         MVC   P+5(79),P+4                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CHECK42  MVI   P+3,C'*'                                                         
         MVI   P+84,C'*'                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P(2),DUB+6(2)       ROW NUMBER                                   
*                                                                               
         MVI   P+3,C'*'                                                         
         MVI   P+84,C'*'                                                        
         MVC   P+4(80),0(R2)       EXTRACT SCREEN ROW                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R2,80(R2)                                                        
         LA    R4,1(R4)            NEXT ROW                                     
         BCT   R3,CHECK42                                                       
*                                                                               
CHECK45  MVI   P+3,C'*'                                                         
         MVI   P+84,C'*'                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   P+4,C'*'                                                         
         MVC   P+5(79),P+4                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CHECKX   B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO PRINT HEADHOOK                                                 
*                                                                               
HOOK     NTR1  ,                                                                
         MVC   H4+10(L'QPANEL),QPANEL                                           
         B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK FOR DUPLICATE FIELD KEYWORDS                             
*    TABLE OF KEYWORDS KEPT IN IO2                                              
* ON EXIT, CC=EQ IF DUPLICATION, CC=NEQ IF NO DUPLICATION                       
*                                                                               
DUPKEY   ST    RE,SAVERE                                                        
         L     RE,AIO2             USE IO2 TO KEEP KEYWORD TABLE                
         SR    R1,R1                                                            
         ICM   R1,1,NKEYS                                                       
         BZ    DUPKEY3             FIRST ENTRY                                  
*                                                                               
DUPKEY2  CLC   ACFDFLD,0(RE)       MATCH ON FIELD KEYWORD                       
         BE    DUPKEYX             FOUND A DUPLICATION                          
         LA    RE,L'ACFDFLD(RE)                                                 
         BCT   R1,DUPKEY2                                                       
*                                                                               
DUPKEY3  MVC   0(L'ACFDFLD,RE),ACFDFLD                                          
         ZIC   R1,NKEYS                                                         
         LA    R1,1(R1)                                                         
         STC   R1,NKEYS                                                         
         LTR   RB,RB               SET CC=NEQ                                   
*                                                                               
DUPKEYX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO SET ADCONS FOR A LIST FIELD LINE                               
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,LISTFLDS                                                      
         LA    R1,ASEL                                                          
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,*-12                                                          
         ST    R2,ANEXTSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
YESXIT   CR    RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
NOXIT    LTR   RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
SAVTSAR  ST    RE,SAVERE                                                        
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSASAV                                                    
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
ERREND   BAS   RE,SAVTSAR                                                       
         GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
RELO     DC    A(0)                                                             
MAXRLEN  DC    Y(1000)                                                          
EDTMSG   DC    C'CHANGES COMPLETED - PRESS PF4 WHEN YOU WANT TO SAVE'           
PENDMSG  DC    C'MOVE/COPY PENDING'                                             
SAVEMSG  DC    C'**PANEL SAVED**'                                               
ERASEMSG DC    C'**MOVE/COPY DETAILS ERASED**'                                  
NONEMSG  DC    C'**THERE IS NO FIELD DATA FOR THIS PANEL**'                     
ENTERMSG DC    C'**ENTER PANEL DATA**'                                          
SCRMSG1  DC    C'**FIELD STARTS BEFORE PREVIOUS ONE ENDED**'                    
SCRMSG2  DC    C'**THIS FIELD IS ON ROW 24--PLEASE CHANGE PANEL**'              
SCRMSG3  DC    C'**SCREEN LIMIT OF 2304 BYTES EXCEEDED--CALL DDS FOR ASX        
               SISTANCE**'                                                      
SCRMSG4  DC    C'**DUPLICATE FIELD KEYWORD**'                                   
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SPECS FOR FIELD MAINTENANCE REPORT                                            
*                                                                               
HEDSPECS DS    0D                                                               
         SPROG 0,1                                                              
         SSPEC H1,2,CREATED                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,45,C'FIELD MAINTENANCE REPORT'                                
         SSPEC H2,45,C'------------------------'                                
         SSPEC H1,85,AGYNAME                                                    
         SSPEC H2,85,AGYADD                                                     
         SSPEC H4,2,C'PANEL'                                                    
         SSPEC H4,85,REPORT                                                     
         SSPEC H4,98,PAGE                                                       
*                                                                               
         SPROG 0                                                                
         SSPEC H4,45,C'FIELD DEFINITION LISTING'                                
         SSPEC H6,8,C'---SCREEN DEFINITION---'                                  
         SSPEC H7,2,C'ENTRY'                                                    
         SSPEC H7,8,C'ROW'                                                      
         SSPEC H7,13,C'COL'                                                     
         SSPEC H7,18,C'LEN'                                                     
         SSPEC H7,22,C'P'                                                       
         SSPEC H7,24,C'H'                                                       
         SSPEC H7,26,C'U/L'                                                     
         SSPEC H7,30,C'KEYWORD'                                                 
         SSPEC H6,51,C'---PRINT DEFINITION---'                                  
         SSPEC H7,51,C'LINE'                                                    
         SSPEC H7,57,C'COLUMN'                                                  
*                                                                               
         SPROG 1                                                                
         SSPEC H4,48,C'SCREEN APPEARANCE'                                       
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO SAVE THE TSAR DATA SET AS A PANEL DATASET    *                 
***************************************************************                 
         SPACE 1                                                                
SAVE     NMOD1 0,**SAVE**,RR=R2                                                 
         L     RC,0(R1)                                                         
         L     R3,=V(DUMMY)        USE AREA BEHIND PROGRAM AS BUFFER            
         AR    R3,R2                                                            
         ST    R3,ABUFF            SAVE BUFFER ADDRESS                          
*                                                                               
         LA    R5,LINKLST                                                       
         USING LINKD,R5                                                         
*                                                                               
SAVE2    ZIC   R4,LINKNEXT         GET NEXT ENTRY NUMBER                        
         BCTR  R4,0                                                             
         MH    R4,=Y(L'LINKREC)                                                 
         LA    R5,LINKLST(R4)      R5=A(NEXT LINKED LIST ENTRY)                 
         OC    LINKRNUM,LINKRNUM   TEST FOR ANCHOR ENTRY                        
         BZ    SAVE4               YES-HAVE REACHED EOL                         
*                                                                               
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH                                                    
         ST    R3,TSAREC           READ RECORD INTO BUFFER                      
         MVC   2(2,R3),LINKRNUM    SET KEY TO READ FOR                          
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,0(R3)          GET RECORD LENGTH                            
         AR    R3,R0               POINT TO NEXT BUFFER POSITION                
         B     SAVE2                                                            
*                                                                               
SAVE4    XC    0(2,R3),0(R3)       SET AN END-OF-BUFFER MARKER                  
         L     R3,ABUFF            RESET BUFFER POINTER                         
         MVI   LASTNUM,0           INITIALIZE DATA NUMBER POINTER               
         BAS   RE,NEWREC           BUILD FIRST RECORD                           
*                                                                               
SAVE6    OC    0(2,R3),0(R3)       TEST FOR END-OF-BUFFER                       
         BZ    SAVE8               YES                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACFDD,R6                                                         
         SR    R1,R1                                                            
         ICM   R1,3,0(R3)          GET TSAR RECORD LENGTH                       
         SH    R1,=H'5'            DEVELOP ELEM LENGTH                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),4(R3)       EXTRACT FIELD ELEMENT                        
*                                                                               
         ZIC   R1,LASTSEQ          GET LAST SEQUENCE NUMBER                     
         LA    R1,1(R1)            INCREMENT SEQUENCE NUMBER                    
         STC   R1,ACFDSEQ                                                       
         STC   R1,LASTSEQ                                                       
         BAS   RE,UPPAN            UPDATE PANEL DATA SET                        
         SR    R0,R0                                                            
         ICM   R0,3,0(R3)                                                       
         AR    R3,R0               NEXT RECORD IN BUFFER                        
         B     SAVE6                                                            
*                                                                               
SAVE8    LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ACFDEL,ACFDELQ      ADD A TRAILER ELEMENT                        
         MVI   ACFDLEN,ACFDSCR-ACFDD                                            
         MVI   ACFDSEQ,X'FF'       TRAILER NOTE                                 
         BAS   RE,UPPAN            UPDATE THE PANEL DATA SET                    
         BAS   RE,PUTPAN           WRITE OUT THE TRAILER RECORD                 
*                                                                               
SAVE10   L     RE,AIO              POINT TO DATA REC WITH HIGHEST NUM           
         LA    R4,KEY                                                           
         MVC   ACPNKEY,0(RE)       EXTRACT LAST PANEL KEY                       
         ZIC   R1,ACPNNUM          GET HIGHEST RECORD NUMBER                    
         LA    R1,1(R1)            AND INCREMENT IT                             
         STC   R1,ACPNNUM                                                       
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVC   AIO,AIO3            USE IO3 TO READ RECORDS                      
         GOTO1 HIGH                                                             
         B     SAVE12                                                           
*                                                                               
SAVE11   LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
SAVE12   CLC   ACPNKEY(ACPNNUM-ACPNKEY),KEYSAVE  TEST SAME PANEL                
         BNE   SAVE15              NO-ALL DONE                                  
*                                                                               
         L     R4,AIO              WRITE BACK ALL RECORDS AFTER                 
         OI    ACSTATUS,X'80'      TRAILER AS DELETED                           
         GOTO1 WRITE                                                            
         B     SAVE11                                                           
*                                                                               
SAVE15   MVC   AIO,AIO1            RESTORE IO AREA POINTER                      
         NI    DMINBTS,X'FF'-X'08' TURN OFF PASS DELETES                        
*                                                                               
SAVE20   LA    R4,KEY                                                           
         L     RE,AIO                                                           
         MVC   ACPNKEY,0(RE)       EXTRACT KEY                                  
         MVI   ACPNNUM,0           CLEAR OUT DATA NUMBER                        
         GOTO1 HIGH                                                             
         CLC   ACPNKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,ACPHELQ                                                   
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING ACPHD,R6                                                         
         MVI   ACPHLEN,ACPHLENQ                                                 
         MVC   ACPHPERS,TWAALIAS                                                
         MVC   ACPHLAST,TODAYP                                                  
         MVI   ACPHWHAT,C'F'       NOTE FIELD MAINTENANCE                       
         GOTO1 ADDELEM                                                          
         GOTO1 WRITE                                                            
*                                                                               
SAVEX    XMOD1 1                                                                
         SPACE 2                                                                
* SUB-ROUTINE TO BUILD A SKELETAL NEW PANEL DATA RECORD                         
*                                                                               
NEWREC   ST    RE,SAVERE                                                        
         L     R4,AIO                                                           
         USING ACPNKEY,R4                                                       
         LR    RE,R4                                                            
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         MVI   ACPNRTYP,ACPNEQU                                                 
         MVI   ACPNSREC,ACPNSEQU                                                
         MVC   ACPNCUL,CUL                                                      
         MVC   ACPNCODE,QPANEL                                                  
         ZIC   R1,LASTNUM                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LASTNUM                                                       
         STC   R1,ACPNNUM                                                       
         LA    R1,ACRECORD-ACKEYD+1 SET RECORD LENGTH                           
         STH   R1,ACLENGTH                                                      
*                                                                               
NEWRECX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO UPDATE THE PANEL DATA SET WITH A NEW ELEMENT                   
* AT ENTRY, R6=A(NEW ELEMENT) R4=A(RECORD)                                      
*                                                                               
UPPAN    NTR1  ,                                                                
         USING ACFDD,R6                                                         
         LH    RE,ACLENGTH                                                      
         ZIC   R1,ACFDLEN          GET NEW ELEMENT LENGTH                       
         AR    RE,R1               COMPUTE NEW RECORD LENGTH                    
         CH    RE,MAXRLEN          TEST FOR RECORD OVERFLOW                     
         BNH   UPPAN2              NO                                           
*                                                                               
         BAS   RE,PUTPAN                                                        
         BAS   RE,NEWREC           BUILD NEW SKELETAL RECORD                    
*                                                                               
UPPAN2   GOTO1 ADDELEM             ADD NEW ELEMENT                              
*                                                                               
UPPANX   XIT1                                                                   
         SPACE 2                                                                
* SUB-ROUTINE TO WRITE THE PANEL DATA RECORD AT AIO BACK TO FILE                
* AT ENTRY, R4=A(PANEL DATA RECORD)                                             
*                                                                               
PUTPAN   ST    RE,SAVERE                                                        
         MVC   KEY(L'ACPNKEY),ACPNKEY                                           
         MVC   AIO,AIO2                                                         
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACPNKEY),KEYSAVE TEST IF DATA REC FOUND                    
         BE    PUTPAN2             YES                                          
*                                                                               
         MVC   AIO,AIO1            RESET IO POINTER                             
         GOTO1 ADD                 ADD RECORD IN IO AREA                        
         B     PUTPANX                                                          
*                                                                               
PUTPAN2  MVC   AIO,AIO1                                                         
         GOTO1 WRITE                                                            
*                                                                               
PUTPANX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO DISPLAY A FIELD DATA ELEMENT ON THE SCREEN   *                 
* AT ENTRY, P1=A(GEND), P2=A(TSAR DATA RECORD)                *                 
*           P2 BYTE 0=0 FOR SCREEN DISPLAY 1=PRINT DISPLAY    *                 
* AND SETLIN HAS BEEN CALLED TO SET OUTPUT POSITIONS          *                 
***************************************************************                 
         SPACE 1                                                                
DISFLD   NMOD1 0,**DISFLD                                                       
         L     RC,0(R1)                                                         
         L     R6,4(R1)            R6=A(TSAR DATA RECORD)                       
         MVC   PRTMODE,4(R1)       EXTRACT PRINT/DISPLAY MODE                   
         LA    R6,4(R6)            POINT TO FIELD DATA ELEMENT                  
         USING ACFDD,R6                                                         
         CLI   ACFDSROW,0          TEST FOR SCREEN DATA                         
         BE    DISFLD20            NO                                           
*                                                                               
         L     R2,AROW                                                          
         CLI   PRTMODE,0           TEST FOR PRINT OR SCREEN DISPLAY             
         BE    *+8                                                              
         LA    R2,ROWSPEC          ITS PRINT                                    
         MVC   LISTAR,SPACES                                                    
         MVC   BYTE,ACFDSROW       EXTRACT ROW VALUE                            
*                                                                               
         MVI   LISTAR,C'*'                                                      
         TM    BYTE,X'80'          TEST FOR AUTO                                
         BO    DISFLD1                                                          
*                                                                               
         LA    R4,LISTAR                                                        
         TM    BYTE,X'40'          TEST FOR PLUS FEATURE                        
         BZ    *+16                                                             
         MVI   0(R4),C'+'                                                       
         LA    R4,1(R4)                                                         
         NI    BYTE,X'FF'-X'40'     TURN OFF BIT                                
*                                                                               
         ZIC   R0,BYTE                                                          
         EDIT  (R0),(2,0(R4)),ALIGN=LEFT                                        
*                                                                               
DISFLD1  BAS   RE,MOVEOUT                                                       
*                                                                               
DISFLD2  L     R2,ACOL             DISPLAY COLUMN                               
         CLI   PRTMODE,0                                                        
         BE    *+8                                                              
         LA    R2,COLSPEC                                                       
         ZIC   R0,ACFDSCOL                                                      
         MVC   LISTAR,SPACES                                                    
         EDIT  (R0),(2,LISTAR),ALIGN=LEFT                                       
         BAS   RE,MOVEOUT                                                       
*                                                                               
DISFLD3  L     R2,ALEN             DISPLAY LENGTH                               
         CLI   PRTMODE,0                                                        
         BE    *+8                                                              
         LA    R2,LENSPEC                                                       
         ZIC   R0,ACFDSLEN                                                      
         MVC   LISTAR,SPACES                                                    
         EDIT  (R0),(2,LISTAR),ALIGN=LEFT                                       
         BAS   RE,MOVEOUT                                                       
*                                                                               
DISFLD4  TM    ACFDATTR,X'20'      TEST FOR PROTECTED FIELD                     
         BZ    DISFLD6                                                          
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVI   LISTAR,C'P'                                                      
         L     R2,APROT                                                         
         CLI   PRTMODE,0                                                        
         BE    *+8                                                              
         LA    R2,PROTSPEC                                                      
         BAS   RE,MOVEOUT                                                       
*                                                                               
DISFLD6  TM    ACFDATTR,X'08'      TEST FOR HIGH INTENSITY                      
         BZ    DISFLD8             NO                                           
         MVC   LISTAR,SPACES                                                    
         MVI   LISTAR,C'H'                                                      
         L     R2,AHI                                                           
         CLI   PRTMODE,0                                                        
         BE    *+8                                                              
         LA    R2,HIGHSPEC                                                      
         BAS   RE,MOVEOUT                                                       
*                                                                               
DISFLD8  TM    ACFDATTR,X'40'      TEST UPPER/LOWER CASE ALLOWED                
         BZ    DISFLD10                                                         
         MVC   LISTAR,SPACES                                                    
         MVI   LISTAR,C'L'                                                      
         L     R2,AUL                                                           
         CLI   PRTMODE,0                                                        
         BE    *+8                                                              
         LA    R2,ULSPEC                                                        
         BAS   RE,MOVEOUT                                                       
*                                                                               
DISFLD10 OC    ACFDFLD,ACFDFLD     TEST FOR FIELD KEYWORD                       
         BZ    DISFLD20                                                         
         L     R2,AKEY                                                          
         CLI   PRTMODE,0                                                        
         BE    *+8                                                              
         LA    R2,KEYSPEC                                                       
         MVC   LISTAR(L'ACFDFLD),ACFDFLD                                        
         BAS   RE,MOVEOUT                                                       
*                                                                               
DISFLD20 OC    ACFDPLIN(2),ACFDPLIN TEST FOR PRINT DEFN                         
         BZ    DISFLD30            NO                                           
         CLI   ACFDPLIN,0                                                       
         BE    DISFLD22                                                         
         L     R2,APLINE                                                        
         CLI   PRTMODE,0                                                        
         BE    *+8                                                              
         LA    R2,PLINSPEC                                                      
         MVC   LISTAR,SPACES                                                    
         ZIC   R0,ACFDPLIN                                                      
         EDIT  (R0),(2,LISTAR),ALIGN=LEFT                                       
         BAS   RE,MOVEOUT                                                       
*                                                                               
DISFLD22 L     R2,APCOL                                                         
         CLI   PRTMODE,0                                                        
         BE    *+8                                                              
         LA    R2,PCOLSPEC                                                      
         MVC   LISTAR,SPACES                                                    
         ZIC   R0,ACFDPCOL                                                      
         EDIT  (R0),(3,LISTAR),ALIGN=LEFT                                       
         BAS   RE,MOVEOUT                                                       
*                                                                               
DISFLD30 MVC   BLOCK(80),SPACES                                                 
         MVC   BLOCK+80(80),SPACES PRE-CLEAR STRING AREA                        
         LA    R4,BLOCK                                                         
         ZIC   R1,ACFDLEN          ELEMENT LENGTH                               
         ZIC   R0,ACFDPDLN         PRINT DATA LENGTH                            
         SR    R1,R0                                                            
         SH    R1,=Y(ACFDSCR-ACFDD) LESS OVERHEAD                               
         BZ    DISFLD32            NO SCREEN DATA                               
         MVC   0(2,R4),=C'S='                                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),ACFDSCR     EXTRACT SCREEN DATA                          
*                                                                               
         LA    R4,3(R1,R4)         ADVANCE OUTPUT POINTER                       
         CLI   ACFDPDLN,0          TEST ANY PRINT DATA                          
         BE    DISFLD32            NO                                           
         MVI   0(R4),C','          PLACE SEPARATING COMMA                       
         LA    R4,1(R4)            BUMP OUTPUT POINTER                          
*                                                                               
DISFLD32 SR    R1,R1                                                            
         ICM   R1,1,ACFDPDLN                                                    
         BZ    DISFLD34            NO PRINT DATA                                
*                                                                               
         CLI   ACFDPDTY,C'C'       TEST FOR CHARACTER STRING                    
         BE    *+12                                                             
         BAS   RE,DISINT           DISPLAY INTERNAL DATA                        
         B     DISFLD34                                                         
*                                                                               
         ZIC   RE,ACFDLEN          ELEMENT LENGTH                               
         LA    RE,ACFDD(RE)        POINT PAST ELEMENT                           
         SR    RE,R1               BACK UP TO START OF PRINT DATA               
*                                                                               
         MVC   0(2,R4),=C'P='                                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),0(RE)                                                    
         LA    R4,3(R1,R4)         POINT TO END OF STRING                       
*                                                                               
DISFLD34 LA    RE,BLOCK                                                         
         SR    R4,RE               COMPUTE L'STRING                             
         BZ    DISFLD40            NOTHING TO OUTPUT                            
*                                                                               
         LA    R0,L'PROSPA1        LENGTH OF SCREEN/PRINT DATA FIELDS           
         GOTO1 CHOPPER,DMCB,((R4),BLOCK),((R0),BLOCK+160),((R0),2),0            
         ICM   R3,15,DMCB+8                                                     
         BZ    DISFLD40                                                         
*                                                                               
         LA    R4,BLOCK+160        R4=OUTPUT POINTER                            
         L     R2,ASP1             R2=A(SCREEN HEADER)                          
         CLI   PRTMODE,0           TEST SCREEN CALL                             
         BNE   DISFLD36            NO                                           
         MVC   8(L'PROSPA1,R2),0(R4)                                            
         BAS   RE,BUMP                                                          
         LA    R4,L'PROSPA1(R4)                                                 
         BCT   R3,*-14                                                          
         B     DISFLD40                                                         
*                                                                               
DISFLD36 ZIC   R2,SPASPEC+1        GET DISP TO PRINT POSITION                   
         LA    R2,P(R2)                                                         
         MVC   0(L'PROSPA1,R2),0(R4)                                            
         LA    R2,L'P(R2)                                                       
         LA    R4,L'PROSPA1(R4)                                                 
         BCT   R3,*-14                                                          
*                                                                               
DISFLD40 DS    0H                                                               
*                                                                               
DISFLDX  XMOD1 1                                                                
         SPACE 2                                                                
* SUB-ROUTINE TO DISPLAY INTERNALLY GENERATED PRINT SPEC DATA                   
*                                                                               
DISINT   ST    RE,SAVERE                                                        
         ZIC   RE,ACFDLEN          RE=ELEM LENGTH                               
         ZIC   R1,ACFDPDLN                                                      
         SR    RE,R1               COMPUTE DISPLACEMENT TO PRINT DATA           
         LA    RE,ACFDD(RE)                                                     
         CLI   0(RE),C'&&'         TEST FOR OLD STYLE DATA                      
         BNE   DISINT10            NO                                           
*                                                                               
DISINT2  MVC   0(2,R4),=C'P='                                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),0(RE)                                                    
         LA    R4,3(R1,R4)         BUMP OUTPUT POINTER                          
         B     DISINTX                                                          
*                                                                               
DISINT10 LA    R1,0(RE,R1)         POINT TO END OF ELEMENT                      
         BCTR  R1,0                                                             
         SR    RF,RF                                                            
*                                                                               
DISINT12 CLI   0(RE),X'01'         TEST FOR INPUT ELEMENT                       
         BE    DISINT15            YES                                          
         IC    RF,1(RE)            GET MINI-ELEM LENGTH                         
         LA    RE,0(RF,RE)         POINT TO NEXT MINI-ELEMENT                   
         CR    RE,R1               TEST PAST END-OF ELEM                        
         BL    DISINT12            NO                                           
         B     DISINTX                                                          
*                                                                               
DISINT15 ZIC   R1,1(RE)                                                         
         SH    R1,=H'3'            DATA LENGTH                                  
         MVC   0(2,R4),=C'P='                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),2(RE)                                                    
         LA    R4,3(R1,R4)                                                      
*                                                                               
DISINTX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEOUT  ST    RE,SAVERE                                                        
         CLI   PRTMODE,0           TEST FOR SCREEN DISPLAY                      
         BNE   MOVEOUT2            NO                                           
*                                                                               
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     MOVEOUTX                                                         
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEOUT2 ZIC   R1,0(R2)            GET LENGTH OF OUTPUT                         
         BCTR  R1,0                                                             
         ZIC   RE,1(R2)            GET DISP FROM P                              
         LA    RE,P(RE)                                                         
         EX    R1,*+8                                                           
         B     MOVEOUTX                                                         
         MVC   0(0,RE),LISTAR                                                   
*                                                                               
MOVEOUTX L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* PRINT TABLE ENTRIES (BYTE 0=LENGTH OF OUTPUT, BYTES 1=DISP TO OUTPUT)         
*                                                                               
ROWSPEC  DC    AL1(2),AL1(7)                                                    
COLSPEC  DC    AL1(3),AL1(12)                                                   
LENSPEC  DC    AL1(2),AL1(17)                                                   
PROTSPEC DC    AL1(1),AL1(21)                                                   
HIGHSPEC DC    AL1(1),AL1(23)                                                   
ULSPEC   DC    AL1(1),AL1(25)                                                   
KEYSPEC  DC    AL1(4),AL1(29)                                                   
PLINSPEC DC    AL1(2),AL1(50)                                                   
PCOLSPEC DC    AL1(3),AL1(56)                                                   
SPASPEC  DC    AL1(0),AL1(134)                                                  
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*******************************************************                         
* SUB-ROUTINE TO EDIT THE FIELD DEFINITION            *                         
* ON EXIT, ELEM CONTAINS THE FIELD ELEMENT            *                         
*******************************************************                         
         SPACE 1                                                                
EDFLD    NMOD1 0,**EDFLD                                                        
         L     RC,0(R1)                                                         
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACFDD,R6                                                         
         MVI   ACFDEL,ACFDELQ                                                   
         MVI   ACFDLEN,ACFDSCR-ACFDD                                            
*                                                                               
EDFLD1   L     R2,AROW             EDIT SCREEN ROW                              
         CLI   5(R2),0                                                          
         BNE   EDFLD2                                                           
*                                                                               
* IF NOTHING HAS BEEN INPUT IN ROW, MAKE SURE THAT NONE OF THE                  
* REMAINING SCREEN DEFINITION FIELDS HAVE BEEN INPUT                            
*                                                                               
         L     R2,ACOL                                                          
         L     R5,APLINE                                                        
         BCTR  R5,0                R5=BXLE LIMIT                                
         SR    R4,R4                                                            
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BNE   ERREND2                                                          
         IC    R4,0(R2)            GET FIELD LENGTH                             
         BXLE  R2,R4,*-12                                                       
*                                                                               
         L     R2,APLINE           FORCE PRINT LINE INPUT                       
         GOTO1 ANY                                                              
         B     EDFLD20                                                          
*                                                                               
* EDIT ROW FIELD                                                                
*                                                                               
EDFLD2   GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   WORK,C'*'           TEST FOR AUTO ROW FEATURE                    
         BNE   EDFLD3              NO                                           
         CLI   5(R2),1             TEST ONLY ONE BYTE                           
         BNE   ERREND2                                                          
         OI    ACFDSROW,X'80'      TURN ON AUTO BIT                             
         B     EDFLD5                                                           
*                                                                               
EDFLD3   CLI   WORK,C'+'           TEST FOR PLUS FEATURE                        
         BNE   EDFLD4                                                           
*                                                                               
         CLI   5(R2),2             TEST FOR 2 BYTES                             
         BNE   ERREND2                                                          
         CLI   WORK+1,C'1'         TEST FOR NUMBER FOLLOWING                    
         BL    ERREND2                                                          
         CLI   WORK+1,C'9'                                                      
         BH    ERREND2                                                          
         NI    WORK+1,X'0F'        TURN OFF ZONE BITS                           
         OI    WORK+1,X'40'        TURN ON PLUS FEATURE BIT                     
         MVC   ACFDSROW,WORK+1                                                  
         B     EDFLD5                                                           
*                                                                               
EDFLD4   GOTO1 VALINUM                                                          
         MVI   ERROR,INVALID                                                    
         CLI   ACTUAL,9            TEST SPECIFIC ROW=9-23                       
         BL    ERREND2                                                          
         CLI   ACTUAL,23                                                        
         BH    ERREND2                                                          
         MVC   ACFDSROW,ACTUAL                                                  
*                                                                               
* EDIT COLUMN FIELD                                                             
*                                                                               
EDFLD5   L     R2,ACOL                                                          
         GOTO1 VALINUM                                                          
         MVI   ERROR,INVALID                                                    
         CLI   ACTUAL,79                                                        
         BH    ERREND2                                                          
         MVC   ACFDSCOL,ACTUAL                                                  
*                                                                               
* EDIT COLUMN LENGTH FIELD                                                      
*                                                                               
EDFLD6   L     R2,ALEN                                                          
         GOTO1 VALINUM                                                          
         MVI   ERROR,INVALID                                                    
         CLI   ACTUAL,79                                                        
         BH    ERREND2                                                          
         ZIC   R0,ACFDSCOL         GET COLUMN NUMBER                            
         ZIC   R1,ACTUAL                                                        
         AR    R0,R1                                                            
         BCTR  R0,0                                                             
         CH    R0,=H'79'           TEST OVERRUNNING SCREEN LINE                 
         BH    ERREND2                                                          
         MVC   ACFDSLEN,ACTUAL                                                  
*                                                                               
* EDIT PROTECTED FIELD                                                          
*                                                                               
EDFLD8   L     R2,APROT            EDIT PROTECTED FIELD                         
         CLI   5(R2),0                                                          
         BE    EDFLD9                                                           
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'P'                                                       
         BNE   ERREND2                                                          
         OI    ACFDATTR,X'20'                                                   
         B     EDFLD10                                                          
*                                                                               
EDFLD9   L     R2,ACOL             FOR UNPROTECTED FIELD                        
         MVI   ERROR,INVALID                                                    
         CLI   ACFDSCOL,2          TEST COL > 1                                 
         BL    ERREND2                                                          
*                                                                               
EDFLD10  L     R2,AHI              EDIT HIGH INTENSITY FIELD                    
         CLI   5(R2),0                                                          
         BE    EDFLD12                                                          
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'H'                                                       
         BNE   ERREND2                                                          
         OI    ACFDATTR,X'08'                                                   
*                                                                               
EDFLD12  L     R2,AUL              EDIT LOWER CASE                              
         CLI   5(R2),0                                                          
         BE    EDFLD14                                                          
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'L'                                                       
         BNE   ERREND2                                                          
         TM    ACFDATTR,X'20'      TEST FOR PROTECTED FIELD                     
         BO    ERREND2             YES-FIELD CANNOT BE LOWER CASE               
         OI    ACFDATTR,X'40'      NOTE LOWER CASE                              
*                                                                               
EDFLD14  L     R2,AKEY             EDIT FIELD KEYWORD                           
         CLI   5(R2),0                                                          
         BNE   EDFLD15                                                          
         TM    ACFDATTR,X'20'      TEST FOR PROTECTED FIELD                     
         BO    EDFLD20             YES                                          
         MVI   ERROR,MISSING       FORCE INPUT HERE IF UNP                      
         B     ERREND2                                                          
*                                                                               
EDFLD15  MVI   ERROR,INVALID                                                    
         TM    ACFDATTR,X'20'                                                   
         BO    ERREND2                                                          
         GOTO1 ANY                                                              
         ZIC   R0,5(R2)                                                         
         GOTO1 TSTAN,WORK                                                       
         MVC   ACFDFLD,WORK                                                     
*                                                                               
* EDIT THE PRINT DEFINITION                                                     
*                                                                               
EDFLD20  L     R2,APLINE                                                        
         CLI   5(R2),0                                                          
         BNE   EDFLD22             INPUT                                        
*                                                                               
         MVI   ERROR,MISSING                                                    
         L     RE,APCOL                                                         
         CLI   5(RE),0             TEST IF PRINT COL HAS INPUT                  
         BE    EDFLD30             NO PRINT DEFINITION                          
         B     ERREND2                                                          
*                                                                               
EDFLD22  GOTO1 VALINUM                                                          
         MVI   ERROR,INVALID                                                    
         CLI   ACTUAL,14           TEST NO MORE THAN 14                         
         BH    ERREND2                                                          
         MVC   ACFDPLIN,ACTUAL                                                  
*                                                                               
EDFLD24  L     R2,APCOL            EDIT PRINT COLUMN                            
         GOTO1 VALINUM                                                          
         MVI   ERROR,INVALID                                                    
         CLI   ACTUAL,132                                                       
         BH    ERREND2                                                          
         MVC   ACFDPCOL,ACTUAL                                                  
*                                                                               
* EDIT THE FREE FORM LINES FOR SCREEN AND PRINT TEXT                            
*                                                                               
EDFLD30  L     R2,ASP1             LOOK FOR SOME INPUT                          
         CLI   5(R2),0                                                          
         BNE   EDFLD32                                                          
         L     R2,ASP2                                                          
         CLI   5(R2),0             CANNOT BE ANYTHING IN SECOND FIELD           
         BE    EDFLD40             EDIT IS DONE                                 
         B     ERREND2                                                          
*                                                                               
EDFLD32  L     R4,AIO3             BUILD SCAN STRING AT IO3                     
         XC    0(8,R4),0(R4)                                                    
         L     R2,ASP1                                                          
         MVC   5(1,R4),5(R2)       EXTRACT DATA LENGTH                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),8(R2)                                                    
         LA    R1,9(R1)            COMPUTE TOTAL LENGTH                         
         STC   R1,0(R4)                                                         
         LA    RE,0(R1,R4)         POINT TO END OF STRING                       
*                                                                               
         L     R2,ASP2                                                          
         CLI   5(R2),0             TEST FOR ANYTHING IN SECOND FIELD            
         BE    EDFLD34             NO                                           
*                                                                               
         MVI   0(RE),C' '          INSERT A SPACE BETWEEN LINES                 
         LA    RE,1(RE)            BUMP POINTER                                 
*                                                                               
         ZIC   R1,5(R2)                                                         
         ZIC   RF,5(R4)                                                         
         LA    RF,1(R1,RF)         COMPUTE DATA LENGTH                          
         STC   RF,5(R4)                                                         
         LA    RF,8(RF)                                                         
         STC   RF,0(R4)                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)       ATTACH REST OF STRING                        
*                                                                               
EDFLD34  XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 PARSNIP,DMCB,AIO3,BLOCK,('PSNNONLQ',0)                           
         CLI   8(R1),0             TEST FOR ERROR                               
         BE    EDFLD35             NO                                           
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)          A(PARSNIP PROBLEM LOCATION)                  
         B     EDFREER                                                          
*                                                                               
EDFLD35  MVC   NCOMPS,4(R1)        SAVE N'COMPONENTS                            
         BAS   RE,EDFREE           EDIT FOR FREE FROM FIELD                     
*                                                                               
EDFLD40  CLI   ACFDPLIN,0          TEST FOR A PRINT SPEC                        
         BE    EDFLDX              NO                                           
         BAS   RE,CHKPRT           CHECK FOR DATA TO PRINT                      
*                                                                               
EDFLDX   XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* SUB-ROUTINE TO EDIT THE FREE FORM INPUT FIELDS--CALLED BY EDFLD *             
*******************************************************************             
         SPACE 1                                                                
EDFREE   NTR1  ,                                                                
         ZIC   R2,NCOMPS           R2=N'COMPONENTS                              
         LA    R4,BLOCK                                                         
         USING PSND,R4                                                          
*                                                                               
EDFREE2  MVI   ERROR,INVALID                                                    
         L     R3,PSNCOMP          R3=COMPONENT POINTER                         
         CLI   PSNTAG,PSNFLDQ      TEST FOR FIELD                               
         BNE   EDFREER             NO                                           
         CLI   PSNLEN,1            TEST FOR ONE BYTE                            
         BNE   EDFREER             NO                                           
         CLI   PSNVSEP,C'='        TEST KEYWORD PARM FIELD                      
         BNE   EDFREER                                                          
*                                                                               
         CLI   0(R3),C'S'          TEST 'S='                                    
         BE    EDFREE6             YES                                          
         CLI   0(R3),X'A2'         TEST LOWER CASE S                            
         BE    EDFREE6                                                          
         CLI   0(R3),C'P'                                                       
         BE    EDFREE20                                                         
         CLI   0(R3),X'97'         TEST LOWER CASE P                            
         BE    EDFREE20                                                         
         B     EDFREER                                                          
*                                                                               
EDFREE4  LA    R4,PSNL(R4)                                                      
         BCT   R2,EDFREE2                                                       
         B     EDFREEX                                                          
*                                                                               
* EDIT S'=' EXPRESSION                                                          
*                                                                               
EDFREE6  LA    R1,ACFDPRT-ACFDD                                                 
         ZIC   R0,ACFDPDLN                                                      
         AR    R1,R0               COMPUTE L'OF BASIC ELEM+PRT DATA             
         MVI   ERROR,DUPINPUT                                                   
         CLM   R1,1,ACFDLEN        TEST NO SCREEN DATA THERE                    
         BNE   EDFREER                                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   ACFDSROW,0                                                       
         BE    EDFREER                                                          
*                                                                               
         BAS   RE,NEXTFLD                                                       
         L     R3,PSNCOMP                                                       
         CLI   PSNTAG,PSNVALQ                                                   
         BNE   EDFREER                                                          
         MVI   ERROR,OVRLNERR                                                   
         CLC   PSNLEN,ACFDSLEN     TEST INPUT WILL NOT FIT IN FIELD             
         BH    EDFREER                                                          
*                                                                               
EDFREE8  CLI   ACFDPDLN,0          TEST PRINT DATA PRESENT                      
         BNE   EDFREE10            YES                                          
*                                                                               
* ATTACH SCREEN DATA TO END OF FIELD ELEMENT                                    
*                                                                               
         ZIC   R1,PSNLEN                                                        
         ZIC   RE,ACFDLEN                                                       
         AR    RE,R1               UPDATE ELEMENT LENGTH                        
         STC   RE,ACFDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     EDFREE4                                                          
         MVC   ACFDSCR(0),0(R3)    ATTACH SCREEN DATA                           
*                                                                               
* INSERT SCREEN DATA IN ELEMENT                                                 
*                                                                               
EDFREE10 ZIC   RE,ACFDLEN                                                       
         ZIC   R1,ACFDPDLN                                                      
         SR    RE,R1               DISP TO PRINT DATA                           
         LA    RE,ACFDD(RE)        RE=A(PRINT DATA)                             
         L     RF,AIO2             SAVE PRINT DATA IN IO2                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)                                                    
*                                                                               
         ZIC   RE,ACFDLEN                                                       
         ZIC   R1,PSNLEN           ATTACH SCREEN DATA                           
         AR    RE,R1                                                            
         STC   RE,ACFDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACFDSCR(0),0(R3)                                                 
*                                                                               
         LA    RE,ACFDSCR+1(R1)    POINT PAST SCREEN DATA                       
         ZIC   R1,ACFDPDLN                                                      
         L     RF,AIO2                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     EDFREE4                                                          
         MVC   0(0,RE),0(RF)       RE-ATTACH PRINT DATA                         
*                                                                               
* EDIT P= DATA                                                                  
*                                                                               
EDFREE20 MVI   ERROR,DUPINPUT                                                   
         CLI   ACFDPDLN,0          TEST PRINT DATA THERE                        
         BNE   EDFREER             YES                                          
         MVI   ERROR,FLKEYERR      TEST FOR KEYED FIELD                         
         OC    ACFDFLD,ACFDFLD                                                  
         BNZ   EDFREER             YES-NO CHARACTER OR INTERNAL DATA            
         MVI   ERROR,NOPLNERR      MUST HAVE PRINT LINE/COL                     
         CLI   ACFDPLIN,0                                                       
         BE    EDFREER                                                          
         BAS   RE,NEXTFLD                                                       
         ST    R4,AFSTINP                                                       
         ST    R4,ALSTINP                                                       
         L     R3,PSNCOMP                                                       
*                                                                               
EDFREE22 MVI   ERROR,INVALID                                                    
         CLI   PSNTAG,PSNVALQ      TEST 'P=' FOLLOWED BY VALUE                  
         BNE   EDFREER                                                          
         CLI   0(R3),C'&&'         TEST FIELD STARTS WITH AMPERSAND             
         BE    EDFREE25            YES DEAL W INTERNALLY GENERTD DATA           
*                                                                               
         ZIC   R1,PSNLEN                                                        
         ZIC   RE,ACFDLEN                                                       
         LR    RF,RE                                                            
         AR    RF,R1               UPDATE ELEM LEN                              
         STC   RF,ACFDLEN                                                       
         MVC   ACFDPDLN,PSNLEN     SET PRINT DATA LENGTH                        
         MVI   ACFDPDTY,C'C'       NOTE CHARACTER STRING                        
*                                                                               
         LA    RE,ACFDD(RE)                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     EDFREE4                                                          
         MVC   0(0,RE),0(R3)       ATTACH CHARACTER STRING                      
*                                                                               
EDFREE25 MVI   ACFDPDTY,C'I'                                                    
         MVI   PARML,0                                                          
         MVC   PARMS,SPACES                                                     
*                                                                               
         BAS   RE,EXTINP           EXTRACT INPUT                                
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                ADJUST LENGTH FOR AMPERSAND                  
         STC   R1,BYTE             SAVE IT                                      
         BCTR  R1,0                EXECUTE LENGTH                               
         LA    RE,INTTAB                                                        
         USING INTTABD,RE                                                       
         LA    R0,INTNTRYS                                                      
         MVI   ERROR,INTERERR                                                   
         CLI   BYTE,L'INTNAME      TEST NAME IS TOO LONG                        
         BH    EDFREER                                                          
*                                                                               
EDFREE27 CLC   BYTE,INTMINL                                                     
         BL    EDFREE28                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LISTAR+1(0),INTNAME                                              
         BE    EDFREE30                                                         
EDFREE28 LA    RE,INTTABL(RE)                                                   
         BCT   R0,EDFREE27                                                      
         B     EDFREER                                                          
*                                                                               
EDFREE30 ST    RE,AINT             SAVE A(INTERNAL ENTRY)                       
         OC    INTDISP,INTDISP     TEST TO PROCESS PARAMETERS                   
         BNZ   EDFREE32                                                         
*                                                                               
         BAS   RE,ADDINP                                                        
         BAS   RE,ADDRTN                                                        
         B     EDFREE4                                                          
*                                                                               
EDFREE32 SR    RF,RF                                                            
         ICM   RF,3,INTDISP                                                     
         LA    RF,EDFLD(RF)        RELOCATE ADDRESS                             
         BASR  RE,RF                                                            
*                                                                               
         BAS   RE,ADDINP                                                        
         BAS   RE,ADDRTN                                                        
         BAS   RE,ADDPARM                                                       
         B     EDFREE4                                                          
*                                                                               
EDFREEX  XIT1                                                                   
*                                                                               
* SUB-ROUTINE TO VALIDATE PARAMETERS WITH &UF INTERNAL DATA                     
*                                                                               
VALUF    NTR1  ,                                                                
         ZIC   R1,PSNLEN                                                        
         LA    R3,1(R1,R3)         ADVANCE R3                                   
         MVI   ERROR,MISSING                                                    
         BAS   RE,NEXTFLD                                                       
*                                                                               
         ST    R4,ALSTINP          SET LAST INPUT FIELD                         
         L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         CLI   PSNTAG,PSNFLDQ                                                   
         BNE   EDFREER                                                          
         CLI   PSNVSEP,0           NO VALUE FIELD FOLLOWING                     
         BNE   EDFREER                                                          
         CLI   PSNLEN,2            TEST FOR 2-BYTE FIELD                        
         BNE   EDFREER                                                          
         BAS   RE,GETPARM                                                       
         XIT1  REGS=(R2,R4)                                                     
         SPACE 1                                                                
* SUB-ROUTINE TO VALIDATE PARAMETERS WITH &UFALL INTERNAL DATA                  
*                                                                               
VALUFALL NTR1  ,                                                                
         ZIC   R1,PSNLEN                                                        
         LA    R3,1(R1,R3)         ADVANCE R3                                   
         MVI   ERROR,MISSING                                                    
         BAS   RE,NEXTFLD                                                       
*                                                                               
         ST    R4,ALSTINP          SET LAST INPUT FIELD                         
         L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         CLI   PSNTAG,PSNFLDQ                                                   
         BNE   EDFREER                                                          
         MVI   ERROR,NOTNUM                                                     
         TM    PSNSTAT,PSNNUMQ     TEST FOR NUMERIC DATA                        
         BZ    EDFREER                                                          
         MVI   ERROR,INVALID                                                    
         ICM   R0,15,PSNNUM                                                     
         BZ    EDFREER                                                          
         CH    R0,=H'132'                                                       
         BH    EDFREER                                                          
         STC   R0,PARMS                                                         
         MVI   PARML,1             SET PARAMETER LENGTH                         
         XIT1  REGS=(R2,R4)                                                     
         SPACE 1                                                                
* SUB-ROUTINE TO VALIDATE &UFLIST PARAMETERS                                    
*                                                                               
VALUFL   NTR1  ,                                                                
         ZIC   R1,PSNLEN                                                        
         LA    R3,1(R1,R3)                                                      
         MVI   ERROR,MISSING                                                    
         BAS   RE,NEXTFLD                                                       
*                                                                               
         ST    R4,ALSTINP                                                       
         L     R3,PSNCOMP                                                       
         MVI   ERROR,INVALID                                                    
         CLI   PSNTAG,PSNFLDQ                                                   
         BNE   EDFREER                                                          
         MVI   ERROR,NOTNUM                                                     
         TM    PSNSTAT,PSNNUMQ                                                  
         BZ    EDFREER                                                          
         MVI   ERROR,INVALID                                                    
         ICM   R0,15,PSNNUM                                                     
         BZ    EDFREER                                                          
         CH    R0,=H'132'                                                       
         BH    EDFREER                                                          
         STC   R0,PARMS                                                         
         MVI   PARML,1                                                          
*                                                                               
VALUFL2  ZIC   R1,PSNLEN                                                        
         LA    R3,1(R1,R3)                                                      
         MVI   ERROR,MISSING                                                    
         BAS   RE,NEXTFLD                                                       
*                                                                               
VALUFL4  ST    R4,ALSTINP                                                       
         L     R3,PSNCOMP                                                       
         BAS   RE,GETPARM                                                       
         MVI   ERROR,INVALID                                                    
         CLI   PSNTAG,PSNFLDQ                                                   
         BNE   EDFREER                                                          
         CLI   PSNLEN,2                                                         
         BNE   EDFREER                                                          
         LA    RE,PSNL(R4)         LOOK AHEAD AT NEXT FIELD                     
         CLI   0(RE),0                                                          
         BE    VALUFLX                                                          
         CLI   PSNTAG-PSND(RE),PSNFLDQ                                          
         BNE   VALUFLX                                                          
         CLI   PSNVSEP-PSND(RE),C'='                                            
         BE    VALUFLX                                                          
         BAS   RE,NEXTFLD                                                       
         B     VALUFL4                                                          
*                                                                               
VALUFLX  XIT1  REGS=(R2,R4)                                                     
         SPACE 1                                                                
* SUB-ROUTINE TO ADVANCE TO NEXT PARSNIP FIELD                                  
*                                                                               
NEXTFLD  LA    R4,PSNL(R4)                                                      
         BCT   R2,*+8                                                           
         B     EDFREER                                                          
         BR    RE                                                               
*                                                                               
EDFREER  L     R1,AIO3             START OF STRING                              
         LA    R1,8(R1)            ADJUST FOR DUMMY FIELD HEADER                
         SR    R3,R1               DISPLACEMENT TO ERROR                        
         L     R2,ASP1             R2=A(FIRST FIELD HEADER)                     
         CLM   R3,1,5(R2)          TEST ERROR IN FIRST FIELD                    
         BNH   EDFREERX            YES                                          
         ZIC   R1,5(R2)                                                         
         LA    R1,1(R1)            ADD IN INSERTED SPACE                        
         SR    R3,R1               DISPLACEMENT INTO SECOND FIELD               
         L     R2,ASP2                                                          
*                                                                               
EDFREERX STC   R3,ERRNDX           SET INDEX                                    
         B     ERREND2                                                          
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR ALPHANUMERIC FIELD                                   
*                                                                               
* AT ENTRY, R0=N'BYTES TO CHECK, R1=A(STRING TO CHECK)                          
*                                                                               
TSTAN    ST    RE,SAVERE                                                        
         MVI   ERROR,INVALID                                                    
*                                                                               
TSTAN1   CLI   0(R1),C'0'                                                       
         BL    *+16                                                             
         CLI   0(R1),C'9'                                                       
         BH    ERREND2                                                          
         B     TSTAN2                                                           
         CLI   0(R1),C'A'                                                       
         BL    ERREND2                                                          
         CLI   0(R1),C'I'                                                       
         BNH   TSTAN2              CHARACTER IS BETWEEN A-I                     
         CLI   0(R1),C'J'                                                       
         BL    ERREND2                                                          
         CLI   0(R1),C'R'                                                       
         BNH   TSTAN2                                                           
         CLI   0(R1),C'S'                                                       
         BL    ERREND2                                                          
         CLI   0(R1),C'Z'                                                       
         BH    ERREND2                                                          
*                                                                               
TSTAN2   LA    R1,1(R1)            NEXT CHARACTER IN FIELD                      
         BCT   R0,TSTAN1                                                        
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK FOR DATA TO PRINT                                        
*                                                                               
CHKPRT   ST    RE,SAVERE                                                        
         L     R2,APLINE                                                        
         MVI   ERROR,PRTERR                                                     
         OC    ACFDFLD,ACFDFLD     TEST FOR FIELD KEYWORD                       
         BNZ   CHKPRTX                                                          
         CLI   ACFDPDTY,0                                                       
         BE    ERREND2                                                          
         CLI   ACFDPDLN,0                                                       
         BE    ERREND2                                                          
*                                                                               
CHKPRTX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EXTRACT DATA POINTED AT BY A PARSNIP BLOCK                     
* INTO LISTAR AND TO SPACE PAD FIELD                                            
*                                                                               
EXTINP   MVC   LISTAR,SPACES                                                    
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTAR(0),0(R3)                                                  
         EX    R1,*+6                                                           
         BR    RE                                                               
         OC    LISTAR(0),SPACES                                                 
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE PARAMETER DATA POINTED AT BY A PARSNIP                 
* BLOCK AND ATTACH IT TO THE PARAMETER STRING                                   
*                                                                               
GETPARM  ST    RE,SAVERE                                                        
         BAS   RE,EXTINP                                                        
         ZIC   RE,PARML                                                         
         ZIC   R1,PSNLEN                                                        
         LR    RF,RE               SAVE LENGTH IN RF                            
         LA    RE,PARMS(RE)                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),LISTAR                                                   
         LA    RF,1(R1,RF)                                                      
         STC   RF,PARML                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO ADD THE INPUT MINI-ELEMENT                                     
*                                                                               
ADDINP   NTR1  ,                                                                
         XC    LISTAR,LISTAR                                                    
         MVI   LISTAR,X'01'                                                     
         L     R2,AFSTINP                                                       
         L     R3,ALSTINP                                                       
         L     RF,PSNCOMP-PSND(R2) RF=A(STRING BEGINNING)                       
         L     RE,PSNCOMP-PSND(R3) RE=A(END OF STRING)                          
         ZIC   R5,PSNLEN-PSND(R3)  GET L'LAST COMPONENT                         
         LR    R1,RE                                                            
         SR    R1,RF               COMPUTE DISPL WITH 1ST + LST STRING          
         AR    R1,R5               COMPUTE TOTAL LENGTH                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTAR+2(0),0(RF)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    LISTAR+2(0),SPACES                                               
         LA    R1,3(R1)                                                         
         STC   R1,LISTAR+1                                                      
*                                                                               
ADDINP2  ZIC   RE,ACFDLEN                                                       
         LR    RF,RE                                                            
         LA    RE,ACFDD(RE)        RE=A(NEXT POSITION)                          
         ZIC   R0,ACFDPDLN                                                      
         AR    R0,R1                                                            
         STC   R0,ACFDPDLN                                                      
         AR    RF,R1                                                            
         STC   RF,ACFDLEN                                                       
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),LISTAR                                                   
         XIT1                                                                   
         SPACE 2                                                                
* SUB-ROUTINE TO ADD THE ROUTINE MINI-ELEMENT                                   
*                                                                               
ADDRTN   ST    RE,SAVERE                                                        
         L     RE,AINT                                                          
         USING INTTABD,RE                                                       
         XC    WORK(L'INTROUT+2),WORK                                           
         MVI   WORK,X'02'                                                       
         MVI   WORK+1,L'INTROUT+2                                               
         MVC   WORK+2(L'INTROUT),INTROUT                                        
         ZIC   RE,ACFDLEN                                                       
         LR    RF,RE                                                            
         ZIC   R1,ACFDPDLN                                                      
         LA    RE,ACFDD(RE)                                                     
         MVC   0(L'INTROUT+2,RE),WORK                                           
         LA    R1,L'INTROUT+2(R1)  NEW PRINT DATA LENGTH                        
         STC   R1,ACFDPDLN                                                      
         LA    RF,L'INTROUT+2(RF)                                               
         STC   RF,ACFDLEN                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO ADD THE PARAMETER MINI-ELEMENT                                 
*                                                                               
ADDPARM  ST    RE,SAVERE                                                        
         ZIC   RE,ACFDLEN          GET ELEMENT LENGTH                           
         LR    RF,RE               SAVE ELEM LEN IN RF                          
         ZIC   R0,ACFDPDLN         R0=PRINT DATA LENGTH                         
         ZIC   R1,PARML                                                         
         LA    RE,ACFDD(RE)                                                     
         MVI   0(RE),X'03'                                                      
         LA    R1,2(R1)                                                         
         STC   R1,1(RE)                                                         
         AR    R0,R1               COMPUTE NEW DATA LEN                         
         STC   R0,ACFDPDLN                                                      
         AR    RF,R1                                                            
         STC   RF,ACFDLEN                                                       
*                                                                               
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RE),PARMS                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
ERREND2  LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVI   TSACTN,TSASAV       SAVE TSAR BLOCK BEFORE EXITING               
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VERRCUR                                                          
         SPACE 2                                                                
* TABLE OF INTERNALLY GENERATED DATA CODES                                      
*                                                                               
INTTAB   DS    0CL(INTTABL)                                                     
         DC    CL8'PG      ',AL1(2,0)  PAGE                                     
         DC    CL8'PG      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'PE      ',AL1(2,0)  PRODUCTION ESTIMATE                      
         DC    CL8'PE      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'CC      ',AL1(2,0)  CLIENT CODE                              
         DC    CL8'CC      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'CN      ',AL1(2,0)  CLIENT NAME                              
         DC    CL8'CN      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'PC      ',AL1(2,0)  PRODUCT CODE                             
         DC    CL8'PC      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'PN      ',AL1(2,0)  PRODUCT NAME                             
         DC    CL8'PN      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'JC      ',AL1(2,0)  JOB CODE                                 
         DC    CL8'JC      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'JN      ',AL1(2,0)  JOB NAME                                 
         DC    CL8'JN      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'OP      ',AL1(2,0)  JOB OPEN DATE                            
         DC    CL8'OP      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'CL      ',AL1(2,0)  JOB CLOSE DATE                           
         DC    CL8'CL      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'BA      ',AL1(2,0)  BILLING ADDRESS                          
         DC    CL8'BA      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'PB      ',AL1(2,0)  PRINT ON BILLS                           
         DC    CL8'PB      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'MC      ',AL1(2,0)  MEDIA CODE                               
         DC    CL8'MC      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'MN      ',AL1(2,0)  MEDIA NAME                               
         DC    CL8'MN      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'TD      ',AL1(2,0)  TODAY'S DATE                             
         DC    CL8'TD      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'RT      ',AL1(2,0)  RUN TIME                                 
         DC    CL8'RT      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'RTS     ',AL1(3,0)  RUN TIME SOON ONLY                       
         DC    CL8'RTS     ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'AN      ',AL1(2,0)  AGENCY NAME (USER ID)                    
         DC    CL8'AN      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'AA      ',AL1(2,0)  AGENCY ADDRESS (USER ID)                 
         DC    CL8'AA      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'CON     ',AL1(3,0)  COMPANY NAME                             
         DC    CL8'CON     ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'COA     ',AL1(3,0)  COMPANY ADDRESS                          
         DC    CL8'COA     ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'OEAPPROV',AL1(3,0)  ORIGINAL EST APPROVER                    
         DC    CL8'OEA     ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'OEPREP  ',AL1(3,0)  ORIGINAL EST PREPARER                    
         DC    CL8'OEP     ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'CEAPPROV',AL1(3,0)  CURRENT EST APPROVER                     
         DC    CL8'CEA     ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'CEPREP  ',AL1(3,0)  CURRENT EST PREPARER                     
         DC    CL8'CEP     ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'OEAD    ',AL1(4,0)  ORIGINAL EST APPROVED DATE               
         DC    CL8'OEAD    ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'OEPD    ',AL1(4,0)  ORIGINAL EST PREPARED DATE               
         DC    CL8'OEPD    ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'CEAD    ',AL1(4,0)  CURRENT EST APPROVED DATE                
         DC    CL8'CEAD    ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'CEPD    ',AL1(4,0)  CURRENT EST PREPARED DATE                
         DC    CL8'CEPD    ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'UF      ',AL1(2,0)  USER FIELD                               
         DC    CL8'UF      ',AL2(VALUF-EDFLD),AL4(0)                            
*                                                                               
         DC    CL8'UFALL   ',AL1(5,0)  USER FIELD ALL                           
         DC    CL8'UFALL   ',AL2(VALUFALL-EDFLD),AL4(0)                         
*                                                                               
         DC    CL8'UFLIST  ',AL1(6,0)  USER FIELD LIST                          
         DC    CL8'UFLIST  ',AL2(VALUFL-EDFLD),AL4(0)                           
*                                                                               
         DC    CL8'BD      ',AL1(2,0)  BILL DATE                                
         DC    CL8'BD      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'BN      ',AL1(2,0)  BILL NUMBER                              
         DC    CL8'BN      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'DD      ',AL1(2,0)  DUE DATE                                 
         DC    CL8'DD      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'CUR     ',AL1(3,0)  CURRENCY CODE                            
         DC    CL8'CUR     ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'OA      ',AL1(2,0)  OFFICE ADDRESS                           
         DC    CL8'OA      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'CV      ',AL1(2,0)  CLIENT VAT REG NUMBER                    
         DC    CL8'CV      ',AL2(0),AL4(0)                                      
*                                                                               
         DC    CL8'BST     ',AL1(3,0)  BILL STATUS                              
         DC    CL8'BST     ',AL2(0),AL4(0)                                      
*                                                                               
INTNTRYS EQU   (*-INTTAB)/L'INTTAB                                              
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0X                                                               
INTMODE  DS    X                   INTERNAL MODE                                
KEYCHG   DS    C                                                                
QPANEL   DS    CL(L'ACPNCODE)                                                   
SCROLL   DS    X                                                                
*                                                                               
SAVERE   DS    A                                                                
VTSAR    DS    V                                                                
ABUFF    DS    V                                                                
*                                                                               
AAFTER   DS    A                                                                
PREV     DS    X                                                                
NEXT     DS    X                                                                
*                                                                               
ATHISLIN DS    A                                                                
ASEL     DS    A                                                                
AROW     DS    A                                                                
ACOL     DS    A                                                                
ALEN     DS    A                                                                
APROT    DS    A                                                                
AHI      DS    A                                                                
AUL      DS    A                                                                
AKEY     DS    A                                                                
APLINE   DS    A                                                                
APCOL    DS    A                                                                
ASP1     DS    A                                                                
ASP2     DS    A                                                                
ANEXTSEL DS    A                                                                
*                                                                               
ALASTF   DS    A                   A(LAST FIELD DATA ELEM)                      
LASTNUM  DS    X                                                                
LASTSEQ  DS    X                                                                
*                                                                               
PREVREC  DS    H                                                                
*                                                                               
AINT     DS    A                   A(INTERNAL DATA TABLE ENTRY)                 
AFSTINP  DS    A                   A(FIRST PARSNIP FIELD)                       
ALSTINP  DS    A                   A(LAST PARSNIP FIELD)                        
PARML    DS    X                                                                
PARMS    DS    CL80                                                             
NCOMPS   DS    X                                                                
*                                                                               
PRTMODE  DS    X                                                                
NENT     DS    X                                                                
NINS     DS    X                                                                
INSTAB   DS    (MAXRECS)XL(L'LINKRNUM)                                          
DISTAB   DS    XL(MAXRECS*L'LINKRNUM)                                           
*                                  CHECK S/R VALUES                             
LASTEND  DS    H                                                                
LASTATTB DS    X                                                                
LASTROW  DS    X                                                                
LASTCOL  DS    X                                                                
THISROW  DS    X                                                                
THISCOL  DS    X                                                                
NKEYS    DS    X                                                                
ERRSW    DS    C                                                                
SCRSW    DS    C                                                                
*                                                                               
         DS    0F                                                               
TSARBLK  DS    XL(TSARDL)                                                       
*                                                                               
         DS    0F                                                               
TWAPARM  DS    XL(TWAPARML)                                                     
TWAELEM  DS    XL(TWAELLNQ+80)                                                  
*                                                                               
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROC6D                                                       
         SPACE 2                                                                
LSAVES   DS    0D                                                               
STARTREC DS    H                                                                
FIRSTREC DS    H                                                                
LASTREC  DS    H                                                                
LNRECS   DS    X                   N'RECORDS ON SCREEN                          
LSELTAB  DS    CL(MAXRECS*SELTABL)                                              
*                                                                               
         DS    0H                                                               
MOVEDETS DS    0XL6                                                             
FROM     DS    H                                                                
LOCATION DS    H                                                                
POSITION DS    C                                                                
MOVECOPY DS    C                                                                
*                                                                               
HIENT    DS    X                                                                
HIREC    DS    H                                                                
LINKLST  DS    XL(MAXLST*L'LINKREC)                                             
         DS    CL((SAVAREA-LSAVES)-(*-LSAVES))  SPARE                           
         EJECT                                                                  
         ORG   CONTAGH             TEXT SCREEN DSECT                            
       ++INCLUDE ACPROC8D                                                       
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXRECS  EQU   4                                                                
MAXLST   EQU   150                                                              
LISTFLDS EQU   12                  NUMBER OF FIELDS                             
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
         SPACE 2                                                                
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELACT   DS    C                   SELECT ACTION                                
SELKEY   DS    CL2                                                              
SELTABL  EQU   *-SELTABD                                                        
         SPACE 2                                                                
* DSECT TO COVER LINKED LIST RECORD                                             
*                                                                               
LINKD    DSECT                                                                  
LINKREC  DS    0CL4                                                             
LINKPREV DS    X                                                                
LINKNEXT DS    X                                                                
         ORG   LINKPREV                                                         
LINKLAST DS    X                                                                
LINKFRST DS    X                                                                
LINKRNUM DS    H                                                                
         SPACE 2                                                                
* DSECT TO COVER INTERNAL DATA TABLE ENTRY                                      
*                                                                               
INTTABD  DSECT                                                                  
INTNAME  DS    CL8                 INPUT NAME                                   
INTMINL  DS    X                   MINIMUM INPUT LENGTH                         
INTIND   DS    X                   INDICATORS                                   
INTROUT  DS    CL8                 ROUTINE NAME                                 
INTDISP  DS    XL2                 DISPLACEMENT TO VALIDATION ROUTINE           
         DS    XL4                 SPARE                                        
INTTABL  EQU   *-INTTABD           ENTRY LENGTH                                 
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
       ++INCLUDE DDTWABLDD                                                      
         EJECT                                                                  
       ++INCLUDE DDPARSNIPD                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACPRO36   09/12/02'                                      
         END                                                                    
