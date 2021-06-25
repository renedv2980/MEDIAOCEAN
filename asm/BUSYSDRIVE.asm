*          DATA SET BUSYSDRIVE AT LEVEL 017 AS OF 05/01/02                      
*PHASE T00A61A                                                                  
*INCLUDE TEXTIO                                                                 
         TITLE 'BUDRIVER - SYSTEM DRIVER FOR BUDGET (ABC)'                      
BUDRIVER CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 BUWORKX-BUWORK,**BUDV**,R9,CLEAR=YES                             
         USING BUWORK,RC                                                        
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     R7,GLAWORKD                                                      
         USING GEND,R7                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         SPACE 1                                                                
         CLI   GLHOOK,GLRESOLV                                                  
         BNE   BU2                                                              
         BAS   RE,SYSRES           EITHER RESOLVING ADDRESSES                   
         B     XIT                                                              
         SPACE 1                                                                
BU2      CLI   GLHOOK,GLROUT                                                    
         BNE   XIT                                                              
         BAS   RE,SYSEXEC          OR EXECUTING ROUTINES                        
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              RESOLVING ROUTINE ADDRESSES                                      
         SPACE 3                                                                
SYSRES   NTR1                                                                   
         LA    R1,ROUTLIST                                                      
         SPACE 1                                                                
SYSRES2  CLC   0(8,R1),GLLABEL                                                  
         BE    SYSRES4                                                          
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    SYSRES4                                                          
         B     SYSRES2                                                          
         SPACE 1                                                                
SYSRES4  MVC   GLAROUT,8(R1)       RETURN ADDRESS                               
         B     XIT                                                              
         SPACE 1                                                                
ROUTLIST DS    0F                                                               
         DC    C'ABCROWS ',A(ABCROWS)                                           
         DC    C'HEADPERD',A(HEADPERD)                                          
         DC    C'HEADASAT',A(HEADASAT)                                          
         DC    C'ABCSTACK',A(STACK)                                             
         DC    C'ABCTEXT ',A(ABCTEXT)                                           
         DC    C'REFNUM  ',A(REF)                                               
         DC    C'REFLET  ',A(REF)                                               
         DC    C'REFNUMA ',A(REF)                                               
         DC    C'REFLETA ',A(REF)                                               
         DC    C'ENDTEXT ',A(ENDTEXT)                                           
         DC    C'FOOTNUM ',A(FOOT)                                              
         DC    C'FOOTLET ',A(FOOT)                                              
         DC    C'FOOTNUMA',A(FOOT)                                              
         DC    C'FOOTLETA',A(FOOT)                                              
         DC    X'FF',CL7' ',A(DATATYPE)                                         
         EJECT                                                                  
*              EXECUTING ROUTINES (ROWS)                                        
         SPACE 3                                                                
SYSEXEC  NTR1                                                                   
         L     RF,GLAROUT                                                       
         L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         BR    RF                                                               
         SPACE 1                                                                
ABCROWS  CLI   GLABCLOW,0          TEST IF LOWEST LEVEL SET                     
         BNE   *+8                 YES                                          
         BAS   RE,GETLOW                                                        
         L     R4,GLABCATO         ROW NAMES                                    
         USING OUTTABD,R4                                                       
         ZIC   R1,OUTLVLN          PICK UP LEVEL NUMBER                         
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         LA    R0,3                INDENT 3 POSITIONS PER LEVEL                 
         CLI   GLABCLOW,5          UNLESS PLAN HAS 5 OR 6 LEVELS                
         BL    *+8                                                              
         LA    R0,2                                                             
         MR    R0,R0               COMPUTE DISPLACEMENT INTO PRINT LINE         
         AR    R2,R1               DISPLACE INTO PRINT LINE                     
*                                  AND MOVE OUT OUTLINE NAME                    
         CLI   GLABCMOD,3          UNLESS THIS IS A TOTAL                       
         BE    ABCROWS2                                                         
         MVC   0(L'OUTNAME,R2),OUTNAME                                          
         B     XIT                                                              
         SPACE 1                                                                
ABCROWS2 CLI   OUTLVLN,0           DO PLAN TOTALS BELOW                         
         DROP  R4                                                               
         BE    ABCROWS4                                                         
         MVC   3(5,R2),=C'*ALL*'   SHOW TOTALS AS *ALL* IN NEXT COL             
         B     XIT                                                              
         SPACE 1                                                                
ABCROWS4 MVC   0(11,R2),=C'PLAN TOTALS'                                         
         B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE LOWEST OUTLINE LEVEL                                   
*                                                                               
GETLOW   NTR1                                                                   
         L     R1,GLABCBPL         R1=A(BUILDER PARMS)                          
         USING BUPARMD,R1                                                       
         L     R4,BUPAOUTT         R4=A(OUTLINE TABLE)                          
         USING OUTTABD,R4                                                       
*                                                                               
GETLOW2  OC    OUTSEQN,OUTSEQN     TEST FOR EOT                                 
         BZ    GETLOWX                                                          
         CLI   OUTLVLN,0                                                        
         BE    GETLOW4                                                          
         TM    OUTIND1,X'40'       TEST FOR SUPPRESSED LINE                     
         BO    GETLOW4                                                          
         CLC   GLABCLOW,OUTLVLN    TEST IF HIGHEST SO FAR                       
         BH    *+10                NO                                           
         MVC   GLABCLOW,OUTLVLN                                                 
*                                                                               
GETLOW4  LA    R4,OUTTABL(R4)                                                   
         B     GETLOW2                                                          
*                                                                               
GETLOWX  B     XIT                                                              
         DROP  R1,R4                                                            
         EJECT                                                                  
*              EXECUTING ROUTINES (HEADLINES)                                   
         SPACE 3                                                                
HEADPERD MVC   0(16,R3),GLARGS      PERIOD TO HEADLINES                         
         B     XIT                                                              
         SPACE 1                                                                
HEADASAT MVI   0(R3),C'('          AS AT - SHOW AS (MMMDD)                      
         GOTO1 DATCON,DMCB,(3,GLARGS),(4,1(R3))                                 
         MVI   6(R3),C')'                                                       
         B     XIT                                                              
         EJECT                                                                  
* PRINT A COLUMN CONTAINING STACKED DATATYPE NAMES                              
*                                                                               
STACK    CLI   GLABCMOD,1          TEST FOR NON-DETAIL OUTLINE                  
         BE    STACKX              YES-DO NOT PRINT DATATYPE LABELS             
         ICM   R4,15,GLARGS        R4=A(STACK TABLE)                            
         BZ    STACKX                                                           
         ZIC   R1,GLARGS+4         GET COLUMN LENGTH                            
         BCTR  R1,0                SET UP FOR EXECUTE                           
*                                                                               
STACK2   CLI   0(R4),0             TEST FOR EOT                                 
         BE    STACKX                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),14(R4)      DATATYPE NAME                                
         LA    R4,34(R4)           NEXT STACK TABLE ENTRY                       
         LA    R3,198(R3)          NEXT PRINT LINE                              
         B     STACK2                                                           
*                                                                               
STACKX   B     XIT                                                              
         EJECT                                                                  
*              EXECUTING ROUTINES (TEXT)                                        
         SPACE 3                                                                
ABCTEXT  MVI   0(R2),C' '          TEXT DATA                                    
         MVC   1(199,R2),0(R2)     PRECLEAR TEXT TO SPACES                      
         CLI   GLABCMOD,3          TEST FOR TOTALS                              
         BE    XIT                 YES-SKIP TEXT PRINTING                       
         LA    R6,TEXTBLOK         SET BLOCK FOR TEXTIO                         
         USING TXTIOD,R6                                                        
         XC    TXTBLOCK(TXTLBLOK),TXTBLOCK                                      
         MVC   TXTACOM,GLCOMFAC                                                 
         ST    R2,TXTAAREA                                                      
         MVC   TXTAGY,GLAGENCY                                                  
         L     R1,GLABCATO                                                      
         USING OUTTABD,R1                                                       
         MVC   TXTNODE,OUTNODE                                                  
         MVC   TXTCODE,OUTCODE                                                  
         ZIC   RE,GLABCNCC                                                      
         LA    RF,1(RE)            INCREMENT NEXT COLUMN NUMBER                 
         STC   RF,GLABCNCC                                                      
         SLL   RE,1                NEXT COLUMN X 2                              
         LA    RE,GLABCTCC-2(RE)   INDEX TO NEXT TEXT COLUMN                    
         MVC   TXTFILT,0(RE)                                                    
         MVI   TXTMAX,4            MAX OF 4 LINES OF TEXT                       
         MVI   TXTWIDE,50          50 CHARACTERS WIDE                           
         MVI   TXTOPT,1            ONLY WANT ONE TEXT RECORD                    
         GOTO1 =V(TEXTIO),DMCB,(R6)                                             
         B     XIT                                                              
         EJECT                                                                  
*              EXECUTING ROUTINES (ENDTEXT)                                     
         SPACE 3                                                                
ENDTEXT  L     R2,GLAINTD          R2=A(INTERNAL RECORD DETAILS)                
         USING GLINTD,R2                                                        
         LH    R4,GLPDISP          R4=DISP TO LEFT MARGIN                       
         LH    R5,GLPWIDTH         R5=L'PRINT LINE                              
         LA    RE,LENENDS                                                       
         CR    R5,RE               TEST IF PRINT LINE LT END LINE LEN           
         BNL   ENDTEXT2            NO                                           
*                                                                               
         SR    RE,R5               YES-TRY TO CENTER THE END LINES              
         SRL   RE,1                SPLIT EXCESS EVENLY                          
         SR    R4,RE                                                            
         BP    ENDTEXT2                                                         
         LH    R4,GLPDISP          CANNOT DO IT                                 
*                                                                               
ENDTEXT2 ST    R4,ENDDISP          SAVE DISP TO PRINT POSITION                  
         LA    R1,SPHOOK           RESET HOOK ADDRESS IN SPOOL BLOCK            
         ST    R1,HEADHOOK                                                      
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         MVI   BOXREQ,C'C'         CLOSE LAST BOX                               
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
         MVC   BOXCOLS,SPACES      CLEAR BOX DESCRIPTION                        
         MVC   BOXCOLSR,SPACES                                                  
         MVC   BOXROWS,SPACES      AND SKIP A LINE OR BREAK PAGE                
         MVI   BOXREQ,0                                                         
         TM    GLABCTXT,X'08'      TEST TO BREAK A PAGE                         
         BZ    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
         L     R1,GLABCBPL         R1=A(BUILDER PARM LIST)                      
         USING BUPARMD,R1                                                       
         L     R5,BUPANODB         R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         LA    R5,NDLVTAB-NODBLKD(R5) R5=A(LEVEL TABLE)                         
         LA    R5,NDLVTABL(R5)     R5=A(CLIENT LEVEL)                           
         USING NDLVTABD,R5                                                      
         LA    R3,3                R3=COUNTER                                   
         LA    R6,TEXTBLOK         R6=A(TEXT BLOCK)                             
         USING TXTIOD,R6                                                        
*                                                                               
ENDTEXT4 GOTO1 INBLK,DMCB,NDLVNOD                                               
         GOTO1 =V(TEXTIO),DMCB,(R6)                                             
         LA    R5,NDLVTABL(R5)                                                  
         BCT   R3,ENDTEXT4                                                      
*                                                                               
ENDTEXTX B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO INITIALIZE TEXT BLOCK FOR CLIENT/PRODUCT/PLAN TEXT             
*     CALLED FROM ENDTEXT                                                       
* AT ENTRY, P1=A(NODE/CODE) AND R6=A(TEXTIO BLOCK)                              
*                                                                               
INBLK    ST    RE,FULL                                                          
         L     RF,0(R1)            RF=A(NODE/CODE)                              
         XC    TEXTBLOK,TEXTBLOK                                                
         MVC   TXTACOM,GLCOMFAC                                                 
         LA    RE,OUTAREA                                                       
         ST    RE,TXTAAREA                                                      
         MVC   TXTAGY,GLAGENCY                                                  
         MVC   TXTNODE,0(RF)                                                    
         MVC   TXTCODE,4(RF)                                                    
         MVC   TXTFILT,GLABCTFC                                                 
         MVI   TXTMAX,MAXENDS                                                   
         MVI   TXTWIDE,LENENDS                                                  
         LA    RE,ENDTXHK                                                       
         ST    RE,TXTHOOK                                                       
         MVI   FIRSTSW,C'Y'        SET SWITCH FOR NEW LEVEL                     
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* HOOK ROUTINE FOR END OF REPORT HIGH LEVEL TEXT                                
*                                                                               
ENDTXHK  NTR1                                                                   
         CLI   FIRSTSW,C'Y'        TEST FOR FIRST FOR LEVEL                     
         BNE   *+8                                                              
         BAS   RE,SKIPLIN          SKIP A LINE BEFORE TEXT                      
         MVI   FIRSTSW,C'N'                                                     
         ZIC   R0,TXTNUM                                                        
         L     R2,TXTAAREA                                                      
         L     R4,ENDDISP                                                       
*                                                                               
ENDTXHK2 L     R1,GLAP1            R1=A(PRINT LINE)                             
         AR    R1,R4               POINT TO PRINT POSITION                      
         MVC   0(LENENDS,R1),0(R2) SLOT IN LINE OF TEXT                         
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         LA    R2,LENENDS(R2)                                                   
         BCT   R0,ENDTXHK2                                                      
*                                                                               
ENDTXHKX B     XIT                                                              
         DROP  R2,R3,R5                                                         
         SPACE 2                                                                
* SUBSTITUTE HOOK ROUTINE FOR SPOOL WHICH CALLS APPLICATION                     
* HOOK ROUTINE                                                                  
*                                                                               
SPHOOK   NTR1                                                                   
         L     RF,GLAHOOK          RF=A(APPLICATION HOOK)                       
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)        RESTORE APPLICATION REGISTERS                
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         EJECT                                                                  
*              EXECUTING ROUTINES (TEXT REFERENCES)                             
         SPACE 3                                                                
REF      MVI   0(R2),C' '                                                       
         MVC   1(3,R2),0(R2)       CLEAR FOUR SPACES                            
         CLI   GLABCMOD,3          TEST FOR TOTALS                              
         BE    XIT                 YES-EXIT                                     
*                                                                               
         LA    R4,GLABCTRC         R4=A(TEXT REFERENCE CODES)                   
         LA    R5,L'GLABCTRC/2     R5=LOOP COUNTER                              
*                                                                               
REF2     OC    0(2,R4),0(R4)       TEST FOR EOL                                 
         BZ    REFX                YES                                          
*                                                                               
         XC    TEXTBLOK,TEXTBLOK                                                
         LA    R6,TEXTBLOK                                                      
         USING TXTIOD,R6                                                        
         MVC   TXTACOM,GLCOMFAC                                                 
         LA    RE,OUTAREA                                                       
         ST    RE,TXTAAREA                                                      
         MVC   TXTAGY,GLAGENCY                                                  
         L     R1,GLABCATO         R1=A(OUTLINE TABLE ENTRY)                    
         USING OUTTABD,R1                                                       
         MVC   TXTNODE,OUTNODE                                                  
         MVC   TXTCODE,OUTCODE                                                  
         MVC   TXTFILT,0(R4)                                                    
         MVI   TXTMAX,1                                                         
         MVI   TXTOPT,1            ONLY NEED ONE RECORD NOW                     
         GOTO1 =V(TEXTIO),DMCB,(R6)                                             
         CLI   TXTNUM,0            TEST IF ANYTHING FOUND                       
         BNE   REF4                YES                                          
*                                                                               
         LA    R4,2(R4)            POINT TO NEXT TEXT CODE                      
         BCT   R5,REF2                                                          
         B     REFX                DID NOT FIND ANYTHING                        
*                                                                               
REF4     LH    R5,GLABCREF         GET CURRENT REFERENCE NUMBER                 
         LA    R5,1(R5)            INCREMENT IT                                 
         STH   R5,GLABCREF                                                      
         CLI   GLLABEL+6,C'A'      TEST FOR OPTION TO PRINT W/O BOXES           
         BNE   *+8                                                              
         OI    GLABCTXT,X'04'      YES-NOTE IT IN GLOBALS                       
         TM    GLABCTXT,X'20'      TEST FOR REFERENCE NUMBERS                   
         BNO   REF6                NO                                           
         EDIT  (R5),(4,(R2)),ALIGN=LEFT                                         
         B     REFX                                                             
*                                                                               
REF6     TM    GLABCTXT,X'10'      TEST FOR REFERENCE LETTERS                   
         BZ    REFX                NO-SOMETHING WRONG SO GO                     
*                                                                               
         SR    R4,R4                                                            
         D     R4,=F'26'                                                        
         LTR   R4,R4               TEST FOR REMAINDER                           
         BZ    *+12                NO                                           
         LA    R5,1(R5)            INCREMENT QUOTIENT TO USE AS COUNTER         
         B     *+8                                                              
         LA    R4,26               SET REMAINDER TO 26 TO FORCE 'Z'             
*                                                                               
         LA    R4,LETTERS-1(R4)    INDEX TO LETTER                              
         MVC   0(1,R2),0(R4)       MOVE OUT REFERENCE                           
         LA    R2,1(R2)                                                         
         BCT   R5,*-10                                                          
*                                                                               
REFX     B     XIT                                                              
         DROP  R1,R6                                                            
         SPACE 2                                                                
LETTERS  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                                    
         EJECT                                                                  
*              EXECUTING ROUTINES (FOOTNOTES)                                   
         SPACE 3                                                                
FOOT     OC    GLABCREF,GLABCREF   TEST FOR ANY REFERENCES                      
         BZ    FOOTX               NONE-EXIT                                    
         MVI   BOXES,C'N'                                                       
         CLI   GLLABEL+7,C'A'      TEST FOR FOOTNOTES WITHOUT BOXES             
         BE    *+8                                                              
         MVI   BOXES,C'Y'                                                       
         LA    R1,SPHOOK           RESET HOOK ADDRESS FOR SPOOL                 
         ST    R1,HEADHOOK                                                      
         L     R3,ABOX             R3=A(BOX BLOCK)                              
         USING BOXD,R3                                                          
         TM    GLABCTXT,X'40'      TEST FOR END TEXT                            
         BO    FOOT2               YES-LAST BOX WAS ALREADY CLOSED              
         MVI   BOXREQ,C'C'                                                      
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
FOOT2    MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   BOXCOLS,SPACES      CLEAR LAST BOX FORMAT                        
         MVC   BOXCOLSR,SPACES                                                  
         MVC   BOXROWS,SPACES                                                   
         MVC   RPTCOLS,SPACES                                                   
         CLI   BOXES,C'N'          TEST FOR PRINT W/O BOXES                     
         BE    FOOT5               YES                                          
         MVI   BOXREQ,0                                                         
         MVI   BOXINIT,0           FORCE RE-INITIALIZE                          
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
*                                                                               
FOOT4    L     R5,GLWPAPER         R5=REPORT WIDTH                              
         LA    RE,LENFOOT          LENGTH OF FOOTNOTE REPORT                    
         ST    RE,LENLINE          SAVE L'LINE                                  
         SR    R5,RE               EXCESS                                       
         SRL   R5,1                DIVIDE IT EVENLY                             
         LA    R1,RPTCOLS-1(R5)                                                 
         MVI   0(R1),C'L'          SET LEFT HAND BOX COLUMN                     
         ST    R5,LEFTDISP         SET DISP TO LEFT MOST COLUMN                 
         LA    RE,6(R5)            ADVANCE PAST REFERENCE AREA                  
         ST    RE,TEXTDISP         DISP TO TEXT DATA                            
         LA    R5,LENFOOT(R5)                                                   
         LA    R1,RPTCOLS(R5)                                                   
         MVI   0(R1),C'R'          SET RIGHT HAND BOX COLUMN                    
         B     FOOT6                                                            
*                                                                               
FOOT5    MVI   BOXYORN,C'N'        TURN OFF BOXES                               
         L     R5,GLWPAPER         R5=REPORT WIDTH                              
         LH    R1,GLABCREF         R1=N'FOOTNOTES                               
         SR    R0,R0               FIND MAX N'DIGITS FOR REFERENCE              
         LA    RF,10                                                            
         TM    GLABCTXT,X'20'      TEST FOR NUMERIC REFERENCES                  
         BO    *+8                 YES                                          
         LA    RF,26                                                            
         DR    R0,RF                                                            
         LTR   R0,R0               TEST FOR REMAINDER                           
         BZ    *+8                                                              
         LA    R1,1(R1)            YES-NEED TO ADD ONE TO QUOTIENT              
         LA    R1,8(R1)            ADD ROOM FOR CONTD.                          
         LA    RE,LENNOTE(R1)      COMPUTE L'LINE                               
         ST    RE,LENLINE          SAVE L'PRINT LINE                            
         SR    R5,RE                                                            
         SRL   R5,1                SPLIT EXCESS EVENLY                          
         ST    R5,LEFTDISP         DISP TO LEFTMOST PRINT POSITION              
         LA    R5,0(R1,R5)         ADD IN REFERENCE/CONTD.                      
         ST    R5,TEXTDISP                                                      
*                                                                               
FOOT6    L     R2,GLAP1                                                         
         A     R2,LEFTDISP         PRINT FOOTNOTE TITLE                         
         MVC   0(10,R2),=C'FOOTNOTES.'                                          
         NC    1(8,R2),LOWERMSK                                                 
         L     R0,LENLINE                                                       
         GOTO1 CENTER,DMCB,(R2),(R0)                                            
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         GOTO1 (RF),(R1),ASPOOLD                                                
*                                                                               
FOOT8    L     R1,GLABCBPL         R1=A(BUILDER PARMS)                          
         USING BUPARMD,R1                                                       
         L     R5,BUPAOUTT         R5=A(OUTLINE TABLE)                          
         USING OUTTABD,R5                                                       
*                                                                               
FOOT10   OC    OUTSEQN,OUTSEQN     TEST FOR EOT                                 
         BZ    FOOTX               YES                                          
         CLI   OUTLVLN,0           TEST FOR PLAN TOTAL RECORD                   
         BE    FOOT12              YES                                          
         TM    OUTIND1,X'40'       TEST FOR SUPPRESSED OUTLINE                  
         BO    FOOT12                                                           
*                                                                               
         BAS   RE,NOTES            LOOK FOR AND PRINT FOOTNOTES                 
*                                                                               
FOOT12   LA    R5,OUTTABL(R5)                                                   
         B     FOOT10                                                           
*                                                                               
FOOTX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO LOOK FOR AND TO PRINT OUTLINE FOOTNOTES                        
*                                                                               
* AT ENTRY, R3=A(BOX BLOCK), R5=A(OUTLINE TABLE ENTRY)                          
*                                                                               
NOTES    NTR1                                                                   
         MVI   FIRSTSW,C'Y'                                                     
         LA    R4,GLABCTRC         R4=A(TEXT REFERENCE CODE)                    
         LA    R0,L'GLABCTRC/2     R0=LOOP COUNTER                              
*                                                                               
NOTES2   OC    0(2,R4),0(R4)       TEST FOR EOL                                 
         BZ    NOTESX              YES                                          
*                                                                               
         XC    TEXTBLOK,TEXTBLOK                                                
         LA    R6,TEXTBLOK                                                      
         USING TXTIOD,R6                                                        
         MVC   TXTACOM,GLCOMFAC                                                 
         LA    RE,OUTAREA                                                       
         ST    RE,TXTAAREA                                                      
         MVC   TXTAGY,GLAGENCY                                                  
         MVC   TXTNODE,OUTNODE                                                  
         MVC   TXTCODE,OUTCODE                                                  
         MVC   TXTFILT,0(R4)                                                    
         MVI   TXTMAX,MAXNOTE                                                   
         MVI   TXTWIDE,LENNOTE                                                  
         MVI   TXTOPT,1                                                         
         LA    RE,NOBOX                                                         
         CLI   BOXES,C'N'                                                       
         BE    *+8                                                              
         LA    RE,NOTEHK                                                        
         ST    RE,TXTHOOK          SET HOOK ROUTINE                             
         GOTO1 =V(TEXTIO),DMCB,(R6)                                             
*                                                                               
         LA    R4,2(R4)            NEXT TEXT REFERENCE CODE                     
         BCT   R0,NOTES2                                                        
*                                                                               
NOTESX   B     XIT                                                              
         EJECT                                                                  
* HOOK ROUTINE FOR FOOTNOTE PRINTING                                            
*    CALLED BY TEXTIO WHICH IS INVOKED BY NOTES SUB-ROUTINE                     
*                                                                               
NOTEHK   NTR1                                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLSR,SPACES                                                  
         MVC   BOXROWS,SPACES                                                   
         BAS   RE,SKIPLIN          SKIP A LINE                                  
         MVI   STARTPG,C'N'        INITIALIZE PAGE START SWITCH                 
*                                                                               
NOTEHK2  MVC   BOXCOLS,RPTCOLS     SET REPORT BOX COLUMNS                       
         MVI   BOXINIT,0                                                        
         ZIC   R1,LINE             GET NEXT LINE NUMBER                         
         LA    R1,4(R1)            ADD IN BOX LINES AND HEADLINE                
         ZIC   R0,TXTNUM           GET N'TEXT LINES                             
         AR    R1,R0                                                            
         CLM   R1,1,MAXLINES       TEST FOR FIT ON PAGE                         
         BL    NOTEHK4             YES                                          
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVI   STARTPG,C'Y'        MARK STARTING NEW PAGE FOR BOX               
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
NOTEHK4  ZIC   R1,LINE             GET NEXT LINE POSITION                       
         BCTR  R1,0                                                             
         LA    RE,BOXROWS(R1)      INDEX INTO ROWS FOR TOP LINE                 
         MVI   0(RE),C'T'                                                       
         LA    RE,2(RE)            BUMP AHEAD FOR MIDDLE LINE                   
         MVI   0(RE),C'M'                                                       
         AR    RE,R0               FIND END OF TEXT                             
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'B'                                                       
*                                                                               
NOTEHK6  GOTO1 SPOOL,DMCB,ASPOOLD  DRAW TOP OF BOX                              
         CLI   FIRSTSW,C'Y'        TEST FOR FIRST FOR OUTLINE                   
         BNE   NOTEHK10                                                         
*                                                                               
         LH    R1,TEXTREF          GET LAST TEXT REFERENCE                      
         LA    R1,1(R1)            INCREMENT IT                                 
         STH   R1,TEXTREF                                                       
         L     R2,GLAP1                                                         
         A     R2,LEFTDISP         R2=A(REFERENCE POSITION)                     
         TM    GLABCTXT,X'20'      TEST FOR NUMERIC REFERENCE                   
         BNO   NOTEHK8             NO                                           
         LR    R0,R1                                                            
         EDIT  (R0),(4,(R2)),ALIGN=LEFT,TRAIL=C'.'                              
         B     NOTEHK10                                                         
*                                                                               
NOTEHK8  SR    R0,R0                                                            
         D     R0,=F'26'                                                        
         LTR   R0,R0                                                            
         BZ    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *+8                                                              
         LA    R0,26               FORCE 'Z' TO PRINT                           
*                                                                               
         LA    RE,LETTERS-1                                                     
         AR    RE,R0                                                            
         MVC   0(1,R2),0(RE)       PRINT THE LETTER                             
         LA    R2,1(R2)            NEXT PRINT POSITION                          
         BCT   R1,*-10                                                          
         MVI   0(R2),C'.'                                                       
*                                                                               
NOTEHK10 L     R2,GLAP1                                                         
         A     R2,TEXTDISP         R2=A(OUTLINE NAME POSITION)                  
         MVC   0(L'OUTNAME,R2),OUTNAME                                          
         CLI   FIRSTSW,C'N'        TEST PAST FIRST FOR OUTLINE                  
         BE    *+12                YES                                          
         MVI   FIRSTSW,C'N'        NO-TURN OFF FIRST TIME SWITCH                
         B     NOTEHK12                                                         
*                                                                               
         CLI   STARTPG,C'Y'        TEST STARTING NEW PAGE FOR BOX               
         BNE   NOTEHK12            NO                                           
         LA    R2,L'OUTNAME-1(R2)                                               
         LA    RE,L'OUTNAME        FIND LAST SIGNIFICANT CHARACTER              
         CLI   0(R2),C' '                                                       
         BH    *+10                                                             
         BCTR  R2,0                                                             
         BCT   RE,*-10                                                          
*                                                                               
         LA    R2,3(R2)                                                         
         MVC   0(6,R2),=C'CONTD.'                                               
         NC    1(4,R2),LOWERMSK                                                 
*                                                                               
NOTEHK12 GOTO1 SPOOL,DMCB,ASPOOLD                                               
         GOTO1 (RF),(R1),ASPOOLD   MIDDLE BOX LINE                              
*                                                                               
         ZIC   R0,TXTNUM           R0=LOOP COUNTER                              
         L     R2,TXTAAREA                                                      
*                                                                               
NOTEHK14 L     R1,GLAP1                                                         
         A     R1,TEXTDISP         R1=A(TEXT POSITION)                          
         MVC   0(LENNOTE,R1),0(R2)                                              
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         LA    R2,LENNOTE(R2)                                                   
         BCT   R0,NOTEHK14                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  BOTTOM BOX LINE                              
*                                                                               
NOTEHKX  B     XIT                                                              
         EJECT                                                                  
* HOOK ROUTINE FOR FOOTNOTE PRINTING WITHOUT BOXES                              
*    CALLED BY TEXTIO WHICH IS INVOKED BY NOTES SUB-ROUTINE                     
*                                                                               
NOBOX    NTR1                                                                   
         BAS   RE,SKIPLIN          SKIP A LINE                                  
         MVI   STARTPG,C'N'        INITIALIZE PAGE START SWITCH                 
         ZIC   R1,LINE             GET NEXT LINE NUMBER                         
         ZIC   R0,TXTNUM           GET N'TEXT LINES                             
         AR    R1,R0                                                            
         CLM   R1,1,MAXLINES       TEST FOR FIT ON PAGE                         
         BL    NOBOX2              YES                                          
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVI   STARTPG,C'Y'        MARK STARTING NEW PAGE FOR BOX               
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
NOBOX2   CLI   FIRSTSW,C'Y'        TEST FIRST TIME FOR OUTLINE                  
         BE    *+12                YES                                          
         CLI   STARTPG,C'Y'        TEST STARTING NEW PAGE                       
         BNE   NOBOX6                                                           
*                                                                               
         LH    R1,TEXTREF          GET LAST TEXT REFERENCE                      
         LA    R1,1(R1)            INCREMENT IT                                 
         STH   R1,TEXTREF                                                       
         L     R2,GLAP1                                                         
         A     R2,LEFTDISP         R2=A(REFERENCE POSITION)                     
         TM    GLABCTXT,X'20'      TEST FOR NUMERIC REFERENCE                   
         BNO   NOBOX4              NO                                           
         LR    R0,R1                                                            
         EDIT  (R0),(4,(R2)),ALIGN=LEFT,TRAIL=C'.'                              
         AR    R2,R0               UPDATE POINTER                               
         B     NOBOX6                                                           
*                                                                               
NOBOX4   SR    R0,R0                                                            
         D     R0,=F'26'                                                        
         LTR   R0,R0                                                            
         BZ    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *+8                                                              
         LA    R0,26               FORCE 'Z' TO PRINT                           
*                                                                               
         LA    RE,LETTERS-1                                                     
         AR    RE,R0                                                            
         MVC   0(1,R2),0(RE)       PRINT THE LETTER                             
         LA    R2,1(R2)            NEXT PRINT POSITION                          
         BCT   R1,*-10                                                          
         MVI   0(R2),C'.'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
NOBOX6   CLI   FIRSTSW,C'N'        TEST PAST FIRST FOR OUTLINE                  
         BE    *+12                YES                                          
         MVI   FIRSTSW,C'N'        NO-TURN OFF FIRST TIME SWITCH                
         B     NOBOX8                                                           
*                                                                               
         CLI   STARTPG,C'Y'        TEST STARTING NEW PAGE FOR BOX               
         BNE   NOBOX8              NO                                           
*                                                                               
         BCTR  R2,0                BACK UP TO PERIOD                            
         MVI   0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
         MVC   0(6,R2),=C'CONTD.'                                               
         NC    1(4,R2),LOWERMSK                                                 
*                                                                               
NOBOX8   ZIC   R0,TXTNUM           R0=LOOP COUNTER                              
         L     R2,TXTAAREA                                                      
*                                                                               
NOBOX10  L     R1,GLAP1                                                         
         A     R1,TEXTDISP         R1=A(TEXT POSITION)                          
         MVC   0(LENNOTE,R1),0(R2)                                              
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         LA    R2,LENNOTE(R2)                                                   
         BCT   R0,NOBOX10                                                       
*                                                                               
NOBOXX   B     XIT                                                              
         DROP  R1,R5,R6                                                         
         SPACE 2                                                                
SKIPLIN  ST    RE,FULL                                                          
         ZIC   RE,LINE                                                          
         ZIC   R1,SPACING                                                       
         AR    RE,R1                                                            
         CLM   RE,1,MAXLINES                                                    
         BNL   SKIPLINX                                                         
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
SKIPLINX L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*              EXECUTING ROUTINES (DATATYPE)                                    
         SPACE 3                                                                
DATATYPE CLI   GLABCMOD,1          DATA TYPE                                    
         BE    XIT                 DON'T SHOW IF FOR OUTLINE HEADING            
         L     R4,GLABCATO                                                      
         USING OUTTABD,R4                                                       
         LH    R5,OUTSEQN          PICK UP SEQUENCE NUMBER                      
         BCTR  R5,0                -1                                           
         L     R1,GLABCBPL                                                      
         USING BUPARMD,R1                                                       
         L     RE,BUPAACCD                                                      
         USING ACCDEFD,RE                                                       
         MH    R5,ACCAWDTH         X WIDTH OF ROWS                              
         A     R5,BUPAACCS         + START OF ACCUMULATORS                      
         L     RE,BUPADTAB         NOW LOOK IN DATA TYPE TABLE                  
         USING DTATABD,RE                                                       
         SPACE 1                                                                
DT2      CLC   0(8,RE),GLLABEL     FOR MATCH ON DATA TYPE NAME                  
         BNE   DTEND                                                            
         CLC   GLARGS(4),DTAPERD   AND MATCH ON PERIOD                          
         BNE   DTEND                                                            
         CLC   GLARGS+4(3),DTAASAT AND MATCH ON AS AT                           
         BNE   DTEND                                                            
         SPACE 1                                                                
DTHIT    MVC   0(8,R2),0(R5)       NOW MOVE OUT SCALED ACCUM                    
         B     XIT                                                              
         SPACE 1                                                                
DTEND    LA    R5,8(R5)            (BUMP TO NEXT ACCUMULATOR)                   
         LA    RE,DTATABL(RE)                                                   
         CLI   0(RE),0                                                          
         BNE   DT2                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         DROP  R1,R4,RE                                                         
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
LOWERMSK DC    16X'BF'                                                          
         SPACE 3                                                                
*              LTORG ETC                                                        
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
*              WORKING STORAGE FOR MODULE                                       
         SPACE 3                                                                
BUWORK   DSECT                                                                  
         DS    D                                                                
TEXTBLOK DS    CL(TXTLBLOK)                                                     
*                                                                               
ENDDISP  DS    F                                                                
LEFTDISP DS    F                                                                
TEXTDISP DS    F                                                                
LENLINE  DS    F                   LENGTH OF TEXT LINE                          
TEXTREF  DS    H                   CURRENT TEXT REFERENCE NUMBER                
FIRSTSW  DS    C                                                                
STARTPG  DS    C                   Y=STARTING NEW PAGE FOR TEXT                 
BOXES    DS    C                   Y/N=PRINT FOOTNOTES IN BOX FORMAT            
RPTCOLS  DS    CL(L'BOXCOLS)                                                    
*                                                                               
OUTAREA  DS    (MAXENDS)CL(LENENDS)                                             
BUWORKX  EQU   *                   END OF W/S                                   
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXENDS  EQU   12                  MAXIMUM NUMBER OF END TEXT LINES             
LENENDS  EQU   60                  LENGTH OF END TEXT LINES                     
MAXNOTE  EQU   MAXENDS                                                          
LENNOTE  EQU   LENENDS                                                          
LENFOOT  EQU   LENNOTE+6+6+2       LENGTH OF FOOTNOTE REPORT                    
         EJECT                                                                  
       ++INCLUDE BUILDERD                                                       
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
       ++INCLUDE DRINTRECD2                                                     
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE BUTEXTIOD                                                      
         EJECT                                                                  
       ++INCLUDE DDNODBLKD                                                      
         EJECT                                                                  
* BUGENFILE                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE BUGENFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017BUSYSDRIVE05/01/02'                                      
         END                                                                    
