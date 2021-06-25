*          DATA SET ACPRO46    AT LEVEL 021 AS OF 02/24/15                      
*PHASE T60B46A                                                                  
*INCLUDE ACDISCOL                                                               
*INCLUDE DLFLD                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B46 - JOB ESTIMATE - DISPLAY/PRINT/DOWNLOAD MODULE'          
T60B46   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B46**,R7,RR=R2                                              
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         L     RE,=V(ACDISCOL)                                                  
         A     RE,RELO                                                          
         ST    RE,VDISCOL                                                       
         SPACE 1                                                                
EST2     CLI   INTMODE,DOWNREP     TEST FOR REPORT DOWNLOAD                     
         BE    EST4                YES                                          
         CLI   INTMODE,PRTREP      TEST FOR REPORT PRINT                        
         BE    EST6                                                             
         CLI   INTMODE,SOONREP     TEST FOR SOON REQUEST                        
         BE    EST8                                                             
         CLI   INTMODE,OVERREP     TEST FOR OVERNIGHT REQUEST                   
         BE    EST8                                                             
         SPACE 1                                                                
         L     R4,4(R1)            GET START ENTRY NUMBER                       
         BAS   RE,DISP                                                          
         B     ESTX                                                             
         SPACE 1                                                                
EST4     L     RE,=V(DLFLD)                                                     
         A     RE,RELO                                                          
         ST    RE,VDLFLD                                                        
         BAS   RE,DOWN                                                          
         B     ESTX                                                             
         SPACE 1                                                                
EST6     BAS   RE,PRINT                                                         
         B     ESTX                                                             
         SPACE 1                                                                
EST8     OC    REPNAME,REPNAME     DO WE HAVE A REPORT NAME ?                   
         BZ    *+12                NO, BUILD REPORT FROM SCREEN                 
         BAS   RE,GETREP           YES, USE THAT ONE                            
         B     *+8                                                              
         BAS   RE,GETREQ           GET ESTIMATE REPORT SCREEN                   
         BAS   RE,BLDREQ           BUILD REQUEST SCREEN                         
         BAS   RE,GENREQ           GENERATE THE REQUEST                         
         SPACE 1                                                                
ESTX     XMOD1 1                                                                
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO DISPLAY THE DATA LINES ON THE SCREEN          *                
*     AT ENTRY, R4=N'START TABLE ENTRY                         *                
*     ON EXIT, CC=EQ AND SVNWCS SET IF SOMETHING TO DISPLAY    *                
*     AND CC=NEQ IF NOTHING TO DISPLAY                         *                
****************************************************************                
         SPACE 1                                                                
DISP     NTR1  ,                                                                
         BAS   RE,DISNOTE          DISPLAY THE NOTE FIELD                       
         MVI   SVNWCS,0            CLEAR COUNT OF SAVED ITEMS TO ZERO           
         MVI   SVLOW,0             CLEAR LOWEST AND HIGHEST ENTRY NUM           
         MVI   SVHI,0                                                           
         LR    RF,R4               RF=START ENTRY NUMBER                        
         L     R5,AESTTAB                                                       
         USING ESTTABD,R5                                                       
         BCTR  RF,0                                                             
         M     RE,LESTTAB                                                       
         LA    R5,0(RF,R5)                                                      
         L     R3,NESTENT                                                       
         SR    R3,R4               R3=N'WORKCODE ENTRIES LEFT IN TABLE          
         LA    R3,1(R3)                                                         
         L     R2,AFSTSEL                                                       
*                                                                               
DISP2    BAS   RE,FILCAT           FILTER ON CATEGORY                           
         BNE   DISP6               REJECT ITEM FOR DISPLAY                      
         BAS   RE,FILZERO          APPLY ZERO OPTION                            
         BNE   DISP6                                                            
         BAS   RE,FILWC            APPLY WORKCODE SELECT FILTER                 
         BNE   DISP6                                                            
         BAS   RE,FILSW            APPLY WORKCODE START FILTER                  
         BNE   DISP6                                                            
*                                                                               
DISP4    BAS   RE,SETLIN           SET FIELD POINTERS                           
         STC   R4,SVHI                                                          
         CLI   SVLOW,0             TEST FIRST ENTRY NOTED                       
         BNE   *+8                 YES                                          
         STC   R4,SVLOW                                                         
         BAS   RE,DISWC            DISPLAY WORKCODE CODE/DESCRIPTION            
         BAS   RE,DISDATA          DISPLAY THE DATA FIELDS                      
         L     R2,ANEXTSEL         POINT TO NEXT SCREEN LINE                    
         CLI   SVNWCS,MAXDATA      TEST FOR FILLED SCREEN                       
         BE    DISP8               YES-ALL DONE                                 
*                                                                               
DISP6    A     R5,LESTTAB                                                       
         LA    R4,1(R4)            INCREMENT ENTRY NUMBER                       
         BCT   R3,DISP2                                                         
*                                                                               
* DISPLAY THE COLUMN NAMES AND ESTIMATE DETAIL COLUMN BY COLUMN                 
*                                                                               
         USING COLD,R4                                                          
DISP8    ZIC   R3,NCOLS            R3=LOOP COUNTER                              
         LA    R4,COLINDS          R4=A(COLUMN INDICATORS)                      
         L     R5,ACOLIST          R5=A(JOBCOL COLUMN ENTRIES)                  
*                                                                               
         MVC   ATHISNAM,AFSTNAME   INITIALIZE NAME FIELD POINTER                
         MVC   ATHISNM2,ASECNAME   INITIALIZE SECOND NAME FIELD POINTER         
*                                                                               
         L     R2,AFSTSEL                                                       
         BAS   RE,SETLIN                                                        
         L     R2,ADATA1                                                        
         ST    R2,ATHISCOL                                                      
*                                                                               
DISP10   TM    COLFLAG,COLFEST     IS THIS AN ESTIMATE ?                        
         BZ    DISP11              NO                                           
*                                                                               
         GOTO1 AGETEST,DMCB,(RC),COLEST                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SVNWCS,0            TEST FOR WORKCODE DETAIL                     
         BE    DISP12              NO                                           
         CLI   COLATTB,COLAPROT    TEST FOR PROTECTED ESTIMATE COLUMN           
         BE    DISP11              YES-NAE=Y AND APPROVED EST IS                
         L     R2,ATHISCOL                                                      
         BAS   RE,GETDET           GET DETAILED COMM/NONCOMM                    
*                                                                               
DISP11   BAS   RE,DISNAME          DISPLAY THE COLUMN NAME                      
*                                                                               
DISP12   L     R2,ATHISNAM                                                      
         BAS   RE,BUMP             ADVANCE NAME POINTER                         
         ST    R2,ATHISNAM                                                      
         L     R2,ATHISNM2         ADVANCE SECOND NAME POINTER                  
         BAS   RE,BUMP                                                          
         ST    R2,ATHISNM2                                                      
         L     R2,ATHISCOL                                                      
         BAS   RE,BUMP             ADVANCE TO NEXT DATA FIELD                   
         ST    R2,ATHISCOL                                                      
         LA    R4,COLLENQ(R4)                                                   
         LA    R5,JBCLENQ(R5)                                                   
         BCT   R3,DISP10                                                        
*                                                                               
DISPX    CLI   SVNWCS,0            TEST FOR ANY WORKCODE LINES                  
         BE    NOXIT               SET CC ON EXIT                               
         B     YESXIT                                                           
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO DISPLAY THE NOTE FIELD--CALLED FROM DISP--    *                
* ROUTINE SHOWS SCHEME AND MARKS ORIGINAL AND CURRENT ESTIMATE *                
****************************************************************                
         SPACE 1                                                                
         PUSH  USING                                                            
DISNOTE  NTR1  ,                                                                
         MVC   PRONOTE,SPACES                                                   
         MVC   PRONOTE(7),=C'SCHEME='                                           
         MVC   PRONOTE+7(L'JOBSCH),JOBSCH DISPLAY SCHEME                        
         MVC   PRONOTE+16(4),=C'NAE='                                           
         MVC   PRONOTE+20(1),GONEEDAE SHOW NEED APPROVED EST VALUE              
         GOTO1 SQUASHER,DMCB,PRONOTE,21                                         
*                                                                               
         USING COLD,R4                                                          
         LA    R4,COLINDS          R4=A(COLUMN INDICATORS)                      
         L     R5,ACOLIST          R5=A(COLUMN LIST)                            
         USING JBCLD,R5                                                         
         ZIC   R6,NCOLS            R0=LOOP COUNTER                              
         LA    R3,PRONOTE+24                                                    
*                                                                               
DISNOT02 CLI   JBCLTYP,JBCLCOL     TEST FOR SINGLE COLUMN                       
         BNE   DISNOT12            NO-SKIP FORMULA                              
         CLC   JBCLCN1,REGEST      TEST REGULAR ESTIMATE REQUESTED              
         BE    DISNOT08            YES                                          
         LA    R1,ORGTYPE                                                       
         CLC   JBCLCN1,ORIGEST     TEST ORIGINAL ESTIMATE REQUESTED             
         BE    DISNOT04            YES                                          
         LA    R1,CURTYPE                                                       
         CLC   JBCLCN1,CURREST     TEST CURRENT ESTIMATE REQUESTED              
         BE    DISNOT04                                                         
         CLC   JBCLCN1,PREVEST     PREVIOUS ESTIMATE?                           
         BNE   DISNOT12            NO                                           
         MVC   0(1,R3),0(R1)                                                    
         ZIC   R0,1(R1)                                                         
         BCTR  R0,0                                                             
         B     DISNOT06                                                         
*                                                                               
DISNOT04 MVC   0(1,R3),0(R1)                                                    
         ZIC   R0,1(R1)                                                         
*                                                                               
DISNOT06 CURED (R0),(2,1(R3)),0,ALIGN=LEFT                                      
         B     DISNOT12                                                         
*                                                                               
DISNOT08 CLC   COLEST,CURTYPE     TEST IF CURRENT ESTIMATE                      
         BE    DISNOT10            YES                                          
         CLC   COLEST,ORGTYPE                                                   
         BNE   DISNOT12                                                         
         MVC   0(2,R3),=C'OE'                                                   
         B     DISNOT12                                                         
*                                                                               
DISNOT10 MVC   0(2,R3),=C'CE'                                                   
*                                                                               
DISNOT12 LA    R4,COLLENQ(R4)    NEXT COLUMN                                    
         LA    R5,JBCLENQ(R5)                                                   
         LA    R3,15(R3)           NEXT OUTPUT POSITION                         
         BCT   R6,DISNOT02                                                      
*                                                                               
DISNOTX  B     XIT                                                              
         DROP  R4                                                               
         POP   USING                                                            
         EJECT                                                                  
* SUB-ROUTINE TO FILTER AGAINST CATEGORY LIST                                   
* AT ENTRY, R5=A(ESTIMATE TABLE ENTRY)                                          
* ON EXIT, CC=EQ IF OK ,CC=NEQ TO REJECT CATEGORY                               
*                                                                               
FILCAT   CLI   NCATS,0                                                          
         BER   RE                                                               
         ZIC   R0,NCATS                                                         
         LA    R1,CATOPT                                                        
         TM    CATOPT,X'40'        TEST POSITIVE FILTER                         
         BZ    FILCAT2             NO                                           
*                                                                               
FILCAT1  CLC   ESTCAT,0(R1)                                                     
         BE    FILCATY                                                          
         LA    R1,L'ESTCAT(R1)                                                  
         BCT   R0,FILCAT1                                                       
         B     FILCATN                                                          
*                                                                               
FILCAT2  MVC   HALF,0(R1)                                                       
         OI    HALF,X'40'                                                       
         CLC   ESTCAT,HALF                                                      
         BE    FILCATN                                                          
         LA    R1,L'ESTCAT(R1)                                                  
         BCT   R0,FILCAT2                                                       
*                                                                               
FILCATY  CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
FILCATN  LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER AGAINST WORKCODE LIST                                   
* AT ENTRY, R5=A(ESTIMATE TABLE ENTRY)                                          
* ON EXIT, CC=EQ IF OK ,CC=NEQ TO REJECT WORKCODE                               
*                                                                               
FILWC    CLI   NWORK,0                                                          
         BER   RE                                                               
         ZIC   R0,NWORK                                                         
         LA    R1,WCOPT                                                         
         TM    WCOPT,X'40'         TEST POSITIVE FILTER                         
         BZ    FILWC2              NO                                           
*                                                                               
FILWC1   CLC   ESTWORKC,0(R1)                                                   
         BE    FILWCY                                                           
         LA    R1,L'ESTWORKC(R1)                                                
         BCT   R0,FILWC1                                                        
         B     FILWCN                                                           
*                                                                               
FILWC2   MVC   HALF,0(R1)                                                       
         OI    HALF,X'40'                                                       
         CLC   ESTWORKC,HALF                                                    
         BE    FILWCN                                                           
         LA    R1,L'ESTWORKC(R1)                                                
         BCT   R0,FILWC2                                                        
*                                                                               
FILWCY   CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
FILWCN   LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER AGAINST WORKCODE START                                  
* AT ENTRY, R5=A(ESTIMATE TABLE ENTRY)                                          
* ON EXIT, CC=EQ IF OK ,CC=NEQ TO REJECT WORKCODE                               
* SET SVSWSTRT TO STARTING WORKCODE POSITION                                    
*                                                                               
FILSW    CLI   SWOPT,0                                                          
         BER   RE                                                               
         CLI   SVSWSTRT,X'00'      DID WE FIND START ALREADY ?                  
         BNE   FILSWY              YES, OK                                      
         CLC   ESTWORKC,SWOPT                                                   
         BNE   FILSWN                                                           
         L     R1,NESTENT                                                       
         SR    R1,R3                                                            
         LA    R1,1(R1)                                                         
         STC   R1,SVSWSTRT         SAVE STARTING POINT                          
*                                                                               
FILSWY   CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
FILSWN   LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO APPLY ZERO OPTION FILTER                                       
* ON ENTRY, R5=A(ESTIMATE TABLE ENTRY)                                          
* ON EXIT, CC=EQ TO ACCEPT ENTRY, CC=NEQ TO REJECT ENTRY                        
*                                                                               
FILZERO  CLI   ZEROPT,C'S'         TEST FOR ZERO SUPPRESSION                    
         BE    *+12                YES                                          
         CLI   ZEROPT,C'A'         TEST SPECIAL ZERO SUPPRESS OPTION            
         BNE   FILZEROY            NO-TAKE LINE                                 
*                                                                               
         USING COLD,RF                                                          
         ZIC   R0,NCOLS                                                         
         LA    R1,ESTVALS          R1=A(WORKCODE VALUES)                        
         LA    RF,COLINDS          RF=A(COLUMN INDICATORS)                      
FILZERO2 TM    COLFLAG,COLFCOM     TEST FOR COMMISSION RATE COLUMN              
         BO    *+14                YES-SKIP ZERO TEST                           
         CP    0(L'ESTVALS,R1),=P'0'                                            
         BNE   FILZEROY            FOUND A NON-ZERO ENTRY                       
         LA    R1,L'ESTVALS(R1)                                                 
         LA    RF,COLLENQ(RF)                                                   
         BCT   R0,FILZERO2                                                      
*                                                                               
FILZERON LTR   RB,RB               SET CC=NEQ FOR ZERO LINE                     
         BR    RE                                                               
*                                                                               
FILZEROY CR    RB,RB                                                            
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO DISPLAY THE NAME FIELDS--CALLED FROM DISP--     *              
* AT ENTRY, R4=A(COLUMN INDICATOR ENTRY)                         *              
******************************************************************              
         SPACE 1                                                                
DISNAME  NTR1  ,                                                                
         PUSH  USING                                                            
         USING COLD,R4                                                          
         USING JBCLD,R5                                                         
         CLI   JBCLTYP,JBCLFRM                                                  
         BNE   DISNAME1                                                         
         OC    JBCLSNV,JBCLSNV     IS THIS A SUPPLEMENT ?                       
         BNZ   DISNAME2            YES                                          
         BAS   RE,GETFORM          NO, PRINT FORMULA AND EXIT                   
         B     DISNAMEX                                                         
*                                                                               
DISNAME1 CLI   JBCLTYP,JBCLCOL     TEST FOR SINGLE COLUMN                       
         BNE   DISNAMEX            NO-EXIT FOR NOW                              
*                                                                               
DISNAME2 GOTO1 VDISCOL,DMCB,(R5),LISTAR                                         
         L     R2,ATHISNAM                                                      
         OI    4(R2),X'20'                                                      
         BAS   RE,MOVEFLD                                                       
         MVC   LISTAR(14),LISTAR+14 SHIFT OVER SECOND NAME                      
         L     R2,ATHISNM2                                                      
         OI    4(R2),X'20'                                                      
         BAS   RE,MOVEFLD                                                       
         OC    JBCLSNV,JBCLSNV     IS THIS A SUPPLEMENT ?                       
         BZ    DISNAME3            NO                                           
         CLC   JBCLCN1,REGEST      YES, IS IT NET ?                             
         BNE   DISNAMEX            NO                                           
         BAS   RE,GETSUPP          YES, GET COLUMN HEADING                      
         B     DISNAMEX                                                         
*                                                                               
DISNAME3 TM    COLFLAG,COLFEST     TEST COLUMN IS AN ESTIMATE                   
         BZ    DISNAMEX                                                         
*                                                                               
         MVI   ELCODE,ACENELQ      LOOK FOR ANY ESTIMATE NAMES                  
         BAS   RE,GETELIO                                                       
         BNE   DISNAMEX            NO                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
         L     R2,ATHISNAM                                                      
         BAS   RE,MOVEFLD                                                       
         L     R2,ATHISNM2                                                      
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,ATHISNAM                                                      
         MVI   BYTE,1              FIRST NAME FIELD                             
         BAS   RE,GETEN                                                         
*                                                                               
         L     R2,ATHISNM2                                                      
         MVI   BYTE,2                                                           
         BAS   RE,GETEN                                                         
*                                                                               
DISNAMEX B     XIT                                                              
         POP   USING                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE ESTIMATE NAME                                          
*                                                                               
GETEN    ST    RE,FULL                                                          
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACENELQ',AIO),(1,BYTE)                
         CLI   12(R1),0                                                         
         BNE   GETENX                                                           
*                                                                               
         L     R6,12(R1)                                                        
         USING ACEND,R6                                                         
         MVC   LISTAR,SPACES                                                    
         ZIC   R1,ACENLEN                                                       
         SH    R1,=Y(ACENAME-ACEND+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTAR(0),ACENAME                                                
         BAS   RE,MOVEFLD                                                       
*                                                                               
GETENX   L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE NAME FOR THE SUPPLEMENT COLUMN                         
*                                                                               
         USING JBCLD,R5                                                         
GETSUPP  NTR1  ,                                                                
         GOTO1 AGETEST,DMCB,(RC),JBCLCN1E                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,ACENELQ      USE NAME ELEMENT, IF THERE IS ONE            
         BAS   RE,GETELIO                                                       
         BNE   GETSUPP2                                                         
*                                                                               
         L     R2,ATHISNAM                                                      
         MVI   BYTE,1                                                           
         BAS   RE,GETEN                                                         
*                                                                               
         L     R2,ATHISNM2                                                      
         MVI   BYTE,2                                                           
         BAS   RE,GETEN                                                         
         B     GETSUPPX                                                         
*                                                                               
         USING ACEUD,R6                                                         
GETSUPP2 MVI   ELCODE,ACEUELQ      NO NAME, GET UPDATE ELEMENT                  
         BAS   RE,GETELIO                                                       
         BNE   GETSUPPX                                                         
         MVC   LISTAR(L'SUPPNAME),SUPPNAME                                      
         CURED (B1,JBCLSNV+1),(3,LISTAR+L'SUPPNAME+1),0,ALIGN=LEFT              
         L     R2,ATHISNAM                                                      
         BAS   RE,MOVEFLD                                                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
         GOTO1 DATCON,DMCB,(1,ACEUADD),(8,LISTAR)                               
         L     R2,ATHISNM2                                                      
         BAS   RE,MOVEFLD                                                       
*                                                                               
GETSUPPX B     XIT                                                              
         SPACE 2                                                                
*                                                                               
* SUB-ROUTINE TO GET THE FORMULA EXPRESSION FROM THE COLUMN FIELD               
*                                                                               
GETFORM  NTR1  ,                                                                
         LA    R2,PROCOLH                                                       
         GOTO1 ANY                 EXTRACT INPUT                                
         LA    RE,WORK             RE=INPUT POINTER                             
         ZIC   R6,NCOLS                                                         
         SR    R6,R3               COMPUTE N`COMMAS TO LOOK FOR                 
         BZ    GETFORM4                                                         
*                                                                               
GETFORM2 CLI   0(RE),C','          TEST FOR A COMMA                             
         BE    GETFORM3                                                         
         LA    RE,1(RE)                                                         
         B     GETFORM2                                                         
*                                                                               
GETFORM3 LA    RE,1(RE)                                                         
         BCT   R6,GETFORM2                                                      
         B     GETFORM4                                                         
*                                                                               
GETFORM4 LA    R1,LISTAR                                                        
         MVC   LISTAR,SPACES                                                    
*                                                                               
GETFORM5 CLI   0(RE),C' '          TEST IF A SPACE                              
         BE    GETFORM6            YES-AT END OF FIELD                          
         CLI   0(RE),C','          TEST FOR ANOTHER COMMA                       
         BE    GETFORM6            YES-AT END                                   
*                                                                               
         MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         B     GETFORM5                                                         
*                                                                               
GETFORM6 L     R2,ATHISNAM                                                      
         BAS   RE,MOVEFLD                                                       
*                                                                               
GETFORMX B     XIT                                                              
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO DISPLAY THE DATA FIELDS FOR A LINE           *                 
* AT ENTRY, R5=A(ESTIMATE TABLE ENTRY)                        *                 
***************************************************************                 
         SPACE 1                                                                
         USING COLD,R4                                                          
         USING ESTTABD,R5                                                       
         USING ACEND,R6                                                         
DISDATA  NTR1  ,                                                                
         L     R2,ADATA1                                                        
         ZIC   R3,NCOLS                                                         
         LA    R4,COLINDS                                                       
         LA    R6,ESTVALS                                                       
         MVC   AIO1,AIO            SAVE AIO                                     
*                                                                               
DISDATA2 MVC   LISTAR,SPACES                                                    
         TM    COLFLAG,COLFCOM     TEST FOR COMMISSION RATE                     
         BO    DISDATA3                                                         
         CURED (P6,0(R6)),(14,LISTAR),2,ALIGN=LEFT,MINUS=YES                    
         B     DISDATA4                                                         
*                                                                               
DISDATA3 CP    0(L'ESTVALS,R6),=P'0' TEST FOR ZERO RATE                         
         BE    DISDATA4            YES                                          
         CURED (P6,0(R6)),(14,LISTAR),4,ALIGN=LEFT                              
*                                                                               
DISDATA4 BAS   RE,MOVEFLD                                                       
         MVI   1(R2),X'08'         UNPROTECT/HI INTENSITY                       
         CLI   COLATTB,COLAPROT                                                 
         BNE   *+8                                                              
         MVI   1(R2),X'20'         PROTECT                                      
         OI    4(R2),X'20'                                                      
         MVI   ELCODE,ACCWELQ                                                   
         BAS   RE,GETELI2                                                       
         B     *+8                                                              
*                                                                               
DISDATA5 BAS   RE,NEXTEL2                                                       
         BNE   DISDATA6            DID NOT FIND WC CATEGORY ELEMENT             
         USING ACCWD,R1                                                         
         CLI   ACCWTYPE,X'01'      IS THIS A WORKCODE TYPE ?                    
         BNE   DISDATA5            NO, KEEP LOOKING                             
         CLC   ACCWWORK,0(R5)      YES, IS THIS THE RIGHT WORKCODE ?            
         BNE   DISDATA5            NO                                           
         CLI   ACCWCAT,C' '        YES, IS IT SPECIAL ?                         
         BNH   *+12                NO                                           
         OI    1(R2),X'20'         YES, PROTECT IT                              
         NI    1(R2),X'FF'-X'08'                                                
*                                                                               
DISDATA6 BAS   RE,BUMP                                                          
         LA    R6,L'ESTVALS(R6)                                                 
         LA    R4,COLLENQ(R4)                                                   
         BCT   R3,DISDATA2                                                      
*                                                                               
DISDATAX MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R1,R4                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE WORKCODE DETAILS AND RE-DISPLAY ESTIMATE               
* VALUE INPUT AS COMMISSIONABLE AND NON-COMMISSIONABLE AMOUNTS                  
*                                                                               
* AT ENTRY, R4=COLUMN INDICATOR ENTRY, R2=A(FIRST DATA FIELD)                   
*                                                                               
GETDET   NTR1  ,                                                                
         LA    R6,1(R4)            R6=A(TYPE/VERSION)                           
*                                                                               
         ZIC   R0,SVNWCS                                                        
         ST    R0,FULL                                                          
         LA    R3,SVTABLE          R3=A(SAVE TABLE ENTRY FOR WC)                
*                                                                               
GETDET2  CLI   2(R3),0             TEST FOR SUFFIX                              
         BNE   GETDET8             YES-SKIP SEARCH                              
         MVI   ELCODE,EDAELQ                                                    
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
GETDET3  BAS   RE,NEXTEL                                                        
         BNE   GETDET8                                                          
*                                                                               
         USING EDAELD,R6                                                        
         TM    EDATYPE,EDATWORK    IS THIS A WORKCODE?                          
         BZ    GETDET3             NO, READ AGAIN                               
         CLC   EDAWORK,0(R3)       YES, IS IT THE ONE WE WANT?                  
         BNE   GETDET3             NO, READ AGAIN                               
*                                                                               
         CLI   EDALN,EDALNQ2       TEST FOR DOUBLE VALUE ELEM                   
         BL    GETDET8             NO-NO NEED TO GO FURTHER                     
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R4,LISTAR                                                        
*                                                                               
GETDET4  LA    R5,EDACOMM                                                       
         BAS   RE,EDITTWO                                                       
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R5,EDANCOM                                                       
         BAS   RE,EDITTWO                                                       
         AR    R4,R0                                                            
         LA    RE,LISTAR                                                        
         SR    R4,RE               COMPUTE LENGTH                               
         CH    R4,=H'14'           TEST IF IT FITS IN FIELD                     
         BNH   GETDET6             YES                                          
*                                                                               
GETDET5  MVC   LISTAR,SPACES       NO-TRY TO COMPRESS OUTPUT                    
         LA    R4,LISTAR           RESET OUTPUT POINTER                         
         LA    R5,EDACOMM                                                       
         BAS   RE,EDITINT                                                       
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         LA    R5,EDANCOM                                                       
         BAS   RE,EDITINT                                                       
*                                                                               
GETDET6  BAS   RE,MOVEFLD                                                       
*                                                                               
GETDET8  LA    R1,NDATAFLD                                                      
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         LA    R3,L'SVTABLE(R3)                                                 
         L     RE,FULL             DECREMENT ITEM COUNT                         
         SH    RE,=H'1'                                                         
         ST    RE,FULL                                                          
         BP    GETDET2             MORE TO DO                                   
*                                                                               
GETDETX  B     XIT                                                              
         DROP  R6                                                               
         SPACE 2                                                                
EDITTWO  ST    RE,SAVERE                                                        
         CURED (P6,0(R5)),(14,0(R4)),2,ALIGN=LEFT                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
EDITINT  ST    RE,SAVERE                                                        
         ZAP   WORK(16),0(6,R5)    GET AMOUNT AND SEE IF IT                     
         DP    WORK(16),=PL8'100'  CAN BE SHORTENED TO DOLLARS                  
         CP    WORK+8(8),=P'0'     TEST FOR ZERO REMAINDER                      
         BNE   EDITINT2                                                         
         SRP   0(6,R5),64-2,0                                                   
         CURED (P6,0(R5)),(11,0(R4)),0,ALIGN=LEFT                               
         B     EDITINTX                                                         
*                                                                               
EDITINT2 CURED (P6,0(R5)),(14,0(R4)),2,ALIGN=LEFT                               
*                                                                               
EDITINTX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
* SUB-ROUTINE TO DISPLAY THE WORKCODE AND DESCRIPTION - CALLED FROM *           
* DISP.  AT ENTRY, R4=ENTRY NUMBER AND R5=A(ESTIMATE TABLE ENTRY)   *           
*********************************************************************           
         SPACE 1                                                                
DISWC    NTR1  ,                                                                
         ZIC   R3,SVNWCS                                                        
         LR    R1,R3                                                            
         LA    R1,1(R1)            INCREMENT SCREEN DATA LINE COUNT             
         STC   R1,SVNWCS                                                        
         MH    R3,=Y(L'SVTABLE)                                                 
         LA    R3,SVTABLE(R3)      INDEX TO POSITION                            
         MVC   0(3,R3),ESTWORKC                                                 
         LR    RE,R5                                                            
         S     RE,AESTTAB                                                       
         STC   R4,3(R3)            SAVE ENTRY NUMBER                            
         BAS   RE,GETWC                                                         
*                                                                               
DISWC2   L     R2,AWCDESC                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISWCX   B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE WORKCODE AND NAME                                      
* AT ENTRY, R5=A(ESTIMATE TABLE ENTRY)                                          
* ON EXIT, LISTAR CONTAINS OUTPUT DATA                                          
*                                                                               
GETWC    NTR1  ,                                                                
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(3),ESTWORKC                                               
         OI    LISTAR+2,X'40'                                                   
*                                                                               
GETWC2   CLC   ESTCAT,SLUSH        TEST FOR SLUSH ITEM                          
         BE    GETWC8              YES-READ THE WORKCODE RECORD                 
         CLC   ESTCAT,LASTCAT      TEST SAME CATEGORY AS LAST TIME              
         BE    GETWC4                                                           
*                                                                               
         MVC   LASTCAT,ESTCAT                                                   
         LA    R4,KEY                                                           
         USING ACCTKEY,R4                                                       
         XC    ACCTKEY,ACCTKEY                                                  
         MVI   ACCTRTYP,ACCTEQU                                                 
         MVI   ACCTSREC,ACCTSEQU                                                
         MVC   ACCTCUL,CUL                                                      
         MVC   ACCTSCH,JOBSCH                                                   
         MVC   ACCTCODE,ESTCAT                                                  
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
GETWC4   L     R4,AIO2                                                          
         SR    R0,R0                                                            
         LA    R6,ACRECORD                                                      
         USING ACCWD,R6                                                         
*                                                                               
GETWC6   CLI   0(R6),0             TEST FOR EOR                                 
         BE    GETWC10                                                          
         CLI   0(R6),ACCWELQ       TEST FOR WORKCODE ELEM                       
         BNE   GETWC7                                                           
         CLI   ACCWTYPE,1          TEST FOR WORKCODE                            
         BNE   GETWC7                                                           
         CLC   ESTWORKC,ACCWWORK   MATCH ON WORKCODE                            
         BNE   GETWC7                                                           
*                                                                               
         CLI   ACCWLEN,ACCWLNQ1    TEST FOR DESCRIPTION OVERRIDE                
         BE    GETWC8              NO                                           
         MVC   LISTAR+4(10),ACCWDESC                                            
         B     GETWC8                                                           
*                                                                               
GETWC7   IC    R0,ACCWLEN                                                       
         AR    R6,R0                                                            
         B     GETWC6                                                           
*                                                                               
         USING WCORECD,R4                                                       
GETWC8   LA    R4,KEY                                                           
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY(3),CUL                                                   
         MVC   WCOKWRK,ESTWORKC                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(WCOKEND),KEYSAVE                                             
         BE    GETWC9                                                           
         MVC   LISTAR+4(10),=C'** DELETED'                                      
         B     GETWC10                                                          
*                                                                               
GETWC9   MVI   ELCODE,ACANELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   GETWC10                                                          
*                                                                               
         USING ACANALD,R6                                                       
         MVC   LISTAR+4(10),ACANDESC                                            
*                                                                               
GETWC10  CLI   INTMODE,EDTLIST     TEST SOON,REP,REQ                            
         BH    GETWCX              YES                                          
*                                                                               
         LA    R4,KEY              LOOK FOR ANY TEXT UNDER THE WORKCODE         
         USING ACTXKEY,R4                                                       
         XC    ACTXKEY,ACTXKEY                                                  
         MVI   ACTXRTYP,ACTXEQU                                                 
         MVI   ACTXSREC,ACTXSEQU                                                
         MVC   ACTXCUL,CUL                                                      
         MVC   ACTXCLI,CLICODE                                                  
         MVC   ACTXPROD,PRODCODE                                                
         MVC   ACTXJOB,JOBNUM                                                   
         MVC   ACTXWORK,ESTWORKC                                                
         MVC   ACTXSUFF,ESTSUF                                                  
         MVI   ACTXFORM,C'E'       FORM=ESTIMATE                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   ACTXKEY(ACTXWHER-ACTXKEY),KEYSAVE                                
         BNE   GETWCX              NO TEXT FOR WORKCODE                         
*                                                                               
         MVI   LISTAR+3,C'*'       YES-MARK THE WORKCODE FOR USER               
*                                                                               
GETWCX   B     XIT                                                              
         DROP  R4,R6                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO DOWNLOAD A REPORT--CALLED FROM CONTROL LOGIC    *              
******************************************************************              
         SPACE 1                                                                
DOWN     NTR1  ,                                                                
         MVC   P+2(26),=C'JOB ESTIMATE DOWNLOAD DATA'                           
         GOTO1 SPOOL,DMCB,(R8)     PRINT SOMETHING ON PAGE 1                    
         MVI   FORCEHED,C'Y'       FORCE A PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
DOWN1    LA    R3,BLOCK                                                         
         USING DLCBD,R3                                                         
         XC    DLCBD(DLCBXL),DLCBD                                              
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,DOWNHK           HOOK ROUTINE FOR PRINTING                    
         ST    R1,DLCBAPR                                                       
         LA    R1,P                                                             
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)                                                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''                                                   
         MVI   DLCXEOLC,X'5E'      SEMI-COLON                                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
*                                                                               
DOWN2    GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
* OUTPUT TWO ROWS OF COLUMN HEADS                                               
*                                                                               
DOWN4    MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(8),=C'WORKCODE'                                          
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
         ZIC   R4,NCOLS                                                         
         L     R2,AFSTNAME         R2=A(NAME FIELD HEADER)                      
*                                                                               
DOWN6    MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         BAS   RE,GETFLD                                                        
         MVC   DLCBFLD,LISTAR                                                   
         GOTO1 VDLFLD,DLCBD                                                     
         BAS   RE,BUMP                                                          
         BCT   R4,DOWN6                                                         
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
DOWN8    MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(11),=C'DESCRIPTION'                                      
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
         ZIC   R4,NCOLS                                                         
         L     R2,ASECNAME                                                      
*                                                                               
DOWN10   MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         BAS   RE,GETFLD                                                        
         MVC   DLCBFLD,LISTAR                                                   
         GOTO1 VDLFLD,DLCBD                                                     
         BAS   RE,BUMP                                                          
         BCT   R4,DOWN10                                                        
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 VDLFLD,DLCBD                                                     
         TM    DLCBRETC,DLCBRCNF   TEST UNEQUAL N'FIELDS                        
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* OUTPUT THE ROWS WITH ESTIMATE VALUES                                          
*                                                                               
DOWN12   L     R5,AESTTAB          R5=A(ESTIMATE TABLE ENTRY)                   
         USING ESTTABD,R5                                                       
         L     R4,NESTENT          R4=LOOP COUNTER                              
         XC    LASTCAT,LASTCAT                                                  
*                                                                               
DOWN14   BAS   RE,FILCAT           APPLY CATEGORY FILTER                        
         BNE   DOWN16                                                           
         BAS   RE,FILZERO          APPLY ZERO FILTER                            
         BNE   DOWN16                                                           
         BAS   RE,FILWC            APPLY WORKCODE SELECT FILTER                 
         BNE   DOWN16                                                           
         BAS   RE,FILSW            APPLY WORKCODE START FILTER                  
         BNE   DOWN16                                                           
*                                                                               
         BAS   RE,GETWC                                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD,LISTAR      WORKCODE/NAME                                
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
         USING COLD,R6                                                          
         ZIC   R0,NCOLS                                                         
         LA    R2,ESTVALS                                                       
         LA    R6,COLINDS                                                       
*                                                                               
DOWN15   MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBPACK                                                 
         MVC   DLCBFLD(L'ESTVALS),0(R2)                                         
         MVI   DLCBLEN,L'ESTVALS                                                
         MVI   DLCBNDP,2                                                        
         TM    COLFLAG,COLFCOM     TEST FOR COMMISSION RATE COLUMN              
         BZ    *+8                                                              
         MVI   DLCBNDP,4           FOUR DECIMAL PLACES                          
         GOTO1 VDLFLD,DLCBD                                                     
         LA    R2,L'ESTVALS(R2)                                                 
         LA    R6,COLLENQ(R6)                                                   
         BCT   R0,DOWN15                                                        
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 VDLFLD,DLCBD                                                     
         TM    DLCBRETC,DLCBRCNF   TEST FOR UNEQUAL NUMBER OF FIELDS            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DOWN16   A     R5,LESTTAB                                                       
         BCT   R4,DOWN14                                                        
*                                                                               
DOWN20   MVI   DLCBACT,DLCBEOR     END OF REPORT                                
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
DOWNX    B     XIT                                                              
         DROP  R6                                                               
         SPACE 2                                                                
* HOOK ROUTINE TO PRINT LINE                                                    
*                                                                               
DOWNHK   NTR1  ,                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              PREVENT A PAGE BREAK                         
         B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO EXTRACT A FIELD'S DATA                                         
* AT ENTRY, R2=A(FIELD HEADER), ON EXIT LISTAR CONTAINS SPACE                   
* PADDED DATA                                                                   
*                                                                               
GETFLD   NTR1  ,                                                                
         MVC   LISTAR,SPACES                                                    
         ZIC   R1,0(R2)                                                         
         LA    R0,9                                                             
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         LA    R0,17                                                            
         SR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTAR(0),8(R2)                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    LISTAR(0),SPACES                                                 
         B     XIT                                                              
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO PRINT A REPORT OF SCREEN DATA--CALLED FROM   *                 
* MAIN LOGIC                                                  *                 
***************************************************************                 
         SPACE 1                                                                
PRINT    NTR1  ,                                                                
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         MVI   RCSUBPRG,0                                                       
         BAS   RE,ACCNAME          GET CLIENT/PRODUCT/JOB NAMES                 
*                                                                               
PRINT2   L     R5,AESTTAB          R5=A(ESTIMATE TABLE ENTRIES)                 
         USING ESTTABD,R5                                                       
         L     R4,NESTENT          R4=N'ESTIMATE TABLE ENTRIES                  
         LA    R3,P                R3=A(PRINT LINE)                             
         USING PRTD,R3                                                          
         XC    LASTCAT,LASTCAT                                                  
*                                                                               
* PRINT THE WORKCODE DETAIL                                                     
*                                                                               
PRINT4   BAS   RE,FILCAT           FILTER ON CATEGORY                           
         BNE   PRINT10             SKIP WORKCODE LINE                           
         BAS   RE,FILZERO          FILTER ON ZERO LINE IF REQUIRED              
         BNE   PRINT10                                                          
         BAS   RE,FILWC            APPLY WORKCODE SELECT FILTER                 
         BNE   PRINT10                                                          
         BAS   RE,FILSW            APPLY WORKCODE START FILTER                  
         BNE   PRINT10                                                          
*                                                                               
PRINT5   BAS   RE,GETWC            GET WORKCODE AND NAME                        
         MVC   PRTWDESC,LISTAR                                                  
         GOTO1 PRTCOL,ESTVALS                                                   
*                                                                               
PRINT8   MVI   ALLOWLIN,2                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 (RF),(R1),(R8)                                                   
*                                                                               
PRINT10  A     R5,LESTTAB                                                       
         BCT   R4,PRINT4                                                        
*                                                                               
* PRINT THE JOB-TOTALS LINE                                                     
*                                                                               
PRINT12  MVC   PRTWDESC(10),=C'JOB TOTALS'                                      
         GOTO1 PRTCOL,SVJOBTOT                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRINTX   B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO PRINT OUT THE COLUMN VALUES                                    
*                                                                               
* AT ENTRY, R1=A(ESTIMATE VALUES)                                               
*                                                                               
         USING COLD,R4                                                          
PRTCOL   NTR1  ,                                                                
         LR    R2,R1               R2=A(ESTIMATE VALUES)                        
         ZIC   R5,NCOLS            R5=N'COLUMNS                                 
         LA    R6,PRTVAL           R6=A(PRINT POSITION)                         
         LA    R4,COLINDS          R4=A(COLUMN INDICATORS)                      
*                                                                               
PRTCOL2  TM    COLFLAG,COLFCOM     TEST FOR COMMISSION RATE                     
         BO    PRTCOL3                                                          
*                                                                               
         CURED (P6,0(R2)),(L'PRTVAL,0(R6)),2,MINUS=YES                          
         B     PRTCOL4                                                          
*                                                                               
PRTCOL3  CP    0(L'ESTVALS,R2),=P'0' TEST FOR ZERO RATE                         
         BE    PRTCOL4             YES-SKIP OUTPUT                              
         CURED (P6,0(R2)),(L'PRTVAL,0(R6)),4                                    
*                                                                               
PRTCOL4  LA    R2,L'ESTVALS(R2)                                                 
         LA    R4,COLLENQ(R4)                                                   
         LA    R6,L'PRTVAL+1(R6)                                                
         BCT   R5,PRTCOL2                                                       
*                                                                               
PRTCOLX  B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE ACCOUNT NAMES-CLIENT, PRODUCT, AND JOB                 
* CALLED FROM PRINT                                                             
*                                                                               
ACCNAME  NTR1  ,                                                                
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(L'CLICODE),CLICODE                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 SETNAME,DMCB,AIO,BLOCK                                           
*                                                                               
         ZIC   R1,LCLI                                                          
         LA    R1,KEY+3(R1)        POINT AT PRODUCT POSITION                    
         MVC   0(L'PRODCODE,R1),PRODCODE                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 SETNAME,DMCB,AIO,BLOCK+36                                        
*                                                                               
         ZIC   R1,LCLIPRO                                                       
         LA    R1,KEY+3(R1)        POINT AT JOB                                 
         MVC   0(L'JOBNUM,R1),JOBNUM                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 SETNAME,DMCB,AIO,BLOCK+72                                        
*                                                                               
ACCNAMEX B     XIT                                                              
         EJECT                                                                  
* HOOK ROUTINE FOR REPORT PRINTING                                              
*                                                                               
HOOK     NTR1  ,                                                                
         MVC   H4+10(L'CLICODE),CLICODE                                         
         MVC   H4+18(36),BLOCK     CLIENT NAME                                  
         MVC   H5+10(L'PRODCODE),PRODCODE                                       
         MVC   H5+18(36),BLOCK+36  PRODUCT NAME                                 
         MVC   H6+10(L'JOBNUM),JOBNUM                                           
         MVC   H6+18(36),BLOCK+72  JOB NAME                                     
*                                                                               
* PRINT THE COLUMN NAMES IN HEADING BY EXTRACTING THE SCREEN DISPLAY            
*                                                                               
HOOK2    ZIC   R4,NCOLS                                                         
         L     R2,AFSTNAME         R2=A(NAME FIELD)                             
         LA    R6,H8               R6=OUTPUT POINTER                            
         LA    R6,PRTVAL-PRTD(R6)                                               
         MVI   BYTE,C'N'                                                        
*                                                                               
HOOK4    BAS   RE,GETFLD                                                        
         MVC   0(L'PRTVAL,R6),LISTAR                                            
         CLC   0(L'PRTVAL,R6),SPACES TEST FOR NAME                              
         BE    HOOK5               NO                                           
         GOTO1 CENTER,PARAS,(R6),L'PRTVAL                                       
*                                                                               
HOOK5    BAS   RE,BUMP                                                          
         LA    R6,L'PRTVAL+1(R6)                                                
         BCT   R4,HOOK4                                                         
*                                                                               
HOOK6    CLI   BYTE,C'Y'           TEST FOR SECOND LINE                         
         BE    HOOKX                                                            
*                                                                               
         MVI   BYTE,C'Y'           SET FOR SECOND PASS                          
         ZIC   R4,NCOLS                                                         
         LA    R6,H9                                                            
         LA    R6,PRTVAL-PRTD(R6)                                               
         L     R2,ASECNAME                                                      
         B     HOOK4                                                            
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
* SUB-ROUTINE TO GET THE ESTIMATE REQUEST SCREEN INTO BUFF  *                   
*************************************************************                   
         SPACE 1                                                                
GETREQ   NTR1  ,                                                                
         LA    RE,BUFF             COPY SCREEN HEADER INTO BUFF                 
         LA    RF,CONTAGH-T60BFFD                                               
         LA    R1,0(RE,RF)                                                      
         ST    R1,ATAG                                                          
         LR    R1,RF                                                            
         L     R0,ATWA                                                          
         MVCL  RE,R0                                                            
*                                                                               
GETREQ2  MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'C9'        LOAD IN ESTIMATE REQUEST SCREEN              
         MVC   DMCB(4),ATAG                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETREQ4  LA    R3,BUFF                                                          
         LA    R2,CONRECH-T60BFFD(R3)                                           
         MVC   8(8,R2),=C'ESTIMATE' SET RECORD                                  
         MVI   5(R2),8                                                          
*                                                                               
         LA    R2,CONACTH-T60BFFD(R3)                                           
         MVC   8(6,R2),=C'REPORT'                                               
         MVI   5(R2),6                                                          
*                                                                               
         LA    R2,CONWHENH-T60BFFD(R3)                                          
         CLI   INTMODE,SOONREP                                                  
         BE    GETREQ6                                                          
         MVC   DUB(3),=C'OV,'                                                   
         MVC   DUB+3(3),TWAALIAS                                                
         MVC   8(6,R2),DUB                                                      
         MVI   5(R2),4                                                          
         CLI   DUB+4,C' '                                                       
         BE    *+8                                                              
         MVI   5(R2),5                                                          
         CLI   DUB+5,C' '                                                       
         BE    *+8                                                              
         MVI   5(R2),6                                                          
         B     GETREQX                                                          
*                                                                               
GETREQ6  MVC   DUB(5),=C'SOON,'                                                 
         MVC   DUB+5(3),TWAALIAS                                                
         MVC   8(8,R2),DUB                                                      
         MVI   5(R2),6                                                          
         CLI   DUB+6,C' '                                                       
         BE    *+8                                                              
         MVI   5(R2),7                                                          
         CLI   DUB+7,C' '                                                       
         BE    *+8                                                              
         MVI   5(R2),8                                                          
*                                                                               
GETREQX  B     XIT                                                              
         EJECT                                                                  
************************************************************                    
* SUB-ROUTINE TO CALL GENCON WITH A REPORT NAME            *                    
************************************************************                    
         SPACE 1                                                                
GETREP   NTR1  ,                                                                
         L     RE,ATWA             FIND OUT WHERE TO LOAD C9                    
         LA    RF,CONTAGH-T60BFFD(RE)                                           
         ST    RF,ATAG                                                          
*                                                                               
         LA    RE,BUFF             COPY SCREEN HEADER INTO BUFF                 
         LA    RF,SAVAREA-T60BFFD                                               
         LR    R1,RF                                                            
         L     R0,ATWA                                                          
         MVCL  RE,R0                                                            
*                                                                               
         MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'C9'        LOAD IN ESTIMATE REQUEST SCREEN              
         MVC   DMCB(4),ATAG                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,CONRECH                                                       
         MVC   8(8,R2),=C'ESTIMATE'                                             
         MVI   5(R2),8                                                          
*                                                                               
         LA    R2,CONACTH                                                       
         MVC   8(7,R2),=C'DISPLAY'                                              
         MVI   5(R2),7                                                          
*                                                                               
         LA    R2,CONKEYH                                                       
         MVC   8(L'REPNAME,R2),REPNAME                                          
         LA    R1,L'REPNAME                                                     
         LA    RE,REPNAME+L'REPNAME-1                                           
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         STC   R1,5(R2)                                                         
*                                                                               
         BAS   RE,SETRD            SET RETURN ADDRESS                           
         L     RF,=V(DUMMY)        PASS GENCON END OF ACPRO46                   
         A     RF,RELO                                                          
         ST    RF,SYSDUMMY                                                      
         GOTO1 GENCON,DMCB,(R8)    READ THE REPORT RECORDS                      
         CLI   OKNO,X'03'          FOUND IT ?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   USEIO,SVUSEIO       RESTORE ACC FILE VALUES                      
         MVC   SYSDIR,SVSYSDIR     AND SWITCH BACK TO ACC                       
         MVC   SYSFIL,SVSYSFIL                                                  
         MVC   DATADISP,SVDATADI                                                
         MVC   LKEY,SVLKEY                                                      
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         MVC   DMCB(1),SVSYS                                                    
         MVC   FILENAME,SVFILENM                                                
         GOTO1 SWITCH,DMCB                                                      
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SYSRD,SAVERE        RESTORE SYSRD                                
         GOTO1 FIND,4              CLEAR OFFICE GROUP, OFFICE,                  
         BAS   RE,CLEARIT           CLIENT, PRODUCT AND JOB                     
         GOTO1 FIND,6                                                           
         BAS   RE,CLEARIT                                                       
         GOTO1 FIND,8                                                           
         BAS   RE,CLEARIT                                                       
         GOTO1 FIND,10                                                          
         BAS   RE,CLEARIT                                                       
         GOTO1 FIND,12                                                          
         BAS   RE,CLEARIT                                                       
         B     XIT                                                              
*                                                                               
CLEARIT  SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         LA    R0,9                                                             
         TM    1(R2),X'02'         DOES FIELD HAVE AN EXTENDED HEADER ?         
         BZ    *+8                 NO                                           
         LA    R0,17               YES                                          
         SR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR OUT DATA                               
         MVI   5(R2),0             SET LENGTH                                   
         BR    RE                                                               
*                                                                               
SETRD    NTR1  ,                                                                
         MVC   SAVERE,SYSRD        SAVE SYSRD HERE                              
         ST    RD,SYSRD                                                         
         B     XIT                                                              
         EJECT                                                                  
************************************************************                    
* SUB-ROUTINE TO BUILD THE ESTIMATE REQUEST SCREEN         *                    
************************************************************                    
         SPACE 1                                                                
BLDREQ   NTR1  ,                                                                
         LA    RA,BUFF                                                          
         OC    REPNAME,REPNAME     DO WE HAVE A REPORT NAME ?                   
         BNZ   BLDREQA             YES, DATA IS IN BUFF                         
         L     RA,ATWA             NO, DATA IS IN TWA                           
*                                                                               
         GOTO1 FIND,2                                                           
         MVI   8(R2),C'L'          TYPE                                         
         MVI   5(R2),1                                                          
*                                                                               
BLDREQA  GOTO1 FIND,8              CLIENT                                       
         GOTO1 SCUNKEY,DMCB,PROCLIH,(R2)                                        
*                                                                               
         GOTO1 FIND,10                                                          
         GOTO1 SCUNKEY,DMCB,PROPROH,(R2)                                        
*                                                                               
         GOTO1 FIND,12                                                          
         GOTO1 SCUNKEY,DMCB,PROJOBH,(R2)                                        
*                                                                               
         OC    REPNAME,REPNAME     DO WE HAVE A REPORT NAME ?                   
         BZ    BLDREQ2             NO, GET ALL FIELDS                           
         CLI   REPFLAG,C'Y'        YES, USE COLUMNS FROM REQUEST ?              
         BE    BLDREQX             YES                                          
*                                                                               
         GOTO1 FIND,40             NO, CLEAR THEM OUT                           
         LR    R5,R1               SAVE HEADER NUMBER                           
BLDREQ0  BAS   RE,CLEARIT                                                       
*                                                                               
BLDREQ1  BAS   RE,BUMPTOUN                                                      
         CLI   0(R2),0             END OF DATA                                  
         BE    BLDREQ2                                                          
         TM    1(R2),X'02'         ANY HEADER ?                                 
         BZ    BLDREQ1             NO, CAN'T FIND IT THEN                       
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'8'                                                         
         LA    RF,0(R2,RF)                                                      
         CLM   R5,1,0(RF)          IF WE DON'T MATCH, WE'RE DONE                
         BE    BLDREQ0                                                          
*                                                                               
BLDREQ2  GOTO1 FIND,40             COLUMNS                                      
         GOTO1 SCUNKEY,DMCB,(C',',PROCOLH),(R2)                                 
*                                                                               
         OC    REPNAME,REPNAME     DO WE HAVE A REPORT NAME ?                   
         BNZ   BLDREQX             YES, ALL DONE WITH THESE FIELDS              
*                                                                               
         CLI   MAXLOPT,0           TEST FOR MAX LINES OVERRIDE                  
         BE    BLDREQ4             NO                                           
*                                                                               
         GOTO1 FIND,34             OPTIONS                                      
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(8),=C'MAXLINES'                                           
         MVI   LISTAR+8,C'='                                                    
         ZIC   R0,MAXLOPT                                                       
         EDIT  (R0),(3,LISTAR+9)                                                
         GOTO1 SQUASHER,DMCB,LISTAR,12                                          
         L     RF,4(R1)                                                         
         BAS   RE,MOVEFLD          ADD INTO REQUEST SCREEN                      
         STC   RF,5(R2)                                                         
*                                                                               
BLDREQ4  MVC   LISTAR,SPACES       COPY OPTIONS FIELD TO WORK AREA              
         MVC   LISTAR(L'PROOPTH+L'PROOPT),PROOPTH                               
         CLI   MAXLOPT,0           TEST MAXLINES OVERRIDE                       
         BE    BLDREQ14                                                         
*                                                                               
         ZIC   R0,PROOPTH+5        LOOP COUNTER                                 
         LA    R4,LISTAR+8         POINT TO FILED DATA                          
BLDREQ6  CLC   =C'MAXL',0(R4)      LOOK FOR MAXLINES IN STRING                  
         BE    BLDREQ8                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,BLDREQ6                                                       
         DC    H'0'                                                             
*                                                                               
BLDREQ8  LA    R1,4                INITIALIZE STRING LENGTH IN R1               
         LR    R2,R4               SAVE START OF STRING IN R2                   
         LA    R4,4(R4)            BUMP PAST START OF KEYWORD                   
         SH    R0,=H'4'            ADJUST COUNTER                               
*                                                                               
BLDREQ10 CLI   0(R4),C','          SEARCH FOR END OF OPTION                     
         BE    BLDREQ12                                                         
         LA    R4,1(R4)            NEXT CHARACTER                               
         LA    R1,1(R1)            INCREMENT L'MAXLINES INPUT                   
         BCT   R0,BLDREQ10                                                      
*                                                                               
BLDREQ12 EX    R1,*+8              CLEAR AREA WHERE MAXLINES IS                 
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
         ZIC   R4,LISTAR+5                                                      
         GOTO1 SQUASHER,DMCB,LISTAR+8,(R4)                                      
         L     RF,4(R1)            GET LENGTH OF OUTPUT                         
         STC   RF,LISTAR+5         RESET FIELD LENGTH                           
*                                                                               
BLDREQ14 GOTO1 FIND,50             DETAILS                                      
         GOTO1 SCUNKEY,DMCB,(X'01',LISTAR),(R2)                                 
*                                                                               
BLDREQX  B     XIT                                                              
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO GENERATE THE SOON REQUEST AND COMPLETE THE   *                 
* MESSAGE.  AT ENTRY, BUFF CONTAINS THE FILLED IN ESTIMATE    *                 
* REPORT SCREEN.                                              *                 
***************************************************************                 
         SPACE 1                                                                
GENREQ   NTR1  ,                                                                
         LA    R2,BUFF             SET BUFF ADDRESS FOR REQTWA                  
         OC    REPNAME,REPNAME     DO WE HAVE A REPORT NAME ?                   
         BZ    GENREQ2             NO,                                          
*                                                                               
         LA    R2,CONACTH          YES, CHANGE ACTION AND WHEN                  
         MVC   8(6,R2),=C'REPORT'                                               
         MVI   5(R2),6                                                          
*                                                                               
         LA    R2,CONWHENH                                                      
         CLI   INTMODE,OVERREP     IS THIS AN OVERNIGHT REPORT ?                
         BE    GENREQ0             YES, DIFFERENT SETUP                         
         MVC   DUB(5),=C'SOON,'                                                 
         MVC   DUB+5(3),TWAALIAS                                                
         MVC   8(8,R2),DUB                                                      
         MVI   5(R2),6                                                          
         CLI   DUB+6,C' '                                                       
         BE    *+8                                                              
         MVI   5(R2),7                                                          
         CLI   DUB+7,C' '                                                       
         BE    *+8                                                              
         MVI   5(R2),8                                                          
         B     GENREQ1                                                          
*                                                                               
GENREQ0  MVC   DUB(3),=C'OV,'                                                   
         MVC   DUB+3(3),TWAALIAS                                                
         MVC   8(6,R2),DUB                                                      
         MVI   5(R2),4                                                          
         CLI   DUB+4,C' '                                                       
         BE    *+8                                                              
         MVI   5(R2),5                                                          
         CLI   DUB+5,C' '                                                       
         BE    *+8                                                              
         MVI   5(R2),6                                                          
*                                                                               
GENREQ1  L     R2,ATWA             AND GIVE REQTWA TWA ADDRESS                  
*                                                                               
GENREQ2  LA    R3,BLOCK            BUILD SPOOK BLOCK                            
         USING SPOOK,R3                                                         
         XC    SPOOK(SPOOKL),SPOOK                                              
         MVC   SPOOKUID,TWAORIG                                                 
         MVC   SPOOKDES,TWADEST                                                 
         MVC   SPOOKTID,TERM                                                    
         MVC   SPOOKSEN,SYSPHASE+1                                              
         MVC   SPOOKERN,GETMSYS                                                 
         MVC   SPOOKAGY,AGENCY                                                  
         MVC   SPOOKAGX,CUL                                                     
         MVC   SPOOKDID,TWAALIAS                                                
         MVC   SPOOKSYS,RCPROG                                                  
         MVI   SPOOKWEN,2          SOON                                         
*                                                                               
         MVC   SPOOKEOD,=C'ES'     LIVE ESTIMATE                                
         MVC   SPOOKJCL,=C'ES'                                                  
         ST    R2,SAVER2           SAVE REGISTER 2                              
         GOTO1 FIND,2                                                           
         CLI   8(R2),C'L'          IS THIS LIVE ?                               
         BE    GENREQ3             YES                                          
         MVC   SPOOKEOD,=C'EY'     NO, CHANGE TO DRAFT                          
         MVC   SPOOKJCL,=C'EY'                                                  
*                                                                               
GENREQ3  L     R2,SAVER2           RESTORE REGISTER 2                           
         CLI   INTMODE,OVERREP     IS THIS AN OVERNIGHT REPORT ?                
         BNE   *+8                 NO, LEAVE AS SOON                            
         MVI   SPOOKWEN,4          OVER                                         
*                                                                               
         L     R4,AIO1             BUILD REQUEST HEADER+80 BYTE CARD            
         USING REQHDR,R4                                                        
         XC    REQHDR(REQEOH-REQHDR),REQHDR                                     
         MVC   REQUEST,SPACES                                                   
         MVC   REQJCLID,SPOOKJCL                                                
         MVC   REQAGYID,AGENCY                                                  
         GOTO1 GETFACT,DMCB,0                                                   
         L     R5,DMCB                                                          
         USING FACTSD,R5                                                        
         ICM   R0,15,FASIN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REQSIN,DUB                                                       
         GOTO1 REQTWA,DMCB,(R2),REQHDR,DATAMGR,ACOMFACS,SPOOK                   
*                                                                               
         OC    REPNAME,REPNAME     DID WE HAVE A REPORT NAME ?                  
         BZ    GENREQ4             NO                                           
*                                                                               
         MVC   PARAS+4(3),SYSPHASE                                              
         MVI   PARAS+7,X'C2'       LOAD IN JOB ESTIMATE SCREEN                  
         MVC   PARAS(4),ATAG                                                    
         GOTO1 CALLOV,PARAS                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,ATWA             RESTORE JOB ESTIMATE SCREEN                  
         LA    RF,SAVAREA-T60BFFD                                               
         LR    R5,RF                                                            
         LA    R4,BUFF                                                          
         MVCL  RE,R4                                                            
         LA    R1,DMCB             RESTORE R1 LOCATION                          
*                                                                               
GENREQ4  CLI   INTMODE,OVERREP     IS THIS AN OVERNIGHT REQUEST ?               
         BNE   GENREQ5             NO, MUST BE SOON                             
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'OVERMSG),OVERMSG                                        
         B     GENREQ9                                                          
*                                                                               
GENREQ5  LA    R2,PROCLIH          POSITION IN CASE OF ERROR                    
         CLI   8(R1),X'FE'         TEST TERMINAL QUEUE FULL                     
         BNE   *+12                                                             
         MVI   ERROR,TQUEFULL                                                   
         B     ERREND                                                           
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BNE   *+12                                                             
         MVI   ERROR,PQUEFULL                                                   
         B     ERREND                                                           
*                                                                               
         L     RE,8(R1)            RE=A(PRINT QUEUE KEY)                        
         OC    0(7,RE),0(RE)       TEST KEY DATA PRESENT                        
         BNZ   GENREQ6                                                          
         MVI   ERROR,NOJCL                                                      
         B     ERREND                                                           
*                                                                               
GENREQ6  MVC   LISTAR,SPACES       PREPARE HEADER MESSAGE                       
         SR    R0,R0                                                            
         ICM   R0,3,6(RE)          GET REPORT NUMBER                            
         LA    R4,LISTAR                                                        
         MVC   0(2,R4),=C'**'                                                   
         MVC   3(3,R4),SPOOKDID                                                 
         LA    R4,4(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    GENREQ8                                                          
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    GENREQ8                                                          
         LA    R4,1(R4)                                                         
*                                                                               
GENREQ8  MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         EDIT  (R0),(5,0(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         MVC   1(L'SOONMSG,R4),SOONMSG                                          
*                                                                               
GENREQ9  MVC   CONHEAD,LISTAR      SET THE MESSAGE                              
*                                                                               
GENREQX  B     XIT                                                              
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
* SUB-ROUTINE TO SET SCREEN LINE ADDRESSES                                      
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,NDATAFLD                                                      
         LA    R1,ASEL                                                          
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,*-12                                                          
         ST    R2,ANEXTSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO FIND THE FIELD ON ESTIMATE REPORT SCREEN                       
*                                                                               
* ON ENTRY, R1=FIELD NUMBER                                                     
*                                                                               
FIND     ST    RE,SAVERE                                                        
         L     R2,ATAG                                                          
*                                                                               
FIND2    BAS   RE,BUMPTOUN                                                      
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   *+6                                                              
         DC    H'0'                A PROBLEM                                    
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    FIND2               NO-SKIP IT                                   
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'8'                                                         
         LA    RF,0(R2,RF)         POINT TO FIELD NUMBER                        
         CLM   R1,1,0(RF)          MATCH ON FIELD NUMBER                        
         BNE   FIND2               NO                                           
*                                                                               
FINDX    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
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
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
GETELI2  L     R1,AIO2                                                          
         GETEL2 (R1),DATADISP,ELCODE                                            
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
YESXIT   CR    RB,RB               SET CC=EQ AND EXIT                           
         B     XIT                                                              
         SPACE 1                                                                
NOXIT    LTR   RB,RB               SET CC=NEQ AND EXIT                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
RELO     DC    A(0)                                                             
SAVER2   DS    A                   SAVEAREA FOR REGISTER 2                      
         EJECT                                                                  
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
SLUSH    DC    C'&&&&'                                                          
SOONMSG  DC    C'WILL BE PROCESSED SOON **'                                     
OVERMSG  DC    C'REPORT WILL BE PROCESSED OVERNIGHT'                            
         SPACE 2                                                                
SUPPNAME DC    CL11'SUPPLEMENT'                                                 
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SPECS FOR JOB ESTIMATE REPORT                                                 
*                                                                               
HEDSPECS DS    0D                                                               
         SSPEC H1,2,CREATED                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,45,C'JOB ESTIMATE REPORT'                                     
         SSPEC H2,45,C'-------------------'                                     
         SSPEC H1,85,AGYNAME                                                    
         SSPEC H2,85,AGYADD                                                     
         SSPEC H4,85,REPORT                                                     
         SSPEC H4,98,PAGE                                                       
*                                                                               
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'JOB'                                                      
*                                                                               
         SSPEC H8,3,C'WORKCODE'                                                 
         SSPEC H9,3,C'DESCRIPTION'                                              
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*DDTWABLDD                                                                      
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDTWABLDD                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE ACPRO32COM                                                     
         EJECT                                                                  
* DSECT TO COVER DOWNLOAD BLOCK                                                 
*                                                                               
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
* DSECT TO COVER SPOOK BLOCK                                                    
*                                                                               
       ++INCLUDE DDSPOOK                                                        
         SPACE 2                                                                
* DSECT TO COVER REQUEST HEADER AND CARD                                        
*                                                                               
REQHDR   DSECT                                                                  
       ++INCLUDE DMREQHDR                                                       
REQUEST  DS    0CL80               REQUEST CARD LAYOUT                          
REQJCLID DS    CL2                 JCL ID                                       
REQAGYID DS    CL2                 AGENCY ID                                    
         DS    CL1                 N/D                                          
REQSIN   DS    CL6                 SYSTEM INPUT NUMBER                          
         ORG   REQUEST+L'REQUEST                                                
         EJECT                                                                  
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
         DS    CL2                 SPARE                                        
PRTWDESC DS    CL20                WORKCODE DESCRIPTION                         
         DS    C                                                                
PRTVAL   DS    CL14                PRINT VALUES                                 
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACPRO46   02/24/15'                                      
         END                                                                    
