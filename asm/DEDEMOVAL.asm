*          DATA SET DEDEMOVAL  AT LEVEL 054 AS OF 08/14/20                      
*PROCESS USING(WARN(15))           ENABLE ALL USING STATEMENT WARNINGS          
*PHASE T00AD9B                                                                  
*INCLUDE BINSR31                                                                
DEMOVAL  TITLE '-  VALIDATE A STRING OF DEMO EXPRESSIONS'                       
*                                                                               
* File DCDEMOVAL.txt shared from Alfresco, look at it here:                     
* https://docs.gomocean.info/share/s/g4jtgQ6PR5GZi5oRVDObwg                     
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-34636  11/07/19 SUPPORT COMSCORE DEMO INDEX "M"           *         
***********************************************************************         
***********************UPDATE LOG************************************           
* 03/25/91     64   REMOVE LEVEL 43 CHANGE                          *           
* 10/25/90     61   RADIO DEFAULT MODIFIER = I                      *           
* 06/13/90     60   MAKE T12+ INVALID, ADD LISTEN TO LIST           *           
* 03/30/89     58   ALLOW ALL DEMOS FOR CSI (U.S. FORMAT)           *           
* 01/06/89     55   ADD REACH TO EDITABLE DEMOS                     *           
* 07/27/88     55   ADD WOMN TO EDITABLE DEMOS                      *           
* 04/29/88     54   ADD HWC TO EDITABLE DEMOS                       *           
* 02/25/88     45   MALE AND FEMALE DEFAULTS TO 2+ (WAS 12+)        *           
* 02/04/88     43   FORCE CATEGORY TO 1 IF MISSING ON NAD VALIDATION*           
*********************************************************************           
                                                                                
*-------------------------------------------------------------------*           
* NOTES ON DEMOVAL LOGIC WHEN DBDEMTYP=C'4' OR 'P':                             
*                                                                               
* 1. LOOK UP ALPHA PIECE OF DEMO IN TABLE SEXTABN.                              
*    THIS IS AN EXECUTED COMPARE IF THE ALPHA PORTION IS A PREFIX               
*    (E.G., WM1849). IF IT'S A COMPLETE DEMO (E.G., HOMES), THEN THE            
*    ENTIRE WORD MUST BE ENTERED.                                               
*    MAKE SURE INPUT AGE RANGE IS IN THE VALID RANGE FOR THIS PREFIX.           
*    PICK UP THE LOOKUP CODE AND DEMO NUMBER IF AVAILABLE.                      
*                                                                               
* 2. IF A SEX/AGE DEMO (EX: WM1834, FE1829), LOOK UP THE AGE BREAK              
*    IN TABLE AGETABN. IF VALID, PICK UP THE INTERNAL NUMBER ASSIGNED           
*    TO THE AGE BREAK.                                                          
*                                                                               
* 3. VALIDATE MODIFIERS AGAINST DATASPACE TABLE BY FILE. THIS TABLE             
*    WAS BUILT USING THE 'DCODE' RECORDS.                                       
*                                                                               
* 4. SCAN TABLE EQU2TO3 FOR THE 2 BYTE COMBINATION OF LOOKUP CODE AND           
*    DEMO NUMBER. THIS TABLE INCLUDES ENTRIES ONLY FOR THE DEMOS                
*    FROM THE STANDARD GROUP OF UP TO 256 DEMOS THAT HAVE A 1-BYTE              
*    DEMO#.                                                                     
*     THE OUTPUT IN THE 2-BYTE DEMO NUMBER WILL BE:                             
*       IF ENTRY FOUND IN TABLE, '00'+ 1-BYTE DEMO NUMBER                       
*          EXAMPLE: WM1834 GETS TRANSLATED TO X'0029'                           
*       IF NO ENTRY FOUND IN TABLE, LOOKUP CODE + AGE BREAK#                    
*          EXAMPLE: FE1829 GETS TRANSLATED TO X'0344'                           
*-------------------------------------------------------------------*           
         EJECT                                                                  
DEMOVAL  RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 DEMWORKX-DEMWORKD,**DEMVAL,R7,RR=RE,CLEAR=YES                    
         USING DEMWORKD,RC         RC=A(W/S)                                    
         ST    RE,RELO                                                          
         LR    R5,R1                                                            
         USING PARMD,R5            R5=A(PARM LIST)                              
         LM    R8,R9,AOUTPUT       R8=A(OUTPUT AREA)                            
         USING DBLOCKD,R9          R9=A(DBLOCK)                                 
         L     RA,DBCOMFCS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES LIST)                 
*                                                                               
* GET SYSTEM NUMBER                                                             
*                                                                               
         MVI   SVOVSYS,0                                                        
         ICM   RE,15,CMASTC          OFFLINE GET ADDR FROM MASTC                
         BZ    DEM0                  NOT OFFLINE                                
         MVC   SVOVSYS,MCOVSYS-MASTD(RE)                                        
         B     DEM0A                                                            
*                                                                               
DEM0     ICM   RE,15,CGETFACT        ONLINE                                     
         BZ    DEM0A                                                            
         GOTO1 CGETFACT,DMCB,(X'80',SVOVSYS),F#TOVSYS                           
*                                                                               
DEM0A    L     RF,=V(BINSRCH)                                                   
         A     RF,RELO                                                          
         ST    RF,VBINSRCH                                                      
*                                                                               
         ICM   R1,15,CDEMOCON      IS A(DEMOCON) SET IN COMFACS?                
         BNZ   DEM0B               YES                                          
         GOTO1 CCALLOV,DMCB,0,X'D9000AE0'  NO: GET A(DEMOCON)                   
         CLI   DMCB+4,X'FF'        WAS A(DEMOCON) RETURNED FROM CALLOV?         
         BNE   *+6                                                              
         DC    H'0'                NO                                           
         MVC   CDEMOCON,DMCB       SET A(DEMOCON) IN COMFACS                    
*                                                                               
DEM0B    GOTO1 CDEMOCON,DMCB,(0,0),('DEMOCON_14',APLDTABS),0                    
*                                                                               
         LA    RE,AGETAB                                                        
         LA    RF,SEXTAB                                                        
*                                                                               
         MVC   FLAVOR,DBDEMTYP                                                  
         CLI   FLAVOR,C'P'         PACKED EXTENDED DEMOS                        
         BE    *+8                                                              
         CLI   FLAVOR,C'4'         4 CHAR EXTENDED DEMOS                        
         BNE   *+12                                                             
         LA    RE,AGETABN              TABLES FOR 4 CHAR DEMOS                  
         LA    RF,SEXTABN               OR PACKED DEMOS                         
*                                                                               
         ST    RE,AAGETAB                                                       
         ST    RF,ASEXTAB                                                       
*                                                                               
         MVI   SCANCHR,C','                                                     
         CLI   NUSRDEMS,C'/'       SLASH MAY BE USED AS DELIMITER               
         BNE   *+8                                                              
         MVI   SCANCHR,C'/'                                                     
         CLI   IOPTN,C'S'          TEST SPOT VALIDATION                         
         BNE   DEM1                                                             
         MVI   NUSRDEMS,0          RESET USER DEMO COUNTER                      
         MVI   PASS,1              SET PASS 1 FOR COMSCORE                      
         MVI   NEWNONT,C'N'                                                     
         ICM   R3,15,AUSRNMS       GET USER NAME                                
         BZ    DEM1                                                             
         XC    0(35,R3),0(R3)      CLEAR USER DEMO NAME AREA                    
*                                                                               
DEM1     MVC   NADR,AINPUT         SET A(FIRST TWA FIELD)                       
         MVC   NFLDS,NINPUT        SET N'TWA FIELDS                             
         CLI   NFLDS,0                                                          
         BNE   *+8                                                              
         MVI   NFLDS,1                                                          
         MVC   NLIST,NOUTPUT       SET MAX N'OUTPUT LIST ENTRIES                
         CLI   NLIST,0                                                          
         BNE   *+8                                                              
         MVI   NLIST,FF                                                         
*        MVI   NINPUT,0            REMOVED FOR PASS 2 COMSCORE BUG              
*                                  SCHT - 8/17                                  
         MVI   NOUTPUT,0                                                        
         MVI   DEMOFLAG,0                                                       
         MVI   0(R8),FF                                                         
         MVI   DEMOINDX,1                                                       
         MVI   PASS,1              SET PASS 1 FOR COMSCORE                      
         B     DEM2                                                             
         SPACE 1                                                                
* EXITS FROM MODULE                                                             
*                                                                               
DEMERR   MVC   AINPUT,FADR         SET A(TWA FIELD)                             
         MVC   NINPUT,FNDX         SET N'FIELD IN ERROR                         
         CLI   NINPUT,0            TEST FIELD INDEX OF ZERO                     
         BNE   *+8                                                              
         MVI   NINPUT,1            YES - SET FIRST SUBFIELD AS INVALID          
         MVI   NOUTPUT,0           SET N'OUTPUT ENTRIES TO ZERO                 
*                                  RETURN TO CALLER                             
         OC    AOUTPUT,AOUTPUT                                                  
         JZ    *+14                                                             
         L     RE,AOUTPUT          RETURN INVALID DEMO CATEGORY                 
         MVC   0(7,RE),WRNSTRNG                                                 
         ANSR                                                                   
         EJECT                                                                  
* VALIDATE DEMO EXPRESSIONS.                                                    
*                                                                               
DEM2     LLC   RE,NFLDS            DECREMENT TWA FIELD COUNT                    
         SHI   RE,1                                                             
         BM    DEM90               EXIT IF ALL FIELDS PROCESSED                 
         STC   RE,NFLDS                                                         
         L     R1,NADR             FIND NEXT TWA FIELD HEADER                   
         B     *+6                                                              
DEM3     AR    R1,RE                                                            
         IC    RE,0(R1)                                                         
         TM    1(R1),X'20'         IGNORE PROTS                                 
         BNZ   DEM3                                                             
         ST    R1,FADR             SET CURRENT FIELD ADDRESS                    
         LA    RF,0(R1,RE)                                                      
         ST    RF,NADR             SET NEXT FIELD ADDRESS                       
         MVI   FNDX,0                                                           
         SHI   RE,8                ADJUST FOR FLDHDR                            
         TM    1(R1),X'02'         TEST FOR EXTENSION                           
         BZ    *+12                                                             
         SHI   RE,8                ADJUST FOR EXTENSION                         
         SHI   RF,8                ADJUST FOR EXTENSION                         
         BCTR  RF,0                                                             
         CLI   0(RF),X'40'                                                      
         BH    *+12                                                             
         BCT   RE,*-10                                                          
         B     DEM2                EXIT IF NO INPUT IN FIELD                    
*                                                                               
         STC   RE,5(R1)            SET ACTUAL INPUT LENGTH                      
*                                  BUILD SCANNER BLOCK FOR THIS FIELD           
         LA    R0,SCANMAX                                                       
         MVC   DMCB+08(2),=C',='                                                
         MVC   DMCB+10(1),SCANCHR                                               
         MVI   DMCB+11,X'FF'                                                    
         GOTO1 CSCANNER,DMCB,FADR,((R0),SCANTAB)                                
         CLI   4(R1),0                                                          
         BE    DEMERR                                                           
         LLC   RF,4(R1)            DECREMENT MAX OUTPUT COUNT                   
         LLC   RE,NLIST                                                         
         CR    RF,RE               TEST INPUT GR MAXIMUM                        
         BNH   *+12                                                             
         STC   RF,FNDX                                                          
         B     DEMERR              YES - ERROR                                  
*                                                                               
         SR    RE,RF                                                            
         STC   RE,NLIST            SET REMAINDER                                
         STC   RF,NLINES                                                        
         LA    R4,SCANTAB-L'SCANTAB                                             
*                                  PROCESS SCANNER BLOCK ENTRIES                
DEM4     LA    R4,L'SCANTAB(R4)    BUMP TO NEXT (OR FIRST) ENTRY                
         LLC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         CLM   R1,1,NLINES         TEST ALL ENTRIES PROCESSED                   
         BH    DEM2                GO TO NEXT TWA FIELD                         
*                                                                               
         STC   R1,FNDX             SET CURRENT FIELD NUMBER                     
         CLI   0(R4),0             TEST FIELD LENGTHS                           
         BE    DEMERR                                                           
         CLI   1(R4),0                                                          
         BNE   DEMERR                                                           
         IC    R1,0(R4)            EXTRACT INPUT INTO FLD & SET FLDLN           
         STC   R1,FLDLN                                                         
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),12(R4)                                                    
         MVC   WRNSTRNG,FLD        FOR POSSIBLE WARNING MESSAGE                 
         XC    OVALS(OVALLN),OVALS                                              
         MVI   OCAT,0                                                           
*                                                                               
         CLI   IOPTN,C'Y'          TEST IF CPP/CPM VALIDATION REQUIRED          
         BNE   DEM20                                                            
*                                  VALIDATE FOR PRIME                           
         LLC   R1,FLDLN                                                         
         LA    RE,FLD-1(R1)                                                     
         CLI   0(RE),C'P'          TEST FIELD ENDS WITH P (PRIME)               
         BNE   *+14                                                             
         OI    OSTATUS,X'40'       YES - SET STATUS BIT                         
         MVI   0(RE),C' '                                                       
         BCTR  R1,0                                                             
*                                                                               
         STC   R1,FLDLN                                                         
*                                  VALIDATE CPP/CPM OPTIONS                     
DEM8     CLI   FLD,C'-'            TEST FIELD STARTS WITH -                     
         BNE   DEM10                                                            
         TM    OSTATUS,X'80'                                                    
         BNZ   DEMERR                                                           
         OI    OSTATUS,X'80'       YES - SET STATUS BIT                         
         B     DEM12                                                            
*                                                                               
DEM10    CLI   FLD,C'$'            TEST FIELD STARTS WITH $                     
         BNE   DEM30                                                            
         TM    OSTATUS,X'20'                                                    
         BNZ   DEMERR                                                           
         OI    OSTATUS,X'20'       YES - SET STATUS BIT                         
*                                                                               
DEM12    BAS   RE,REMOVE           REMOVE - OR $                                
         BZ    DEMERR                                                           
         B     DEM8                                                             
*                                                                               
DEM20    CLI   DBSELMED,C'T'       TV ONLY?                                     
         JE    DEM20A                                                           
*        CLI   DBSELMED,C'N'       NET?                                         
         CLI   SVOVSYS,NETQ        NET?                                         
         JNE   DEM28                                                            
DEM20A   CLI   FLD,C'X'            COMSCORE?                                    
         BE    *+12                                                             
         CLI   FLD+1,C'X'                                                       
         BNE   DEM28                                                            
*                                                                               
         CLI   FLD,DEMO_MODIFIER_I IMPRESSIONS?  SUPPRESS THE C'I'              
         JNE   *+14                                                             
         MVC   FLD(L'FLD-1),FLD+1                                               
         MVI   FLD+L'FLD-1,C' '                                                 
*                                                                               
         MVC   BYTE,FLD            SAVE MODIFIER                                
*                                                                               
         OC    ARTKTAB,ARTKTAB     HAVE A(COMSCORE TABLE)?                      
         JNZ   DEM20C                                                           
         OC    ARTKTABN,ARTKTABN   HAVE A(COMSCORE TABLE)?                      
         JNZ   DEM20C                                                           
         BRAS  RE,GETTABS                                                       
*                                                                               
DEM20C   MVI   0(R8),0                                                          
         MVI   2(R8),0                                                          
*                                                                               
         CLI   BYTE,C'X'           ANY MODIFIER?                                
         JE    DEM21               NO                                           
         CLI   BYTE,DEMO_MODIFIER_B                                             
         JE    DEM21                                                            
         CLI   BYTE,DEMO_MODIFIER_I                                             
         JE    DEM21                                                            
         CLI   BYTE,DEMO_MODIFIER_R                                             
         JE    DEM21                                                            
         TM    PARAM5,X'40'             CALLER WANTS COMSCORE MODIFIER?         
         JZ    *+12                     NO                                      
         CLI   BYTE,DEMO_MODIFIER_M     YES - COMSCORE DEMO INDEX?              
         JE    DEM21                    YES                                     
         CLI   SVOVSYS,NETQ        NET?                                         
         JNE   DEMERR                                                           
         CLI   BYTE,DEMO_MODIFIER_G     COMSCORE - ARI                          
         JE    DEM21                                                            
         CLI   BYTE,DEMO_MODIFIER_J     COMSCORE - DEMO INDEX                   
         JE    DEM21                                                            
         CLI   BYTE,DEMO_MODIFIER_U                                             
         JE    DEM21                                                            
         CLI   BYTE,DEMO_MODIFIER_T     COMSCORE - HOURS                        
         JE    DEM21                                                            
         CLI   BYTE,DEMO_MODIFIER_K     COMSCORE - DEMO INDEX                   
         JNE   DEMERR                                                           
*                                                                               
DEM21    LA    R3,BPARAMS                                                       
         USING BSPARA,R3                                                        
*                                                                               
         MVI   DUB,C' '                                                         
         MVC   DUB+1(L'DUB-1),DUB                                               
         MVC   DUB(6),FLD+1        SKIP C'X'                                    
         CLI   BYTE,C'X'           ANY MODIFIER?                                
         JE    *+10                                                             
         MVC   DUB(6),FLD+2        YES - SKIP MODIFIER + C'X'                   
*                                                                               
         LA    RF,DUB                                                           
         STCM  RF,15,BSPAREC       A(INPUT FIELD)                               
*                                                                               
         ICM   RE,15,ARTKTAB       A(RENTRAK TABLE)                             
*        CLI   DBSELMED,C'N'       NET?                                         
         CLI   SVOVSYS,NETQ        NET?                                         
         JNE   *+8                                                              
         ICM   RE,15,ARTKTABN      A(RENTRAK TABLE)                             
*                                                                               
         LAM   ARE,ARE,ALET                                                     
         SAC   512                                                              
         MVC   BSPSTRT(24),BSPSTRT-BSPAREC(RE)   BINSRCH PARAMETERS             
         DROP  R3                                                               
*                                                                               
         GOTO1 VBINSRCH,BPARAMS                                                 
         IF  (CLI,BPARAMS,NE,0)    DEMO NOT FOUND?                              
           IF  (TM,PARAM5,X'80',NZ) PARAM5,BYTE 0 HAS X'80' ON?                 
             L     RE,AEXTBLK                                                   
             USING P5XD,RE                                                      
             CLC   P5XID,=C'P5X '     PARAM 5 EXTENDED BLOCK?                   
             JNE   DEM21A                                                       
             OC    P5XLICNS,P5XLICNS  WAS A(COMSCORE LICENSE) PASSED?           
             JZ    DEMERR             NO                                        
             L     R3,P5XLICNS        YES, CHECK IF WE GOT LICENSE              
             CLC   0(L'CSFLIC,R3),=CL32' '                                      
             JNH   DEMERR                  NO, BLANKS OR WORSE                  
*                                                                               
             XC    KEY,KEY            LOOK FOR LICENSE IN GENFIL                
             LA    RE,KEY                                                       
             USING CSLKEY,RE          X'00'                                     
             MVI   CSLKMIN,CSLKMIQ    C'D' - DEMO SYSTEM                        
             MVI   CSLKREC,CSLKREQ    C'L' - LICENSE RECORD                     
             MVC   CSLKLIC,0(R3)      FIRST 10 CHARS OF LICENSE                 
             DROP  RE                                                           
*                                                                               
             MVC   KEYSAVE,KEY                                                  
             GOTO1 CDATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,KEY              
****         CLC   KEY(CSLKEYL),KEYSAVE                                         
             CLC   KEY(CSLKLIC+L'CSLKLIC-CSLKEY),KEYSAVE                        
             JNE   DEMERR             LICENSE IS NOT IN GENFIL                  
*************                                                                   
* WE'RE CHECKING ONLY ONE PHYSICAL RECORD FOR NOW, BUT THERE COULD BE           
* MORE LICENSE RECORDS FOR THE SAME FIRST 10 CHARACTERS                         
*************                                                                   
             GOTO1 CDATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+36,IOAREA,  +        
               DMWORK                                                           
             JNE   DEMERR                                                       
             LA    RE,IOAREA                                                    
             USING CSLKEY,RE                                                    
             XR    R1,R1                                                        
             ICM   R1,3,CSLRLEN       RECORD LENGTH                             
             LA    RE,CSLFIRST(RE)    POINT TO THE FIRST ELEMENT                
             SHI   R1,CSLFIRST                                                  
             DROP  RE                                                           
*                                                                               
             DO  INF                                                            
               IF  (CLI,0(RE),EQ,0)     END OF RECORD?                          
                  J   DEMERR            LICENSE IS NOT IN GENFIL                
               ELSE ,                                                           
                 IF  (CLI,0(RE),EQ,CSFLELQ)   X'0F'-LICENSE ELEM                
                   USING CSFLD,RE                                               
                   IF  (CLC,CSFLIC,EQ,0(R3))  MATCH ON LICENSE?                 
                     MVC  HALF,CSFLSEQ        YES, COPY THE SEQ #               
                     ASMLEAVE ,               DONE WITH THIS REC                
                   ENDIF ,                                                      
                   DROP  RE                                                     
                 ENDIF ,                                                        
                 LLC R0,1(RE)           BUMP TO NEXT ELEM                       
                 AR  RE,R0                                                      
*                                                                               
                 SR  R1,R0              BOUNDARY CHECK FOR EOR                  
                 JNP DEMERR             LICENSE NOT IN GENFIL                   
               ENDIF ,                                                          
             ENDDO ,                                                            
*                                                                               
             XC    KEY,KEY            LOOK FOR LICENSE/OXCODE IN GENFIL         
             LA    RE,KEY                                                       
             USING LDCKEY,RE          X'00'                                     
             MVI   LDCKMIN,LDCKMIQ    C'D' - DEMO SYSTEM                        
             MVI   LDCKREC,LDCKREQ    C'C' - CATEGORY RECORD                    
             MVC   LDCKLIC,0(R3)      1ST 10 CHARACTERS OF LICENSE              
             MVC   LDCKSEQ,HALF       LICENSE SEQ #                             
             MVC   LDCKOXC,DUB        OXCODE FROM ABOVE BINSRCH                 
             DROP  RE                                                           
             MVC   KEYSAVE,KEY                                                  
             GOTO1 CDATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,KEY              
             CLC   KEY(LDCKEYL),KEYSAVE                                         
             JNE   DEMERR            NEITHER IN TABSDEM NOR CUSTOM DEMO         
*                                                                               
             XC    KEY,KEY            NOW LOOK FOR THE OXCODE IN GENFIL         
             LA    RE,KEY                                                       
             USING CDCKEY,RE          X'00'                                     
             MVI   CDCKMIN,CDCKMIQ    C'D' - DEMO SYSTEM                        
             MVI   CDCKREC,CDCKREQ    C'C' - CATEGORY RECORD                    
             MVC   CDCKOXC,DUB        OXCODE FROM ABOVE BINSRCH                 
             DROP  RE                                                           
             MVC   KEYSAVE,KEY                                                  
             GOTO1 CDATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,KEY              
             CLC   KEY(CDCKEYL),KEYSAVE                                         
             JNE   DEMERR            NEITHER IN TABSDEM NOR CUSTOM DEMO         
*                                                                               
             GOTO1 CDATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+36,IOAREA,  +        
               DMWORK                                                           
             JNE   DEMERR            NEITHER IN TABSDEM NOR CUSTOM DEMO         
             LA    RE,IOAREA                                                    
             USING CDCKEY,RE          X'00'                                     
             XR    R1,R1                                                        
             ICM   R1,3,CDCRLEN      RECORD LENGTH                              
             LA    RE,CDCFIRST(RE)   POINT TO THE FIRST ELEMENT                 
             SHI   R1,CDCFIRST                                                  
             DROP  RE                                                           
*                                                                               
             DO  INF                                                            
               IF  (CLI,0(RE),EQ,0)  END OF RECORD?                             
                  J   DEMERR         DON'T KNOW IF LOCAL OR NAT'L               
               ELSE ,                                                           
                 IF  (CLI,0(RE),EQ,CDCELQ)   X'0A'-CUSTOM DEMO ELEM             
                   USING CDCELD,RE                                              
                   IF  (CLI,SVOVSYS,EQ,NETQ)  NAT'L REQUESTED?                  
                     IF  (TM,CDCFLAG,CDCFNAT,Z)  BUT NAT'L NOT ON?              
                        J   DEMERR               THEN ERROR                     
                     ENDIF                                                      
                   ELSE ,                     NO, THEN LOCAL                    
                     IF  (TM,CDCFLAG,CDCFLOC,Z)  BUT LOCAL NOT ON?              
                        J   DEMERR               THEN ERROR                     
                     ENDIF                                                      
                   ENDIF ,                                                      
                   ASMLEAVE ,               DONE WITH THIS REC                  
                   DROP  RE                                                     
                 ENDIF ,                                                        
                 LLC R0,1(RE)           BUMP TO NEXT ELEM                       
                 AR  RE,R0                                                      
*                                                                               
                 SR  R1,R0              BOUNDARY CHECK FOR EOR                  
                 JNP DEMERR             DON'T KNOW IF LOCAL OR NAT'L            
               ENDIF ,                                                          
             ENDDO ,                                                            
           ELSE ,                                                               
             J   DEMERR            NO, DON'T HAVE A COMSCORE LICENSE            
           ENDIF                                                                
         ENDIF                                                                  
*                                                                               
DEM21A   OI    DEMOFLAG,DEMONTDQ   SET NON-TRAD PROCESSED FLAG                  
*                                                                               
         IF  (TM,PARAM5,X'80',NZ)  PARAM5, BYTE 0 HAS X'80' ON?                 
           L     RE,AEXTBLK                                                     
           USING P5XD,RE                                                        
           CLC   P5XID,=C'P5X '    PARAM 5 EXTENDED BLOCK?                      
           JNE   DEM21AA                                                        
           OC    P5XINNTD,P5XINNTD USE INPUT NON-TRAD DEMO LIST                 
           JNZ   DEM22             TO GET INDEX #?                              
           DROP  RE                                                             
         ELSE ,                    PARAM 5, BYTE 0 EQUAL TO X'00'?              
DEM21AA    OC    AINNTDM,AINNTDM   USE INPUT NON-TRAD DEMO LIST                 
           JNZ   DEM22             TO GET INDEX #?                              
         ENDIF                                                                  
*                                                                               
         CLI   DBDEMTYP,C'4'       4 BYTE DEMO CATEGORIES?                      
         JNE   DEM21B                                                           
         MVC   3(1,R8),DEMOINDX    NO - BUILD FROM SCRATCH                      
         AHI   R8,4                                                             
         J     DEM21C                                                           
*                                                                               
DEM21B   MVC   1(1,R8),DEMOINDX    NO - BUILD FROM SCRATCH                      
         AHI   R8,3                                                             
*                                                                               
DEM21C   LLC   RF,DEMOINDX         INCREMENT INDEX #                            
         AHI   RF,1                                                             
         STC   RF,DEMOINDX                                                      
*                                                                               
         OC    AOUTNTDM,AOUTNTDM                                                
         JZ    DEMERR                                                           
*                                                                               
         L     RF,AOUTNTDM         OUTPUT NON-TRAD DEMO CATEGORIES              
         MVC   0(8,RF),FLD                                                      
         AHI   RF,8                                                             
         ST    RF,AOUTNTDM                                                      
         B     DEM60                                                            
*                                                                               
* HAVE INPUT NON-TRAD DEMO LIST TO VALIDATE FROM (PARAM 5)                      
*                                                                               
DEM22    DS    0H                                                               
         IF  (TM,PARAM5,X'80',NZ)  PARAM5, BYTE 0 HAS X'80' ON?                 
           L     RE,AEXTBLK                                                     
           USING P5XD,RE                                                        
           CLC   P5XID,=C'P5X '    PARAM 5 EXTENDED BLOCK?                      
           JNE   DEM22A                                                         
           L     RF,P5XINNTD       A(INPUT NON-TRAD DEMO LIST)                  
           DROP  RE                                                             
         ELSE ,                    PARAM 5, BYTE 0 EQUAL TO X'00'?              
DEM22A     L     RF,AINNTDM          A(INPUT NON-TRAD DEMO LIST)                
         ENDIF                                                                  
         LA    RE,50                                                            
         MVI   DEMOINDX,1                                                       
*                                                                               
DEM24    CLC   0(8,RF),FLD         IN INPUT NON-TRAD DEMO LIST?                 
         JE    DEM26                                                            
*                                                                               
         AHI   RF,8                NOT YET - BUMP TO NEXT ENTRY IN LIST         
         LLC   R1,DEMOINDX         INCREMENT INDEX #                            
         AHI   R1,1                                                             
         STC   R1,DEMOINDX                                                      
         BCT   RE,DEM24                                                         
                                                                                
* NONT DEMO NOT IN INPUT LIST                                                   
                                                                                
         MVI   NEWNONT,C'Y'                                                     
         CLI   PASS,2              INSERT ON PASS 2 ONLY                        
         JE    *+12                                                             
         LA    R8,3(R8)            BUT LEAVE A SLOT FOR IT                      
         J     DEM60                                                            
*                                                                               
         OC    AOUTNTDM,AOUTNTDM                                                
         JZ    DEMERR                                                           
*                                                                               
         L     RF,AOUTNTDM         NOT IN LIST MEANS BRAND NEW DEMO             
         LA    RE,50               ADD IT TO OUPUT DEMO INDEX LIST              
         MVI   DEMOINDX,1          IN FIRST AVAILABLE SLOT                      
*                                                                               
DEM25    OC    0(8,RF),0(RF)       FIND AN EMPTY NONT DEMO SLOT                 
         JZ    DEM26                                                            
*                                                                               
         LA    RF,8(RF)            BUMPT TO NEXT                                
         LLC   R1,DEMOINDX         INCREMENT INDEX #                            
         AHI   R1,1                                                             
         STC   R1,DEMOINDX                                                      
         BCT   RE,DEM25                                                         
         B     DEMERR              LIST FULL                                    
*                                                                               
DEM26    MVC   1(1,R8),DEMOINDX    SET INDEX #                                  
         LA    R8,3(R8)                                                         
*                                                                               
         L     RF,AOUTNTDM         PUT NON-TRAD DEMO IN OUTPUT INDEX            
         LLC   RE,DEMOINDX                                                      
         BCTR  RE,0                                                             
         SLL   RE,3                X8                                           
         AR    RF,RE               BUMP TO INDEXED POSITION                     
         MVC   0(8,RF),FLD                                                      
         B     DEM60                                                            
*                                                                               
DEM28    CLI   IOPTN,C'S'          TEST SPOT VALIDATION                         
         BNE   DEM30                                                            
         CLC   =C'W/',FLD          TEST SPOT WEIGHTED DEMO                      
         BE    DEMWGT                                                           
         LA    RE,USNTAB           POINT TO USER NAME PREFIX TABLE              
         LA    R0,USNAMES          COUNTER                                      
         CLC   0(L'USNTAB,RE),FLD  TEST SPOT USER DEMO                          
         BE    DEMUSR                                                           
         LA    RE,L'USNTAB(RE)     NEXT TABLE ENTRY                             
         BCT   R0,*-14                                                          
*                                                                               
DEM30    DS    0H                                                               
         BRAS  RE,RMPFX            REMOVE THE CATEGORY PREFIX                   
         CLI   PREFIXLN,0                                                       
         BE    *+12                                                             
         BRAS  RE,CATVAL                                                        
         BZ    DEMERR              ERROR IN PREFIX                              
*                                                                               
         CLI   IOPTN,C'C'          VALIDATE CATEGORIES ONLY                     
         BE    DEM42                                                            
*                                                                               
         CLI   IOPTN,C'Y'          TEST IF CPP/CPM VALIDATION REQUIRED          
         BNE   *+12                                                             
         CLI   PREFIXLN,0          CANNOT HAVE PREFIXES WITH THIS OPT.          
         BNE   DEMERR                                                           
*                                  VALIDATE SPECIALS                            
         CLI   FLD,C'='            = MEANS SUPPRESS MODIFIER VALIDATION         
         BNE   *+16                                                             
         OI    OFLAG,OFLAG_SM                                                   
         BAS   RE,REMOVE                                                        
         BZ    DEMERR                                                           
*                                                                               
         CLI   FLD,C'*'            * MEANS TAPE PRECISION DEMO VALUE            
         BNE   *+16                                                             
         OI    OFLAG,OFLAG_TP                                                   
         BAS   RE,REMOVE                                                        
         BZ    DEMERR                                                           
         EJECT                                                                  
*                                  SPOT DEMO NUMBER #N(NN)                      
         CLI   FLD,C'#'                                                         
         BNE   DEM33                                                            
         TM    OFLAG,OFLAG_SM      CAN'T HAVE = PREFIX                          
         BNZ   DEMERR                                                           
         MVI   BYTE,C'4'           WARNING #                                    
*        BAS   RE,SNDMAIL          WARN DEIS                                    
         BAS   RE,REMOVE                                                        
         BZ    DEMERR                                                           
         B     DEM33A                                                           
*                                                                               
DEM33    TM    FLD,X'F0'           TEST NUMERIC                                 
         BNO   DEM35               NO                                           
         CLI   IOPTN,C'S'          TEST DEFAULT TO SPOT NUMBERS                 
         BNE   DEM34                                                            
*                                                                               
DEM33A   LA    R1,FLD              GET NUMERIC VALUE                            
         LLC   R0,FLDLN                                                         
         BAS   RE,NUMERIC                                                       
         BE    DEMERR                                                           
         TM    DUB,X'80'           CAN BE BETWEEN 1 AND 127                     
         BNZ   DEMERR                                                           
         LLC   R1,DUB                                                           
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    RE,SPDEMTAB                                                      
         TM    DBVOPT,X'80'        TEST USE U.S. CATEGORIES                     
         BO    DEM33B                                                           
         CLI   DBSELMED,C'C'       TEST CANADIAN                                
         BNE   DEM33B                                                           
         LARL  RE,CNDEMTAB                                                      
*                                                                               
DEM33B   AR    R1,RE               INDEX INTO APPROPRIATE TABLE                 
         CLC   0(2,R1),=H'0'                                                    
         BE    DEMERR                                                           
         MVC   OMODDEMO,0(R1)      SET MODIFIER/DEMO                            
         B     DEM38                                                            
*                                  TSA SHARE N(NN)                              
DEM34    MVI   OMOD,DEMO_MODIFIER_T SET MODIFIER TO 'T' FOR REP IMPS            
*                                                                               
DEM34A   LA    R1,FLD              GET NUMERIC VALUE                            
         LLC   R0,FLDLN                                                         
         BAS   RE,NUMERIC                                                       
         BE    DEMERR                                                           
*                                                                               
         MVC   ODEMO,DUB           SET DEMO                                     
         CLI   FLAVOR,X'00'        LEAVE OLD STYLE ALONE                        
         BE    DEM38                                                            
*                                                                               
         LAY   RE,EQU2TO3                                                       
DEM34B   CLC   ODEMO,0(RE)         CONVERT TO NEW STYLE                         
         BE    DEM34C                                                           
         LA    RE,3(RE)                                                         
         CLI   0(RE),0                                                          
         BE    DEMERR                                                           
         B     DEM34B                                                           
DEM34C   MVC   OSEX2,1(RE)                                                      
         MVC   ODEMO,2(RE)                                                      
         B     DEM38                                                            
*        B     DEM38               <----------OLD                               
*                                  TEST FOR SEX/AGE (NO MODIFIER)               
DEM35    MVC   SVFLD,FLD           SAVE INPUT FIELD & LENGTH                    
         MVC   SVFLDLN,FLDLN                                                    
*                                  EXTRACT LEADING ALPHA DATA INTO W/S          
         LA    RF,WORK                                                          
         CLC   FLD(5),=C'YCU12'                                                 
         BNE   *+14                                                             
         MVC   FLD+3(2),=C'A '                                                  
         MVI   FLDLN,4                                                          
         CLC   FLD+1(5),=C'YCU12'                                               
         BNE   *+14                                                             
         MVC   FLD+4(2),=C'A '                                                  
         MVI   FLDLN,5                                                          
         CLC   FLD(5),=C'YCU18'                                                 
         BNE   *+14                                                             
         MVC   FLD+3(2),=C'B '                                                  
         MVI   FLDLN,4                                                          
         CLC   FLD+1(5),=C'YCU18'                                               
         BNE   *+14                                                             
         MVC   FLD+4(2),=C'B '                                                  
         MVI   FLDLN,5                                                          
*                                                                               
         BAS   RE,FIXFM914         FIX FE9-14 PROBLEM                           
*                                                                               
         MVC   SVFLD,FLD           SAVE INPUT FIELD & LENGTH                    
         MVC   SVFLDLN,FLDLN                                                    
DEM35A   CLI   FLD,C'A'                                                         
         BL    DEM35B                                                           
         CLI   FLD,C'Z'                                                         
         BH    DEM35B                                                           
         MVC   0(1,RF),FLD                                                      
         BAS   RE,REMOVE                                                        
         LA    RF,1(RF)                                                         
         B     DEM35A                                                           
*                                                                               
DEM35B   LA    RE,WORK                                                          
         SR    RF,RE               RF=L'SEX EXPRESSION                          
         BZ    DEMERR                                                           
         SHI   RF,1                TEST FOR 1 BYTE ALPHA DATA                   
         BP    DEM35C                                                           
         SR    R0,R0                                                            
         ICM   R0,1,FLDLN          TEST IF ANY MORE DATA IN FIELD               
         BZ    DEM35C                                                           
         LA    R1,FLD                                                           
         BAS   RE,NUMERIC          TEST IF NUMERIC                              
         BNE   DEM35E              YES - ASSUME MODIFIER/DEMO                   
*                                  LOOK-UP ALPHA DATA IN SEX TABLE              
DEM35C   L     RE,ASEXTAB                                                       
         CHI   RF,1                2 CHAR INPUT                                 
         BNE   DEM35D                                                           
         CLC   WORK(2),=C'NO'      STOP NO FROM VALIDATING                      
         BE    DEMERR               AS NONWWRK                                  
DEM35D   CLI   0(RE),FF            TEST E-O-T                                   
         BE    DEM35E                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),WORK                                                     
         BE    *+12                                                             
         LA    RE,L'SEXTAB(RE)                                                  
         B     DEM35D                                                           
*                                                                               
         MVI   OMOD,DEMO_MODIFIER_I SEX IS VALID - SET DEFAULT MODIFIER         
         CLI   DBSELMED,C'R'       RADIO DEFAULTS TO I                          
         BE    *+8                                                              
         CLI   IOPTN,C'S'          AND SPOT DOES ALSO                           
         BE    *+8                                                              
         MVI   OMOD,DEMO_MODIFIER_T                                             
         B     DEM36H                                                           
*                                  RESTORE SAVED FIELD VALUES                   
DEM35E   MVC   FLD,SVFLD                                                        
         MVC   FLDLN,SVFLDLN                                                    
*                                  OTHER FORMATS PREFIXED BY MODIFIER           
         CLI   FLD,C'A'                                                         
         BL    DEMERR                                                           
         CLI   FLD,C'Z'                                                         
         BH    DEMERR                                                           
         MVC   OMOD,FLD                                                         
         CLI   OMOD,DEMO_MODIFIER_T WHEN THE USER INPUTS 'T' MODIFIER,          
         BNE   *+8                   CHANGE IT TO 'X' FOR TSA SHARES            
         MVI   OMOD,DEMO_MODIFIER_X                                             
         BAS   RE,REMOVE                                                        
         BZ    DEMERR                                                           
*                                                                               
DEM36A   TM    FLD,X'F0'           TEST FOR DEMO NUMBER                         
         BO    DEM34A                                                           
*                                  EXTRACT SEX FROM EXPRESSION INTO DUB         
         LA    RF,DUB                                                           
         LLC   R0,FLDLN                                                         
DEM36C   CLI   FLD,C'A'                                                         
         BL    DEM36E                                                           
         CLI   FLD,C'Z'                                                         
         BH    DEM36E                                                           
         MVC   0(1,RF),FLD                                                      
         BAS   RE,REMOVE                                                        
         LA    RF,1(RF)                                                         
         BCT   R0,DEM36C                                                        
DEM36E   LA    RE,DUB                                                           
         SR    RF,RE               RF=L'SEX                                     
         BZ    DEMERR                                                           
         BCTR  RF,0                                                             
*                                  LOOK-UP SEX IN TABLE                         
         L     RE,ASEXTAB          RE=A(SEXTAB)                                 
         CHI   RF,1                2 CHAR INPUT                                 
         BNE   DEM36G                                                           
         CLC   DUB(2),=C'NO'       STOP NO FROM VALIDATING                      
         BE    DEMERR                AS NONWWRK                                 
DEM36G   CLI   0(RE),FF            TEST E-O-T                                   
         BE    DEMERR                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),DUB         COMPARE INPUT WITH TABLE                     
         BE    *+12                                                             
         LA    RE,L'SEXTAB(RE)                                                  
         B     DEM36G                                                           
*                                                                               
DEM36H   MVC   OSEX,9(RE)          SET SEX VALUES FROM TABLE                    
         MVC   OSEX2,10(RE)                                                     
         MVC   ORANGE,6(RE)                                                     
         MVC   ODEMO,8(RE)                                                      
         OC    ORANGE,ORANGE       TEST SPECIALS (HOMES, METRO ETC.)            
         BNZ   *+16                                                             
         CLI   FLDLN,0             YES - FIELD MUST BE EMPTY                    
         BNE   DEMERR                                                           
         B     DEM39                                                            
         CLI   FLDLN,0             TEST FIELD EMPTY (NO AGE IN FIELD)           
         BNE   *+14                                                             
         MVC   OAGE,ORANGE         YES - ASSUME TOTAL RANGE                     
         B     DEM360                                                           
*                                                                               
         CLI   FLAVOR,0            NEW TYPES SKIP '+' FORCE                     
         BNE   DEM36H1                                                          
         CLI   OSEX,WW+VW          THESE ONLY HAVE A START AGE                  
         BNE   DEM36H1                                                          
         LLC   R1,FLDLN            ADD A '+' TO FORCE THRU EDIT                 
         LA    RE,FLD(R1)                                                       
         MVI   0(RE),C'+'                                                       
         LA    R1,1(R1)                                                         
         STC   R1,FLDLN                                                         
*                                                                               
DEM36H1  CLI   FLDLN,1                                                          
         BNH   DEMERR                                                           
         LLC   R1,FLDLN            TEST FIELD SUFFIXED BY + (IE WM18+)          
         LA    RE,FLD-1(R1)                                                     
         CLI   0(RE),C'+'                                                       
         BNE   DEM36I                                                           
         CLI   ORANGE+1,X'FF'      MUST BE UNDEFINED                            
         BNE   DEMERR                                                           
         MVC   OAGE+1(1),ORANGE+1  YES - SET END AGE EQ MAXIUM AGE              
         MVI   0(RE),C' '                                                       
         BCTR  R1,0                                                             
         STC   R1,FLDLN                                                         
         CLI   FLDLN,2                                                          
         BH    DEMERR                                                           
         LA    R1,FLD              GET START AGE                                
         LLC   R0,FLDLN                                                         
         BAS   RE,NUMERIC                                                       
         BE    DEMERR                                                           
         MVC   OAGE(1),DUB         SET START AGE                                
         B     DEM360                                                           
*                                  VALIDATE START & END AGES                    
DEM36I   LA    RE,FLD+1            MAY HAVE EMBEDDED - (IE WM18-49)             
         CLI   FLDLN,4                                                          
         BNH   DEM36K                                                           
         LA    RE,FLD+2                                                         
         CLI   FLDLN,5                                                          
         BE    DEM36K                                                           
         BH    DEMERR                                                           
         B     DEM36M                                                           
DEM36K   CLI   0(RE),C'-'          TEST FOR EMBEDDED -                          
         BNE   DEM36M                                                           
         MVC   0(4,RE),1(RE)       YES - REMOVE                                 
         LLC   R1,FLDLN                                                         
         BCTR  R1,0                                                             
         STC   R1,FLDLN                                                         
*                                  SET L'START AGE (RE) & END AGE (RF)          
DEM36M   LLC   RF,FLDLN                                                         
         SR    RE,RE                                                            
         LA    RE,1(RE)                                                         
         BCTR  RF,0                                                             
         CHI   RF,2                                                             
         BH    *-10                                                             
         LA    R1,FLD              GET BINARY START & END AGES                  
         LR    R0,RE                                                            
         BAS   RE,NUMERIC                                                       
         BE    DEMERR                                                           
         MVC   OAGE(1),DUB         SET START AGE                                
         AR    R1,R0                                                            
         LR    R0,RF                                                            
         BAS   RE,NUMERIC                                                       
         BE    DEMERR                                                           
         MVC   OAGE+1(1),DUB       SET END AGE                                  
*                                                                               
DEM360   CLC   OAGE(1),OAGE+1      TEST START GR END                            
         BH    DEMERR                                                           
         CLC   OAGE(1),ORANGE      TEST START & END WITHIN LIMITS               
         BL    DEMERR                                                           
         CLC   OAGE+1(1),ORANGE+1                                               
         BH    DEMERR                                                           
*                                  LOOK-UP AGE IN TABLE                         
         L     RE,AAGETAB          RE=A(AGETAB)                                 
DEM36S   CLI   0(RE),FF            TEST E-O-T                                   
         BE    DEMERR                                                           
         CLC   OAGE,0(RE)                                                       
         BE    *+12                                                             
DEM36T   LA    RE,L'AGETAB(RE)                                                  
         B     DEM36S                                                           
         IC    R1,OSEX             TEST SEX IS VALID FOR AGE                    
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    3(RE),0                                                          
         BNO   DEM36T                                                           
         LLC   R1,2(RE)                                                         
         LLC   R0,ODEMO                                                         
         AR    R1,R0               YES - ADD DEMO TO BASE DEMO NUMBER           
         STC   R1,ODEMO                                                         
         B     DEM39                                                            
*                                  VALIDATE DEMO NUMBER                         
DEM38    L     RE,ASEXTAB          RE=A(SEXTAB)                                 
         XC    DUB(3),DUB          DUB=LAST BASE/AGE                            
DEM38A   CLI   0(RE),FF            TEST E-O-T                                   
         BE    DEMERR                                                           
         LLC   R0,8(RE)            R0=DEMO NUMBER/BASE DEMO NUMBER              
         CLC   DUB(3),6(RE)        TEST SAME AS LAST BASE/AGE                   
         BE    DEM38G                                                           
         CLC   6(2,RE),=H'0'       TEST FOR ACTUAL DEMO NUMBER                  
         BNE   *+16                                                             
         CLM   R0,1,ODEMO          YES - TEST EQ INPUT DEMO NUMBER              
         BE    DEM39               YES - GOT IT                                 
         B     DEM38G              NO - IGNORE                                  
*                                  LOOK-UP AGE TABLE FOR SEX                    
         MVC   DUB(3),6(RE)        SET LAST BASE/AGE                            
         L     RF,AAGETAB          RF=A(AGETAB)                                 
DEM38C   CLI   0(RF),FF            TEST E-O-T                                   
         BE    DEM38G                                                           
         LLC   R1,9(RE)            R1=SEX CATEGORY                              
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    3(RF),0             TEST AGE VALID FOR SEX CATEGORY              
         BNO   DEM38E                                                           
         IC    R1,2(RF)            R1=INDEX DEMO NUMBER                         
         AR    R1,R0               R1=ABSOLUTE DEMO NUMBER                      
         CLM   R1,1,ODEMO          TEST EQ INPUT DEMO NUMBER                    
         BE    DEM39                                                            
*                                  BUMP TO NEXT AGE TABLE ENTRY                 
DEM38E   LA    RF,L'AGETAB(RF)                                                  
         B     DEM38C                                                           
*                                  BUMP TO NEXT SEX TABLE ENTRY                 
DEM38G   LA    RE,L'SEXTAB(RE)                                                  
         B     DEM38A                                                           
*                                  VALIDATE MODIFIER FOR FILE/SUB-FILE          
DEM39    TM    OFLAG,OFLAG_SM      TEST FOR MODIFIER OVERRIDE                   
         BNZ   DEM42                                                            
         OC    INTFIL,INTFIL       FIRST TIME?                                  
         BNZ   DEM40D                                                           
         BRAS  RE,GETTABS                                                       
*&&DO                                                                           
         MVC   TABLIST,TABLISTL    GET A(REQUIRED TABLES)                       
         GOTO1 CDEMADDR,DMCB,(X'FF',TABLIST),COMFACSD                           
*                                                                               
         XC    DUB,DUB             SPECIAL CALL FOR SYSFACS                     
         MVC   DUB(4),=XL4'FEFFFFFF'                                            
         ICM   RF,15,CSWITCH                                                    
         BZ    DEM39A                                                           
         GOTO1 CSWITCH,DUB                                                      
         L     RF,0(R1)            RF=V(SYSFACS)                                
         L     RF,VSSB-SYSFACD(RF) RF=V(SSB)                                    
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
         B     DEM40                                                            
*                                                                               
DEM39A   L     RE,CMASTC             OFFLINE GET ADDR FROM MASTC                
         L     RF,MCSSB-MASTD(RE)    OBTAIN ALET FOR TABS DSPACE                
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
*&&                                                                             
DEM40    MVI   INTFIL,FF                                                        
         MVC   DUB(3),DBFILE       FIND INTERNAL FILE/SUBFILE                   
         MVC   DUB+3(1),DBSELMED                                                
         L     RE,AFMTAB           RE=A(FMTAB)                                  
         LAM   ARE,ARE,ALET                                                     
         SAC   512                                                              
DEM40A   CLI   0(RE),FF            TEST E-O-T                                   
         BNE   DEM40B                                                           
         SAC   0                                                                
         LAM   ARE,ARE,=F'0'                                                    
         B     DEM40D                                                           
*                                                                               
DEM40B   CLC   0(4,RE),DUB                                                      
         BE    *+12                                                             
         AHI   RE,6                                                             
         B     DEM40A                                                           
*                                                                               
         MVC   DUB(2),4(RE)        BUILD KEY FOR MODIFIER TABLE LOOK-UP         
         LAM   ARE,ARE,=F'0'                                                    
         MVC   DUB+2(3),=X'FFFFFF'                                              
*                                                                               
         L     R1,AMODTAB          FIND ENTRY IN MODIFIER TABLE                 
         LAM   AR1,AR1,ALET                                                     
         USING MODHDRD,R1          R1=A(MODTAB)                                 
DEM40C   CLC   MODFILE(2),=H'0'    E-O-T ?                                      
         BE    DEM40D              YES                                          
         CLC   MODHDRD(MODLDE-MODHDRD),DUB                                      
         BE    *+16                                                             
         ICM   R1,7,MODAET         BUMP TO NEXT TABLE ENTRY                     
         LA    R1,1(0,R1)                                                       
         B     DEM40C                                                           
*                                                                               
         XR    RE,RE               SET-UP BXLE REGISTERS                        
         ICM   RE,3,MODLDE                                                      
         XR    RF,RF                                                            
         ICM   RF,7,MODAET                                                      
         BCTR  RF,0                                                             
         AHI   R1,MODHDRLN                                                      
         ST    R1,MODBXLE                                                       
         STM   RE,RF,MODBXLE+4                                                  
         MVC   INTFIL,DUB          SET INTERNAL FILE/SUB-FILE                   
*                                                                               
*                                  LOOK-UP MODIFIER IN TABLE                    
DEM40D   SAC   0                                                                
         LAM   AR1,AR1,=F'0'                                                    
         CLI   INTFIL,FF           TEST IF TABLE FOUND                          
         BE    DEM42                                                            
         L     R1,MODBXLE          GET BXLE REGS                                
         LM    RE,RF,MODBXLE+4                                                  
         LAM   AR1,AR1,ALET                                                     
         SAC   512                                                              
         USING MODDTAD,R1                                                       
         CLC   MODMOD,OMOD                                                      
         BE    DEM40E                                                           
         BXLE  R1,RE,*-10                                                       
         SAC   0                                                                
         LAM   AR1,AR1,=F'0'                                                    
         B     DEMERR                                                           
*                                  TEST MODIFIER/DEMO IS VALID                  
DEM40E   MVC   DUB(2),INTFIL                                                    
         MVC   DUB+2(1),DBSELSRC   DUB=IFILE/IMED/SRC                           
         SAC   0                                                                
         LAM   AR1,AR1,=F'0'                                                    
*                                                                               
         TM    DBVOPT,X'80'        TEST USE U.S. CATEGORIES                     
         BO    DEM42                                                            
*                                                                               
         LARL  R1,DEMOTAB          R1=A(VALID DEMO TABLE)                       
         SR    RE,RE                                                            
DEM40G   CLC   0(2,R1),=H'0'       E-O-T ?                                      
         BE    DEM42               YES                                          
         CLC   2(3,R1),DUB         MATCH ON IFILE/IMED/SRC                      
         BE    *+14                                                             
         ICM   RE,3,0(R1)                                                       
         AR    R1,RE                                                            
         B     DEM40G                                                           
*                                                                               
         ICM   RE,3,0(R1)                                                       
         SHI   RE,5                                                             
         SRL   RE,1                RE=N'TABLE ENTRIES                           
         LA    R1,5(R1)            R1=A(FIRST VALID DEMO)                       
DEM40I   CLC   OMODDEMO,0(R1)      MATCH ON MODIFIER/DEMO                       
         BE    DEM42                                                            
         LA    R1,2(R1)                                                         
         BCT   RE,DEM40I                                                        
         B     DEMERR                                                           
*                                  BUILD OUTPUT LIST ENTRY                      
DEM42    CLI   IOPTN,C'A'                                                       
         BNE   DEM43               FOR OPTION 'A'                               
         LA    RE,MFSEXCAT         RETURN SEX+START_AGE+END_AGE                 
         MVI   0(R8),0                                                          
DEM42A   LLC   R1,0(RE)                                                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    OSEX,0                                                           
         BNO   *+10                                                             
         OC    0(1,R8),1(RE)       UPDATE SEX CATEGORY                          
         CLI   0(RE),FF                                                         
         BE    *+12                                                             
         LA    RE,L'MFSEXCAT(RE)                                                
         B     DEM42A                                                           
         MVC   1(2,R8),OAGE        START_AGE+END_AGE                            
         LA    R8,3(R8)                                                         
         B     DEM60                                                            
*                                                                               
DEM43    CLI   FLAVOR,0            TEST IF OLD STYLE                            
         BNE   DEM44                                                            
*                                                                               
         CLI   DBSELMED,C'R'       IF IT'S RADIO                                
         JNE   DEM43ARX                                                         
         CLI   DBSELSRC,C'M'       BBM IS OK                                    
         JE    DEM43ARX                                                         
         LA    RE,ARBINV           ARB/RADAR NEED FUTHER CHECKS                 
DEM43AR  CLI   0(RE),X'FF'                                                      
         JE    DEM43ARX                                                         
         CLC   ODEMO,0(RE)         IN TABLE SAYS ITS NOT VALID                  
         JE    DEMERR                                                           
         LA    RE,1(RE)                                                         
         J     DEM43AR                                                          
DEM43ARX DS    0C                                                               
*                                                                               
         TM    OFLAG,OFLAG_TP      TEST IF A TAPE PRECISION RATING              
         BZ    DEM43B                                                           
         NI    OMOD,X'FF'-X'40'    YES - MAKE MODIFIER LOWER CASE               
         MVI   BYTE,C'1'           WARNING #                                    
*        BAS   RE,SNDMAIL          WARN DEIS: VERY BAD FOR PLD                  
*                                                                               
DEM43B   CLI   OCAT,0              MOVE IN CATEGORY IF NOT ZERO                 
         BE    *+10                                                             
         MVC   OSTATUS,OCAT                                                     
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+1(1),OMOD      UNENCODED MODIFIER                           
         GOTO1 CDEMOCON,DMCB,(1,FULL),('DEMOCON_17',FULL),(R9),OPLD             
         MVC   OMOD,FULL+1         ENCODED MODIFIER                             
*                                                                               
         MVC   0(1,R8),OSTATUS     ADD ENTRY TO OUTPUT LIST                     
         MVC   1(1,R8),OMOD                                                     
         MVC   2(1,R8),ODEMO                                                    
         LA    R8,3(R8)                                                         
         B     DEM60                                                            
*                                                                               
DEM44    LAY   RE,EQU2TO3                                                       
         MVC   FULL(1),OCAT                                                     
         MVC   FULL+1(1),OMOD                                                   
         MVC   FULL+2(1),OSEX2                                                  
         MVC   FULL+3(1),ODEMO                                                  
DEM44A   CLC   0(2,RE),=H'0'       MATCH TO OLD DEMOS                           
         BE    DEM44B                                                           
         CLC   FULL+2(2),1(RE)                                                  
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     DEM44A                                                           
*                                                                               
         LLC   R1,0(RE)            FOUND ONE - CONVERT TO SEX 0 RO 1            
         STC   R1,ODEMO                                                         
         SLL   R1,1                                                             
         STCM  R1,2,OSEX2                                                       
         NI    ODEMO,X'7F'                                                      
*                                                                               
DEM44B   CLI   FLAVOR,C'4'         4 CHAR CMSD                                  
         BNE   DEM46                                                            
         TM    OFLAG,OFLAG_TP      TEST IF TAPE PRECISION RATING                
         BZ    DEM44C                                                           
         NI    OMOD,X'FF'-X'40'    YES - MAKE MODIFIER LOWER CASE               
         MVI   BYTE,C'1'           WARNING #                                    
*        BAS   RE,SNDMAIL          WARN DEIS: VERY BAD FOR PLD                  
*                                                                               
DEM44C   MVC   0(1,R8),OCAT                                                     
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+1(1),OMOD      UNENCODED MODIFIER                           
         GOTO1 CDEMOCON,DMCB,(1,FULL),('DEMOCON_17',FULL),(R9),OPLD             
         MVC   OMOD,FULL+1         ENCODED MODIFIER                             
*                                                                               
         MVC   1(1,R8),OMOD                                                     
         MVC   2(1,R8),OSEX2                                                    
         MVC   3(1,R8),ODEMO                                                    
*                                                                               
         BAS   RE,PROCDEMO         PROCESS SPECIAL DEMO CATEGORIES              
         JNE   *+14                                                             
         MVI   2(R8),0             NO SEX CATEGORY                              
         MVC   3(1,R8),DEMO#       USE ACTUAL DEMO #, NOT OFFSET                
*                                                                               
         LA    R8,4(R8)                                                         
         B     DEM60                                                            
*                                                                               
DEM46    CLI   FLAVOR,C'P'         3 CHAR PACKED                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R8),OCAT                                                     
*                                                                               
         LA    R1,PAKCHAR          ASSIGN MODIFIER NUMBER                       
         LR    R0,R1                                                            
DEM46A   CLI   0(R1),0             EOF                                          
         BNE   *+12                                                             
         LA    R1,27               SET TO DEFAULT                               
         B     DEM46B                                                           
         CLC   OMOD,0(R1)          FIND CURRENT MODIFIER DISPLACEMENT           
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     DEM46A                                                           
*                                                                               
         SR    R1,R0               CALC DISPLACEMENT                            
         LA    R1,1(R1)                                                         
*                                                                               
DEM46B   SLDL  R0,32                                                            
         LLC   R1,OSEX2                                                         
         SLL   R0,4                                                             
         AR    R0,R1                                                            
         LLC   R1,ODEMO                                                         
         SLL   R0,7                                                             
         AR    R1,R0                                                            
         STCM  R1,3,1(R8)                                                       
         LA    R8,3(R8)                                                         
         B     DEM60                                                            
*                                                                               
DEM60    MVI   0(R8),FF            SET E-O-L                                    
         LLC   R1,NOUTPUT          BUMP OUTPUT FIELD COUNT                      
         LA    R1,1(R1)                                                         
         STC   R1,NOUTPUT                                                       
         B     DEM4                PROCESS NEXT FIELD                           
         DROP  R1                                                               
         EJECT                                                                  
*==================================================================             
* IF ANY COMSCORE DEMOS ADDED, GO BACK TO ASSIGN SEQUENCE NUMBERS               
* USING EMPTY SLOTS IN AOUTNTDM.                                                
*==================================================================             
                                                                                
DEM90    CLI   PASS,2              TEST SECOND PASS                             
         JE    DEM95                                                            
*                                                                               
         CLI   IOPTN,C'S'          TEST SPOT VALIDATION                         
         JNE   DEM95                                                            
*                                                                               
         CLI   NEWNONT,C'Y'        TEST ANY NEW COMSCORE DEMOS                  
         JNE   DEM95               NO                                           
*                                                                               
         MVC   NADR,AINPUT         SET A(FIRST TWA FIELD)                       
         MVC   NFLDS,NINPUT        SET N'TWA FIELDS                             
         CLI   NFLDS,0                                                          
         BNE   *+8                                                              
         MVI   NFLDS,1                                                          
         MVC   NLIST,NOUTPUT       SET MAX N'OUTPUT LIST ENTRIES                
         CLI   NLIST,0                                                          
         BNE   *+8                                                              
         MVI   NLIST,FF                                                         
*        MVI   NINPUT,0            REMOVED FOR PASS 2 COMSCORE BUG              
*                                  SCHT - 8/17                                  
         MVI   NOUTPUT,0                                                        
         MVI   DEMOFLAG,0                                                       
         MVI   DEMOINDX,1                                                       
         MVI   PASS,2              SET PASS 2 FOR COMSCORE                      
         LM    R8,R9,AOUTPUT       THEN RESTORE THESE REGS                      
         B     DEM2                                                             
                                                                                
*=============================================================                  
* END OF PASS 2 - SET EOL FLAG                                                  
*=============================================================                  
                                                                                
DEM95    L     R8,AOUTPUT                                                       
*                                                                               
DEM95A   CLI   0(R8),X'FF'         EOL FROM FIRST PASS?                         
         JE    DEM96                                                            
         CLI   DBDEMTYP,C'4'       4 BYTE DEMO CATEGORIES?                      
         JE    DEM95B                                                           
         OC    0(3,R8),0(R8)                                                    
         JZ    *+12                                                             
         LA    R8,3(R8)                                                         
         J     DEM95A                                                           
         MVI   0(R8),X'FF'                                                      
         J     DEM96                                                            
*                                                                               
DEM95B   OC    0(4,R8),0(R8)                                                    
         JZ    *+12                                                             
         LA    R8,4(R8)                                                         
         J     DEM95A                                                           
         MVI   0(R8),X'FF'                                                      
*==========================================================                     
* HAVE TO REPLICATE CATEGORIES IF NO CATEGORY WAS GIVEN                         
*==========================================================                     
                                                                                
DEM96    CLI   DBSELMED,C'R'       FOR RADIO ONLY                               
         BNE   DEM100                                                           
         MVI   PREVCATN,0                                                       
         L     RF,AOUTPUT                                                       
DEM97    CLI   0(RF),X'FF'                                                      
         BE    DEM100                                                           
         CLI   0(RF),0             DO WE NEED THE CATEGORY?                     
         BE    *+10                  YES, USE PREVIOUS ONE                      
         MVC   PREVCATN,0(RF)      COPY THE CATEGORY WHEN NON-ZERO              
*                                                                               
         MVC   0(1,RF),PREVCATN                                                 
         LA    RF,3(RF)            NEXT DEMO                                    
         CLI   FLAVOR,C'4'         4 CHAR EXTENDED DEMOS?                       
         BNE   *+8                                                              
         LA    RF,1(RF)            YES: BUMP 1 MORE                             
         B     DEM97                                                            
*                                                                               
DEM100   MVI   NINPUT,0            EXITING W/O ERROR                            
         J     XIT                                                              
*                                                                               
* SPOTPAK USER DEMOS                                                            
*                                                                               
DEMUSR   DS    0H                                                               
         OC    AUSRNMS,AUSRNMS                                                  
         BZ    DEMERR                                                           
         CLI   NUSRDEMS,3                                                       
         BH    DEMERR              MAX 4 USER DEMOS                             
         NI    FLD+1,X'0F'         EXTRACT USER DEMO NUMBER                     
         LLC   R3,FLD+1                                                         
         BAS   RE,REMOVE           REMOVE C'UN/'                                
         BAS   RE,REMOVE                                                        
         BAS   RE,REMOVE                                                        
         BZ    DEMERR                                                           
         LLC   RE,NUSRDEMS                                                      
         LA    RE,1(RE)                                                         
         STC   RE,NUSRDEMS                                                      
*                                                                               
         MVI   OMOD,USERDEMO       SET DEMO TYPE                                
         STC   R3,ODEMO            AND NUMBER                                   
*                                                                               
         BCTR  R3,0                                                             
         MHI   R3,7                                                             
         A     R3,AUSRNMS                                                       
         OC    0(7,R3),0(R3)       TEST FOR DUPLICATE US NUMS IN INPUT          
         BNZ   DEMERR                                                           
         MVC   0(7,R3),FLD                                                      
         B     DEM42                                                            
         SPACE 2                                                                
* SPOTPAK WEIGHTED DEMO                                                         
*                                                                               
DEMWGT   DS    0H                                                               
         OC    AUSRNMS,AUSRNMS                                                  
         BZ    DEMERR                                                           
         MVI   OMOD,WGHTDEMO                                                    
         MVI   ODEMO,1                                                          
         L     R3,AUSRNMS                                                       
         OC    28(7,R3),28(R3)     TEST PREVIOUSLY INPUT                        
         BNZ   DEMERR                                                           
         BAS   RE,REMOVE                                                        
         BAS   RE,REMOVE                                                        
         BZ    DEMERR                                                           
         MVC   28(7,R3),FLD                                                     
         B     DEM42                                                            
         EJECT                                                                  
*                                                                               
* THIS ROUTINE PROCESSES SPECIAL CASE DEMO CATEGORIES                           
*                                                                               
PROCDEMO NTR1                                                                   
         MVI   DEMO#,0                                                          
         USING PDTABD,RF                                                        
         LA    RF,PDEMOTAB                                                      
PDEMO02  CLI   0(RF),X'FF'                                                      
         JE    PDEMON                                                           
*                                                                               
         CLC   PDTSTD,OAGE         START AGE?                                   
         JNE   PDEMO10                                                          
         CLC   PDTETD,OAGE+1        END AGE?                                    
         JNE   PDEMO10                                                          
*        JE    PDEMO04                                                          
*        ZIC   R1,PDTSEX           SEX                                          
*        EX    R1,*+8                                                           
*        J     *+8                                                              
*        TM    OSEX,0                                                           
*        JZ    PDEMO10                                                          
         CLC   OSEX,PDTSEX                                                      
         JNE   PDEMO10                                                          
         MVC   DEMO#,PDTDEMO#      SET DEMO #                                   
         J     PDEMOY                                                           
*                                                                               
PDEMO10  AHI   RF,PDTTABL          GET NEXT TABLE ENTRY                         
         J     PDEMO02                                                          
*                                                                               
PDEMOY   SR    RC,RC               YES                                          
PDEMON   LTR   RC,RC               NO                                           
         J     XIT                                                              
         DROP  RF                                                               
*                                                                               
* TABLE OF SPECIAL DEMO CATEGORIES                                              
*                                                                               
* AL1(START AGE,END AGE,SEX,DEMO #)                                             
*                                                                               
PDEMOTAB DC    AL1(30,54,MA,170)      MEN 30-54                                 
         DC    AL1(30,54,MA+MT,170)   MEN 30-54                                 
         DC    AL1(30,54,FA,169)      WOMEN 30-54                               
         DC    AL1(30,54,FA+FT,169)   WOMEN 30-54                               
         DC    AL1(30,54,VW,250)      VIEWERS 30-54                             
*                                                                               
* NPOD HAS FA+MA+VW SET FOR OSEX WHEN ADULTS IS REQUESTED                       
         DC    AL1(30,54,FA+MA+VW,250)   ADULTS 30-54                           
         DC    X'FF'                                                            
*                                                                               
*                                                                               
* REPLACE FE9-14 BY G9-14                                                       
* AND     MA9-14 BY B9-14                                                       
FIXFM914 NTR1                                                                   
*                                                                               
         LA    RE,FE914TAB         FE9-14                                       
FIXF914A CLI   0(RE),X'FF'                                                      
         BE    FIXM914                                                          
         LLC   R1,FLDLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RE)                                                     
         BE    FIXFEM                                                           
         LA    RE,L'FE914TAB(RE)                                                
         B     FIXF914A                                                         
FIXFEM   MVC   FLD,=CL20'G9-14'                                                 
         MVI   FLDLN,5                                                          
         B     XIT                                                              
*                                                                               
FIXM914  LA    RE,MA914TAB         MA9-14                                       
FIXM914A CLI   0(RE),X'FF'                                                      
         BE    FIXFMX                                                           
         LLC   R1,FLDLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RE)                                                     
         BE    FIXMAL                                                           
         LA    RE,L'FE914TAB(RE)                                                
         B     FIXM914A                                                         
*                                                                               
FIXMAL   MVC   FLD,=CL20'B9-14'                                                 
         MVI   FLDLN,5                                                          
         B     XIT                                                              
*                                                                               
FIXFMX   B     XIT                                                              
*                                                                               
FE914TAB DC    CL10'F914'                                                       
         DC    CL10'FE914'                                                      
         DC    CL10'FEM914'                                                     
         DC    CL10'FEMA914'                                                    
         DC    CL10'FEMAL914'                                                   
         DC    CL10'FEMALE914'                                                  
         DC    CL10'F9-14'                                                      
         DC    CL10'FE9-14'                                                     
         DC    CL10'FEM9-14'                                                    
         DC    CL10'FEMA9-14'                                                   
         DC    CL10'FEMAL9-14'                                                  
         DC    CL10'FEMALE9-14'                                                 
         DC    X'FF'                                                            
MA914TAB DC    CL10'M914'                                                       
         DC    CL10'MA914'                                                      
         DC    CL10'MAL914'                                                     
         DC    CL10'MALE914'                                                    
         DC    CL10'M9-14'                                                      
         DC    CL10'MA9-14'                                                     
         DC    CL10'MAL9-14'                                                    
         DC    CL10'MALE9-14'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
* REMOVE FIRST BYTE OF FLD & DECREMENT FLDLN.                                   
* ON EXIT CC=EQ IF FIELD HAS BECOME EMPTY.                                      
*                                                                               
REMOVE   LLC   R1,FLDLN                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),FLD+1                                                     
         SHI   R1,1                SETS CC=EQ WHEN R1=0                         
         STC   R1,FLDLN                                                         
         BR    RE                                                               
         SPACE 1                                                                
* CONVERT FIELD TO BINARY.                                                      
* ON ENTRY R1=A(FIELD), R0=L'FIELD. ON EXIT CC=EQ ON ERROR ELSE DUB(1)          
* CONTAINS BINARY VALUE OF FIELD.                                               
*                                                                               
NUMERIC  STM   RE,R1,12(RD)        SAVE REGISTERS                               
         LR    RE,R1               RE=A(FIELD)                                  
         SR    R1,R1                                                            
         LTR   RF,R0               R0=L'FIELD                                   
         BZ    NUMERICX                                                         
         CHI   RF,8                MAX FIELD LENGTH                             
         BH    NUMERICX                                                         
         BCTR  RF,0                FOR EX INSTRUCTION(S)                        
*                                                                               
         MVC   DUB,=8C'0'                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),0(RE)                                                     
         CLC   DUB,=8C'0'          TEST FIELD IS NUMERIC                        
         BNE   NUMERICX                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)                                                      
         CVB   R1,DUB              GET BINARY VALUE                             
         CHI   R1,255              TEST VALUE GR MAX                            
         BNH   *+6                                                              
         SR    R1,R1               YES - SET TO ZERO                            
*                                                                               
NUMERICX STC   R1,DUB              RETURN VALUE IN DUB(1)                       
         LM    RE,R1,12(RD)        RESTORE REGISTERS                            
         CLI   DUB,0               SET CC=EQ ON ERROR                           
         BR    RE                  RETURN TO CALLER                             
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
NAD_DELIMITER EQU C'.'             DEFAULT NAD CATEGORY DELIMITER               
TABLISTL DC   X'D6',3X'00',X'E2',3X'00',X'E8',3X'00',X'EA',3X'00',X'FF'         
PAKCHAR  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ',X'00'                              
         SPACE 1                                                                
* EQUATED VALUES                                                                
*                                                                               
R        EQU   DEMO_MODIFIER_R     RATING                                       
S        EQU   DEMO_MODIFIER_S     SHARE                                        
I        EQU   DEMO_MODIFIER_I     IMPRESSIONS                                  
B        EQU   DEMO_MODIFIER_B     EXTENDED DMA SHARE                           
E        EQU   DEMO_MODIFIER_E     EXTENDED DMA SHARE                           
X        EQU   DEMO_MODIFIER_X     TSA SHARE                                    
T        EQU   DEMO_MODIFIER_T     TSA IMPRESSIONS                              
U        EQU   DEMO_MODIFIER_U     UNIVERSES                                    
USERDEMO EQU   33                  USER DEMO MODIFIER (SPOT)                    
WGHTDEMO EQU   63                  WEIGHTED DEMO MODIFIER (SPOT)                
         EJECT                                                                  
       ++INCLUDE DEDEMOVAL1                                                     
         EJECT                                                                  
* TABLE OF USER NAME EXPRESSIONS FOR SPOTPAK                                    
* BYTES 0-2 = USER NAME PREFIXES                                                
*                                                                               
USNTAB   DS    0CL3                                                             
         DC    C'U1/'                                                           
         DC    C'U2/'                                                           
         DC    C'U3/'                                                           
         DC    C'U4/'                                                           
USNAMES  EQU   (*-USNTAB)/L'USNTAB                                              
*                                                                               
* TABLE OF INVALID ARB/RADAR DEMOS (THESE ARE ON THE FILE AND CAN ONLY          
*                                   BE USED TO CALC LARGER GROUPS)              
ARBINV   DC    AL1(68)  W1820                                                   
         DC    AL1(69)  W2124                                                   
         DC    AL1(112) M1820                                                   
         DC    AL1(113) M2124                                                   
         DC    AL1(188) V1820                                                   
         DC    AL1(189) V2124                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
* SPOT DEMO CONVERSION TABLE (INDEX BY SPOT DEMO-1*2).                          
*                                                                               
* BYTE 0 = MODIFIER CODE, BYTE 1 = DEMO NUMBER                                  
*                                                                               
SPDEMTAB DS    0XL2                                                             
         DC    AL1(S,001,R,002,R,001,R,078,R,025,R,128,R,040,R,041)             
         DC    AL1(R,042,R,045,R,047,R,050,R,057,R,054,R,028,R,048)             
         DC    AL1(R,049,R,091,R,092,R,095,R,097,R,101,R,121,R,122)             
         DC    AL1(R,123,R,098,R,125,R,028,R,099,R,084,R,141,R,142)             
         DC    AL1(R,145,R,154,R,172,R,129,R,154,R,104,R,100,R,147)             
         DC    AL1(R,148,R,150,I,075,R,182,I,079,I,084,I,090,I,091)             
         DC    AL1(I,092,I,095,I,096,I,097,I,100,I,101,I,104,I,107)             
         DC    AL1(I,098,I,099,I,000,0,000,0,000,0,000,0,000,I,001)             
         DC    AL1(I,078,0,000,I,025,I,034,R,029,I,029,R,043,I,043)             
         DC    AL1(R,052,I,052,0,000,I,028,I,040,I,041,I,042,I,045)             
         DC    AL1(I,046,I,047,I,050,I,051,I,054,I,057,I,048,I,049)             
         DC    AL1(I,053,S,003,R,003,I,103,R,103,I,153,R,153,R,065)             
         DC    AL1(I,065,I,121,I,122,I,123,R,090,I,125,R,053,I,172)             
         DC    AL1(I,127,I,140,I,182,I,179,I,185,I,128,I,129,I,130)             
         DC    AL1(R,140,R,093,I,093,I,141,I,142,I,145,I,146,I,147)             
         DC    AL1(I,148,R,127,I,151,I,154,I,157,R,149,I,149)                   
         SPACE 1                                                                
CNDEMTAB DS    0XL2                                                             
         DC    AL1(S,001,R,002,R,001,R,127,R,025,0,000,B,127,R,041)             
         DC    AL1(R,042,R,045,R,047,E,145,R,057,R,054,R,066,R,048)             
         DC    AL1(R,049,R,091,R,092,R,095,R,097,R,101,E,045,R,122)             
         DC    AL1(R,123,R,098,R,125,R,034,R,099,R,084,R,141,R,142)             
         DC    AL1(R,145,R,154,E,042,E,127,R,154,R,104,E,092,E,122)             
         DC    AL1(E,125,R,150,I,075,R,107,I,079,I,084,I,090,I,091)             
         DC    AL1(I,092,I,095,I,096,I,097,I,100,I,101,I,104,I,107)             
         DC    AL1(I,098,I,099,0,000,0,000,0,000,0,000,0,000,I,001)             
         DC    AL1(0,000,0,000,I,025,I,034,E,095,I,029,0,000,0,000)             
         DC    AL1(0,000,0,000,0,000,0,000,I,040,I,041,I,042,I,045)             
         DC    AL1(I,046,I,047,I,050,I,051,I,054,I,057,I,048,I,049)             
         DC    AL1(I,053,0,000,0,000,0,000,0,000,I,067,I,064,R,065)             
         DC    AL1(I,065,I,121,I,122,I,123,0,000,I,125,0,000,I,172)             
         DC    AL1(I,127,0,000,0,000,I,179,I,185,I,128,I,129,I,130)             
         DC    AL1(0,000,0,000,0,000,I,141,I,142,I,145,I,146,I,147)             
         DC    AL1(I,148,0,000,I,151,I,154,I,157,S,127,S,145,R,148)             
         EJECT                                                                  
* TABLE OF VALID DEMO MODIFIER/NUMBERS FOR FILES WHERE CHECKING                 
* IS NECCESSARY                                                                 
*                                                                               
DEMOTAB  DS    0X                                                               
*                                                                               
DEMOTCA  DC    AL2(DEMOTCAX-*),C'TCA'                                           
         DC    AL1(R,127,B,127,R,042,R,045,E,145,R,092,R,095,E,045)             
         DC    AL1(R,122,R,025,R,145,E,042,E,127,E,092,E,122,E,025)             
         DC    AL1(I,091,I,092,I,095,I,101,I,104,I,107,E,095,I,041)             
         DC    AL1(I,042,I,045,I,051,I,054,I,122,I,125,I,172,I,127)             
         DC    AL1(I,129,I,130,I,141,I,142,I,145,I,151,I,154,I,157)             
         DC    AL1(S,127,S,145,I,148,E,148,R,048,R,098,R,125,I,048)             
         DC    AL1(I,048,I,098,E,142,E,141,E,048,E,041,E,098,E,091)             
         DC    AL1(E,125,U,122,U,125,U,090,U,096,U,101,U,105,U,111)             
         DC    AL1(U,112,U,040,U,046,U,051,U,055,U,061,U,062,R,047)             
         DC    AL1(U,047,E,047,I,047,R,052,U,052,E,052,I,052)                   
         DC    AL1(U,020,E,020,I,020,R,020,U,029,E,029,I,029,R,029)             
         DC    AL1(U,030,E,030,I,030,R,030,U,033,E,033,I,033,R,033)             
         DC    AL1(U,046,E,046,I,046,R,046,U,055,E,055,I,055,R,055)             
         DC    AL1(U,058,E,058,I,058,R,058,U,059,E,059,I,059,R,059)             
         DC    AL1(U,060,E,060,I,060,R,060,U,075,E,075,I,075,R,075)             
         DC    AL1(U,077,E,077,I,077,R,077,U,079,E,079,I,079,R,079)             
         DC    AL1(U,080,E,080,I,080,R,080,U,083,E,083,I,083,R,083)             
         DC    AL1(U,090,E,090,I,090,R,090,U,096,E,096,I,096,R,096)             
         DC    AL1(U,097,E,097,I,097,R,097,U,102,E,102,I,102,R,102)             
         DC    AL1(U,105,E,105,I,105,R,105,U,108,E,108,I,108,R,108)             
         DC    AL1(U,109,E,109,I,109,R,109,U,110,E,110,I,110,R,110)             
         DC    AL1(U,119,E,119,I,119,R,119,U,133,E,133,I,133,R,133)             
         DC    AL1(U,143,E,143,I,143,R,143,U,146,E,146,I,146,R,146)             
         DC    AL1(U,155,E,155,I,155,R,155,U,158,E,158,I,158,R,158)             
         DC    AL1(U,159,E,159,I,159,R,159,U,160,E,160,I,160,R,160)             
         DC    AL1(U,63,E,63,I,63,R,63)                                         
         DC    AL1(U,244,E,244,I,244,R,244)                                     
         DC    AL1(U,245,E,245,I,245,R,245)                                     
         DC    AL1(U,246,E,246,I,246,R,246)                                     
         DC    AL1(U,247,E,247,I,247,R,247)                                     
         DC    AL1(U,152,E,152,I,152,R,152)                                     
         DC    AL1(U,248,E,248,I,248,R,248)                                     
         DC    AL1(U,249,E,249,I,249,R,249)                                     
         DC    AL1(U,104,E,104,I,104,R,104)                                     
         DC    AL1(U,54,E,54,I,54,R,54)                                         
*ADDED MAR02                                                                    
         DC    AL1(R,128,E,128,I,128,B,128)  V1224                              
         DC    AL1(R,129,E,129,I,129)        V1234                              
         DC    AL1(R,130,E,130,I,130)        V1249                              
         DC    AL1(R,140,E,140,I,140)        V1824                              
         DC    AL1(R,150,E,150,I,150)        V25+                               
         DC    AL1(R,147,E,147,I,147)        V2549                              
         DC    AL1(R,154,E,154,I,154)        V35+                               
         DC    AL1(R,151,E,151,I,151,U,151)  V3549                              
         DC    AL1(R,152,E,152,I,152)        V3554                              
         DC    AL1(R,157,E,157,I,157)        V50+                               
         DC    AL1(R,153,E,153,I,153)        V3564                              
         DC    AL1(R,050,E,050,I,050)        W25+                               
         DC    AL1(R,054,E,054,I,054)        W35+                               
         DC    AL1(R,057,E,057,I,057)        W50+                               
         DC    AL1(R,053,E,053,I,053)        W3564                              
         DC    AL1(R,100,E,100,I,100)        M25+                               
         DC    AL1(R,104,E,104,I,104)        M35+                               
         DC    AL1(R,101,E,101,I,101)        M3549                              
         DC    AL1(R,107,E,107,I,107)        M50+                               
         DC    AL1(R,103,E,103,I,103)        M3564                              
* ADDED DEC02 FOR P&G                                                           
         DC    AL1(R,011,E,011,I,011)        YCU12                              
         DC    AL1(R,012,E,012,I,012)        YCU18                              
         DC    AL1(R,013,E,013,I,013)        WWCAT                              
         DC    AL1(R,014,E,014,I,014)        WWDOG                              
         DC    AL1(R,237,E,237,I,237)        WW1849                             
* ADDED OCT/04                                                                  
         DC    AL1(R,051,E,051)              W3549                              
DEMOTCAX DS    0X                                                               
*                                                                               
*EMOTCN  DC    AL2(DEMOTCNX-*),C'TCN'                                           
*        DC    AL1(R,001,R,127,R,025,R,041,R,042,R,045,R,047,R,057)             
*        DC    AL1(R,091,R,092,R,095,R,097,R,122,R,125,R,145,I,075)             
*        DC    AL1(R,107,I,079,I,090,I,091,I,092,I,095,I,096,I,097)             
*        DC    AL1(I,100,I,101,I,104,I,107,I,001,I,025,I,029,I,040)             
*        DC    AL1(I,041,I,042,I,045,I,046,I,047,I,050,I,051,I,054)             
*        DC    AL1(I,057,I,064,R,065,I,122,I,125,I,172,I,127,I,128)             
*        DC    AL1(I,129,I,130,I,141,I,142,I,145,I,146,I,147,I,151)             
*        DC    AL1(I,048,I,098,I,148,R,048,R,098,R,148)                         
*        DC    AL1(I,154,I,157)                                                 
*EMOTCNX DS    0X                                                               
*                                                                               
DEMOTABX DC    AL2(0)                                                           
         EJECT                                                                  
EQU2TO3  DS    0H                                                               
       ++INCLUDE DEEQU2TO3                                                      
         EJECT                                                                  
         DS    0H                                                               
       ++INCLUDE DERADCATS                                                      
         SPACE 2                                                                
         DS    0H                                                               
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
* PERSONAL LANGUAGE DEMO CHARACTER SEARCH                                       
*                                                                               
         DS    0D                                                               
TRTPLD   DC    XL256'00'                                                        
         ORG   TRTPLD+NAD_DELIMITER                                             
         DC    X'01'               NOT A PLD                                    
         ORG   TRTPLD+PLD_SPANISH_ONLY                                          
         DC    X'FF'               PLD                                          
         ORG   TRTPLD+PLD_MOSTLY_SPANISH                                        
         DC    X'FF'               PLD                                          
         ORG   TRTPLD+PLD_ENGLISH_ONLY                                          
         DC    X'FF'               PLD                                          
         ORG   TRTPLD+PLD_MOSTLY_ENGLISH                                        
         DC    X'FF'               PLD                                          
         ORG   TRTPLD+PLD_ENGLISH_SPANISH                                       
         DC    X'FF'               PLD                                          
         ORG                                                                    
         EJECT                                                                  
*                                                                               
* GET DEMO TABLES AND ALET                                                      
*                                                                               
GETTABS  NTR1  BASE=*,LABEL=*                                                   
         MVC   TABLIST,TABLISTL    GET A(REQUIRED TABLES)                       
         GOTO1 CDEMADDR,DMCB,(X'FF',TABLIST),COMFACSD                           
*                                                                               
         XC    DUB,DUB             SPECIAL CALL FOR SYSFACS                     
         MVC   DUB(4),=XL4'FEFFFFFF'                                            
         ICM   RF,15,CSWITCH                                                    
         BZ    GTABS10                                                          
         GOTO1 CSWITCH,DUB                                                      
         L     RF,0(R1)            RF=V(SYSFACS)                                
         L     RF,VSSB-SYSFACD(RF) RF=V(SSB)                                    
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
         B     GETTABSX                                                         
*                                                                               
GTABS10  L     RE,CMASTC             OFFLINE GET ADDR FROM MASTC                
         L     RF,MCSSB-MASTD(RE)    OBTAIN ALET FOR TABS DSPACE                
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
*                                                                               
GETTABSX J     XIT                                                              
* ROUTINES TO DEAL WITH DEMO NAME PREFIXES (CATEGORIES)                         
         SPACE 2                                                                
* REMOVE A PREFIX FROM CURRENT FIELD AND SAVE IN PREFIX                         
* ALSO CHECK FOR PERSONAL LANGUAGE DEMO (PLD) INDICATOR CHARACTER.              
* NOTE THAT IF A PERSONAL LANGUAGE DEMO INDICATOR CHARACTER CAN                 
* LEGITIMATELY APPEAR FOR ANOTHER REASON, THEN THIS CODE WILL NEED              
* FURTHER CONSIDERATION.                                                        
*                                                                               
RMPFX    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   PREFIXLN,0                                                       
         MVC   PREFIX,=CL7' '                                                   
*                                                                               
         LARL  RE,TRTPLD           A(PLD TRT TABLE)                             
         LLC   R1,FLDLN            GET CURRENT LENGTH                           
         LA    RF,FLD-1(R1)        POINT TO END OF FIELD                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         TRTR  0(0,RF),0(RE)       SCAN BACKWARDS (MODIFIES R2)                 
         BZ    RMPFX2              NONE FOUND                                   
*                                                                               
         LR    RF,R1               POINT RF AT FOUND CHARACTER                  
         CLI   0(R1),NAD_DELIMITER IS IT A PERIOD?                              
         BE    RMPFX4              YES                                          
*                                                                               
         MVC   OPLD,0(R1)          IT'S A PLD INDICATOR: SAVE IT                
*                                                                               
         LA    R0,FLD              IS IT THE FIRST CHARACTER...                 
         CR    R1,R0               ...IN THE STRING?                            
         BNE   *+12                NO                                           
         BAS   RE,REMOVE           YES: REMOVE IT                               
         B     RMPFXEX             THERE IS NO CATEGORY: EXIT                   
*                                                                               
         LR    RF,R1               RF = A(PLD CHARACTER)                        
         B     RMPFX4                                                           
*                                                                               
RMPFX2   DS    0H                                                               
         CLI   IOPTN,C'C'          ASSUME NO CATEGORY REQUIRED                  
         BNE   RMPFXEX             IF THIS IS NOT A CATEGORY VALIDATION         
*                                                                               
         LLC   R1,FLDLN            ACCEPT FIELD WITHOUT TERMINATOR              
         LA    RF,FLD(R1)          IF CAT. VALIDATION ONLY                      
*                                                                               
RMPFX4   LA    R1,FLD                                                           
         SR    RF,R1               GET LENGTH OF PREFIX                         
         STC   RF,PREFIXLN         AND SAVE THE LENGTH MINUS DELIMITER          
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PREFIX(0),FLD       * SAVE THE PREFIX *                          
         LA    RF,2(RF)            BUMP BACK TO ORIG. LENGTH PLUS DELIM         
         BAS   RE,REMOVE           AND REMOVE THE PREFIX                        
         BCT   RF,*-4                                                           
*                                                                               
RMPFXEX  DS    0H                                                               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE A CATEGORY DESCRIPTION                                               
* ON ENTRY PREFIX=CATEGORY NAME PREFIXLN=LENGTH.                                
* ON EXIT OCAT= CATEGORY NUMBER OR 0 IF ERROR                                   
CATVAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LLC   R0,PREFIXLN         GET PREFIX LENGTH                            
         LA    R1,PREFIX                                                        
         BAS   RE,NUMERIC          TRY FOR A NUMERIC CHARACTER                  
         BNE   CATVAL1                                                          
         MVI   BYTE,C'2'           WARNING #                                    
*        BAS   RE,SNDMAIL          WARN DEIS: TROUBLE FOR PLD                   
*                                                                               
CATVAL1  LARL  RF,PFXTAB           POINT TO VALID CATEGORIES                    
         LA    RE,L'PFXTAB                                                      
         LA    R1,PREFIXES         GET NUMBER OF ENTRIES                        
*                                                                               
         CLI   DBSELMED,C'R'       CHECK FOR RADIO                              
         BNE   CATVAL2                                                          
         LARL  RF,PFXRAD           POINT TO VALID CATEGORIES                    
         LA    RE,L'PFXRAD                                                      
         LA    R1,PRERADES         GET NUMBER OF ENTRIES                        
*                                                                               
CATVAL2  IC    R0,0(RF)            ANTICIPATE VALID CATEGORY                    
         CLC   DUB(1),0(RF)        CHECK FOR VALID NUMBER                       
         BE    *+10                                                             
         CLC   PREFIX(7),1(RF)     MATCH ON DESCRIPTION                         
         BE    CATVAL4                                                          
         AR    RF,RE               TRY NEXT ENTRY                               
         BCT   R1,CATVAL2                                                       
         SR    R0,R0               SEND OUT ERROR                               
*                                                                               
CATVAL4  STC   R0,OCAT             SET DUB FOR OUTPUT                           
         LTR   R0,R0               SET CONDITION CODE                           
*                                                                               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
* SEND DEIS AN E-MAIL IF THIS ROUTINE VALIDATES SOMETHING WE'RE NOT             
* EXPECTING.                                                                    
*                                                                               
SNDMAIL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* NOTE: FIELD WRNSTRNG IS ALREADY FILLED IN.                                    
*                                                                               
         MVC   WRNMSG,=C'AUTONOTE*DEISN:DEMOVAL WARNING '                       
         MVC   WRNMSG#,BYTE        WARNING #                                    
         MVI   WRNSPACE,C' '                                                    
         MVI   WRNSTRNG_START,C'>'                                              
         MVI   WRNSTRNG_END,C'<'                                                
*                                                                               
         MVC   WRNMSG1,=C'. CALLED BY '                                         
         LR    RF,RD                                                            
         CLC   =C'DEMV',0(RF)      LOOK FOR DEMOVAL EYE-CATCHER                 
         BE    *+12                                                             
         L     RF,4(,RF)                                                        
         B     *-14                                                             
*                                                                               
         L     R1,4(,RF)           GO BACK 1 MORE LEVEL, JUST IN CASE           
         L     RE,(17-1)*4(,RF)    THIS IS THE CALLER'S RB                      
         MVC   WRNCALR,22(RE)      POSIT CALLER USED NBASE                      
         CLC   =X'90ECD00C0DB041BB0000',0(RE)                                   
         BE    SNDMAIL5            CORRECT                                      
         CLC   =X'90ECD00C0DB0A7BAFFFA47FB',0(RE)   ... OR NMOD1                
         BE    SNDMAIL5            CORRECT                                      
         MVC   WRNCALR,20(RE)      POSIT CALLER USED NTR1 BASE=*                
         CLC   =X'90ECD00C0DB0A7BAFFFA50DD',0(RE)                               
         BE    SNDMAIL5            CORRECT                                      
         MVC   WRNCALR(4),0(R1)    USE RD CHAIN EYE-CATCHER                     
         MVC   WRNCALR+4(4),=C'????'                                            
*                                                                               
SNDMAIL5 DS    0H                                                               
         OC    WRNCALR,=C'        '  OPMSG CAN'T HANDLE UNPRINTABLES            
         GOTO1 CDATAMGR,DMCB,=C'OPMSG',('WRNMSGLQ',WRNMSG)                      
*                                                                               
         J     XIT                                                              
*&&                                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DSECT TO COVER PARAMETER LIST.                                                
*                                                                               
PARMD    DSECT                     INPUT VALUE/OUTPUT VALUE                     
PARAM1   DS    0A                                                               
NINPUT   DS    0XL1                N'FLDS/FIELD ERROR NUMBER                    
AINPUT   DS    A                   A(FIRST FLD)/A(ERROR FLD)                    
*                                                                               
PARAM2   DS    0A                                                               
NOUTPUT  DS    0XL1                MAX OUTPUT/ACTUAL OUTPUT                     
AOUTPUT  DS    A                   A(OUTPUT LIST)                               
*                                                                               
PARAM3   DS    0A                                                               
IOPTN    DS    0CL1                S=SPOTPAK VALIDATION                         
*                                  Y=VALIDATE PRIME/CPP/CPM                     
ABLOCK   DS    A                   A(DBLOCK)                                    
*                                                                               
PARAM4   DS    0A                                                               
NUSRDEMS DS    0CL1                NUMBER OF USER DEMOS PRESENT                 
AUSRNMS  DS    A                   A(USER NAME LIST) FOR SPOTPAK                
*                                                                               
PARAM5   DS    0A                  X'00'- A(INP NONTRD INDX DEMO LIST)          
*                                  X'80'- A(64 BYTE EXTENDED BLOCK)             
*                                  X'40'- VALIDATE COMSCORE DEMO INDEX          
AEXTBLK  DS    0A                  A(EXTENDED BLOCK)                            
AINNTDM  DS    A                   A(INPUT NON-TRAD INDEX DEMO LIST)            
*                                                                               
PARAM6   DS    0A                                                               
AOUTNTDM DS    A                   A(OUTPUT NON-TRAD INDEX DEMO LIST)           
         SPACE 1                                                                
* DSECT TO COVER EXTBLK - PARAM 5                                               
       ++INCLUDE DEDEMOVALD                                                     
*                                                                               
* DSECT TO COVER W/S                                                            
*                                                                               
DEMWORKD DSECT                                                                  
DUB      DS    D                                                                
DMWORK   DS    12D                                                              
FULL     DS    F                                                                
ALET     DS    F                                                                
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
HALF     DS    H                                                                
BYTE     DS    X                                                                
THREE    DS    XL3                                                              
FLAVOR   DS    C                   OUTPUT TYPE                                  
*                                   X'00'=OLD 3 BYTE                            
*                                   C'P' =PACKED 3 BYTE                         
*                                   C'4' =4 BYTE (1CAT.1MOD.2DEMO)              
PASS     DS    C                                                                
NEWNONT  DS    C                                                                
         DS    CL1                                                              
ASEXTAB  DS    A                   SEXTAB DEPENDING ON OPTION                   
AAGETAB  DS    A                   AGETAB DEPENDING ON OPTION                   
FADR     DS    A                   A(CURRENT TWA FIELD HEADER)                  
NADR     DS    A                   A(NEXT TWA FIELD HEADER)                     
MODBXLE  DS    3A                  A(TABLE)/LENGTH/A(END OF TABLE)              
TABLIST  DS    0CL17                                                            
AMODTAB  DS    A                   A(MODIFIER TABLE)                            
AFMTAB   DS    A                   A(FILE/MEDIA TABLE)                          
ARTKTAB  DS    A                   A(RENTRAK TABLE)                             
ARTKTABN DS    A                   A(RENTRAK NET TABLE)                         
         DS    X                                                                
KEY      DS    XL40                                                             
KEYSAVE  DS    XL40                                                             
SCANCHR  DS    C                   SCANNER CHARACTER                            
NFLDS    DS    X                   N'TWA FIELDS LEFT TO PROCESS                 
NLIST    DS    X                   N'OUTPUT ENTRIES REMAINING                   
FNDX     DS    X                   FIELD ERROR NUMBER                           
NLINES   DS    X                   N'SCANNER BLOCK ENTRIES                      
FLDLN    DS    X                   FIELD LENGTH                                 
FLD      DS    CL20                EXTRACTED SCANNER BLOCK FIELD                
SVFLDLN  DS    X                   SAVED FIELD LENGTH                           
SVFLD    DS    CL20                SAVED SCANNER BLOCK ENTRY                    
INTFIL   DS    CL2                 INTERNAL FILE/SUB-FILE                       
PREFIXLN DS    X                   LENGTH OF CURRENT PREFIX                     
PREFIX   DS    CL7                 ACTUAL CURRENT PREFIX                        
OCAT     DS    C                   OUTPUT CATEGORY (OVERLAYS STATUS)            
*                                                                               
OVALS    DS    0X                  OUTPUT VALUES                                
OSTATUS  DS    X                   STATUS (CPP/CPM/PRIME)                       
OMODDEMO DS    0XL2                MODIFIER/DEMO                                
OMOD     DS    C                   MODIFIER CODE                                
ODEMO    DS    X                   DEMO NUMBER                                  
OFLAG    DS    X                   OPTIONS FLAG                                 
OFLAG_SM EQU   X'80'                SUPPRESS MODIFIER VALIDATION                
OFLAG_TP EQU   X'40'                TAPE PRECISION                              
OSEX     DS    X                   INTERNAL SEX                                 
ORANGE   DS    XL2                 START & END AGE RANGE                        
OAGE     DS    XL2                 ACTUAL START & END AGES                      
OSEX2    DS    X                   ALTERNATE INTERNAL SEX                       
OPLD     DS    C                   PERSONAL LANGUAGE INDICATOR CHAR.            
OVALLN   EQU   *-OVALS                                                          
         SPACE 2                                                                
WRNMSG   DS    C'AUTONOTE*DEISN:DEMOVAL WARNING '                               
WRNMSG#  DS    C                   WARNING IDENTIFIER                           
WRNMSG1  DS    C'. CALLED BY '                                                  
WRNCALR  DS    CL8                 EYE-CATCHER OF CALLER                        
WRNSPACE DS    C                                                                
WRNSTRNG_START DS    C'>'          DELIMIT THE STRING                           
WRNSTRNG DS    CL(L'FLD)           DEMO STRING                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054DEDEMOVAL 08/14/20'                                      
WRNSTRNG_END   DS    C'<'          DELIMIT THE STRING                           
WRNMSGLQ EQU   *-WRNMSG                                                         
         SPACE 2                                                                
         DS    0D                                                               
SCANTAB  DS    24CL32                                                           
SCANMAX  EQU   (*-SCANTAB)/L'SCANTAB                                            
PREVCATN DS    X                   PREVIOUS CATEGORY NUMBER                     
*                                                                               
         DS    0D                                                               
APLDTABS DS    0XL(6*4)            6 ADDRESSES                                  
APLMODTB DS    V                   A(MODIFIER TABLE)                            
APLPLDTB DS    V                   A(PERSONAL LANG. ATTRIBUTE TABLE)            
APLENCTB DS    V                   A(MODIFIER BYTE ENCODING TABLE)              
APLPRNTB DS    V                   A(PERSONAL LANG. PRINTABLE DESCRIPS)         
         DS    V                   SPARE                                        
         DS    V                   SPARE                                        
*                                                                               
DEMOFLAG DS    XL1                 INTERNAL FLAGS                               
DEMONTDQ EQU   X'80'               PROCESSED A NON-TRAD DEMO                    
*                                                                               
RELO     DS    F                                                                
VBINSRCH DS    A                   A(BINSRCH)                                   
DEMOINDX DS    XL1                 NON TRADITIONAL DEMO INDEX                   
BPARAMS  DS    XL32                                                             
*                                                                               
SVOVSYS  DS    XL1                 SYSTEM EQUATE                                
SPOTQ    EQU   2                   SPOT                                         
NETQ     EQU   3                   NET                                          
*                                                                               
DEMO#    DS    XL1                 DEMO NUMBER                                  
*                                                                               
IOAREA   DS    XL2000              IOAREA FOR GENFIL                            
*                                                                               
DEMWORKX EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASSB                                                          
       ++INCLUDE DEDEMEQUS2                                                     
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBSPARA                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE GEGENCDC                                                       
*                                                                               
*                                                                               
PDTABD   DSECT                                                                  
PDTSTD   DS    XL1                 START AGE                                    
PDTETD   DS    XL1                 END AGE                                      
PDTSEX   DS    XL1                 SEX CATEGORY                                 
PDTDEMO# DS    XL1                 DEMO NUMBER                                  
PDTTABL  EQU   *-PDTSTD                                                         
*                                                                               
**PAN#1  CSECT                                                                  
         END                                                                    
