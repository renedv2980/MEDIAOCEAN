*          DATA SET ACCLB0C    AT LEVEL 049 AS OF 05/01/02                      
*PHASE T6210CA                                                                  
CLB0C    TITLE '- BILL PROGRAM - CONTROL'                                       
CLB0C    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLBC**,R8,R7                                                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING CWORKD,RC                                                        
         USING FBLKD,CFMTBLK                                                    
         USING BOFELD,FBBOFEL                                                   
         SPACE 1                                                                
***********************************************************************         
* CONTROL CONTROLLER                                                  *         
***********************************************************************         
         SPACE 1                                                                
CONT     CLC   TWASCRN,CSSCRN      TEST 1ST TIME                                
         BNE   CONT02                                                           
         BAS   RE,RESLST           RESTORE ELEMENT LIST                         
         B     CONT10                                                           
*                                                                               
CONT02   XC    SVWORK,SVWORK                                                    
         BAS   RE,RECTOLST         INITIALIZE BLFEL LIST                        
         GOTO1 AOVRSCR,BOPARM,(CSSCRN,BASOLAYH)                                 
         CLI   CUCTRY,CTRYGER      TEST NOT GERMANY                             
         BE    CONT04                                                           
         XC    CONLNGW,CONLNGW     REMOVE LANGUAGE FIELD                        
         XC    CONLNGX,CONLNGX                                                  
         OI    CONLNGH+FHATD,FHATPR                                             
*                                                                               
CONT04   XR    RF,RF               TEST COME FROM CONTROL LIST SCREEN           
         ICM   RF,1,TWASESNL                                                    
         BZ    CONT05                                                           
         SLL   RF,1                                                             
         LA    RF,TWASESRA-1(RF)                                                
         CLI   0(RF),ACTLFT                                                     
         BE    CONT06                                                           
CONT05   GOTO1 DISPAGE,0           NO - ASK USER TO ENTER KEY                   
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$ENTKY)                                           
         LA    RE,CONFMTH                                                       
         ST    RE,FVADDR                                                        
         B     CONTX                                                            
*                                                                               
CONT06   L     RF,ALSVALS                                                       
         USING LSVALSD,RF                                                       
         USING TLSTD,LSTLST                                                     
         LA    RF,TLCDIR                                                        
         DROP  RF                                                               
         GOTO1 DISLINE,BOPARM,CONFMTH,CONLNGH,(RF),0                            
         TM    CSINDSL1,CSIUSELC   TEST NESTED CALL                             
         BZ    CONT10                                                           
         ICM   RF,15,APFKNTRY      TEST NTRSES VIA PFKEY/NOT LIST               
         BZ    *+12                                                             
         CLI   PFKACTN-PFKTABD(RF),ACTFT1                                       
         BE    CONT10                                                           
         OI    CONFMTH+FHATD,FHATPR PROTECT KEY                                 
         OI    CONLNGH+FHATD,FHATPR                                             
         OI    SVINDS,SVINSEL                                                   
         XR    RF,RF                                                            
         ICM   RF,3,CSSELCUR                                                    
         A     RF,AOVERSEL         RF=SELTAB ENTRY                              
         USING SELTABD,RF                                                       
         CLI   SELTVRTN,2                                                       
         BNE   *+8                                                              
         OI    SVINDS,SVIPDEL      SELECTION FOR DELETE                         
         CLI   SELTVRTN,3                                                       
         BNE   *+8                                                              
         OI    SVINDS,SVIPRES      SELECTION FOR RESTORE                        
         DROP  RF                                                               
*                                                                               
CONT10   LA    R2,CRECKEY          INITIALIZE RECORD KEY                        
         USING PBCRECD,R2                                                       
         XC    PBCKEY,PBCKEY                                                    
         GOTO1 VALLINE,BOPARM,CONFMTH,CONLNGH,PBCRECD,0                         
         BNE   CONTX                                                            
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         BAS   RE,READ             READ THE RECORD                              
         DROP  R2                                                               
*                                                                               
         CLC   CRECKEY,SVKEY       TEST CHANGE OF KEY                           
         BE    CONT14                                                           
         OC    CRACCHA,CRACCHA     TEST RECORD BEING ADDED                      
         BNZ   *+12                                                             
         OI    SVINDS,SVIPWRT                                                   
         B     CONT16                                                           
         NI    SVINDS,FF-SVIPWRT   DISPLAY RECORD DETAILS                       
         BAS   RE,RECTOLST                                                      
         GOTO1 DISPAGE,1                                                        
         B     CONTX                                                            
*                                                                               
CONT14   CLC   CRACCHA,SVRACCHA    TEST RECORD THE SAME AS LAST TIME            
         BE    CONT16                                                           
         BAS   RE,RECTOLST         SOMEONE ELSE HAS CHANGED IT                  
         GOTO1 AGETPID,CRACCHA+(RACPERS-RACELD)                                 
         MVC   FVXTRA,BCWORK                                                    
         GOTO1 DISPAGE,1                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RCRED)                                           
         LA    RE,CONTLINH                                                      
         ST    RE,FVADDR                                                        
         B     CONTX                                                            
*                                                                               
CONT16   CLI   BCPFKEY,PFKCCLRQ    TEST CLEAR PFKEY                             
         BNE   CONT18                                                           
         TM    SVINDS,SVIPDEL+SVIPRES                                           
         BNZ   CONT18              NOT VALID IF DELETE/RESTORE                  
         OI    SVINDS,SVIPWRT                                                   
         BAS   RE,CLRLST                                                        
         GOTO1 DISPAGE,0                                                        
         B     CONTX                                                            
*                                                                               
CONT18   CLI   BCPFKEY,PFKRFSHQ    TEST REFRESH PFKEY                           
         BNE   CONT20                                                           
         TM    SVINDS,SVIPADD      ONLY VALID IF NOT ADDING                     
         BO    CONT20                                                           
         OC    CRACCHA,CRACCHA     ONLY VALID IF RECORD ON FILE                 
         BZ    CONT20                                                           
         BAS   RE,RECTOLST                                                      
         GOTO1 DISPAGE,1                                                        
         NI    SVINDS,FF-SVIPWRT                                                
         B     CONTX                                                            
*                                                                               
CONT20   TM    SVINDS,SVIPDEL+SVIPRES                                           
         BNZ   CONT22                                                           
         BAS   RE,VALPAGE          VALIDATE THE PAGE                            
         BNE   CONTX                                                            
         TM    CINDS,CICHG                                                      
         BZ    *+8                                                              
         OI    SVINDS,SVIPWRT      WRITE PENDING IF ANY CHANGES MADE            
*                                                                               
CONT22   BAS   RE,SCROLL           DEAL WITH ANY SCROLLING                      
         GOTO1 PRTSET,0            SET UP PRINT BLOCKS                          
         BNE   CONTX                                                            
*                                                                               
         CLI   BCPFKEY,PFKCPRTQ    TEST FOR PRINT PFKEY                         
         BNE   CONT24                                                           
         BAS   RE,PRTOUT                                                        
         B     CONTX                                                            
*                                                                               
CONT24   CLI   BCPFKEY,PFKCSAVQ    TEST FOR WRITE PFKEY                         
         BNE   CONTX                                                            
         TM    SVINDS,SVIPWRT+SVIPDEL+SVIPRES                                   
         BZ    CONTX                                                            
         BAS   RE,LSTTOREC                                                      
         BAS   RE,WRITE                                                         
         TM    SVINDS,SVINSEL      TEST SELECTED FROM LIST                      
         BZ    CONTX                                                            
         GOTO1 AXITSES                                                          
         DC    H'0'                                                             
*                                                                               
CONTX    BAS   RE,SAVLST           SAVE ELEMENT LIST                            
         MVC   SVKEY,CRECKEY                                                    
         MVC   SVRACCHA,CRACCHA                                                 
         CLC   FVMSGNO,=AL2(FVFOK)  TEST FOR ANY ERRORS                         
         BNE   EXIT                                                             
         LA    RE,CONTLINH                                                      
         ST    RE,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RPF6A)                                           
         TM    SVINDS,SVIPADD      TEST ADD IS PENDING                          
         BO    EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$RPF6D)                                           
         TM    SVINDS,SVIPDEL      TEST DELETE IS PENDING                       
         BO    EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$RPF6R)                                           
         TM    SVINDS,SVIPRES      TEST RESTORE IS PENDING                      
         BO    EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$RPF6S)                                           
         TM    SVINDS,SVIPWRT      TEST WRITE IS PENDING                        
         BO    EXIT                                                             
         MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* HANDLE SCROLLING                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCROLL   NTR1  ,                                                                
         MVC   BOBYTE1,SVTOP#                                                   
*                                                                               
         ICM   RF,15,APFKNTRY                                                   
         BZ    SCROLL02                                                         
         USING PFKTABD,RF          TEST SCROLL PFKEY                            
         TM    PFKINDS1,PFKISCRL                                                
         BZ    SCROLLX                                                          
         B     SCROLL04                                                         
         DROP  RF                                                               
*                                                                               
SCROLL02 TM    CINDS,CICHG         TEST ANY CHANGES TO ANY FIELDS               
         BO    SCROLLX                                                          
*                                                                               
SCROLL04 XR    RF,RF                                                            
         IC    RF,BCSCRNUM         SET SCROLL MAGNITUDE = # ENTERED             
         TM    BCSCRNUM,PFKIMAXN+PFKIPAGE+PFKIHALF                              
         BZ    SCROLL12                                                         
*                                                                               
         LA    RF,CONLINN          SET SCROLL MAGNITUDE = 1 PAGE                
         TM    BCSCRNUM,PFKIHALF   TEST HALF PAGE                               
         BZ    SCROLL12                                                         
         LA    RF,CONLINN/2                                                     
         TM    BCSCROLL,PFKIUPDN                                                
         BO    SCROLL12                                                         
         IC    RF,SVBOT#                                                        
         XR    RE,RE                                                            
         IC    RE,SVTOP#                                                        
         SR    RF,RE                                                            
         SRA   RF,1                                                             
         BNZ   *+8                                                              
         LA    RF,1                                                             
*                                                                               
SCROLL12 XR    RE,RE                                                            
         IC    RE,SVTOP#           RE=CURRENT TOP NUMBER                        
         TM    BCSCROLL,PFKIUPDN   TEST SCROLL UP OR DOWN                       
         BZ    SCROLL14                                                         
         MVI   SVTOP#,0                                                         
         TM    BCSCRNUM,PFKIMAXN   TEST MAX SCROLL UP                           
         BO    SCROLL20                                                         
         CR    RF,RE                                                            
         BH    SCROLL20                                                         
         SR    RE,RF                                                            
         STC   RE,SVTOP#                                                        
         B     SCROLL20                                                         
*                                                                               
SCROLL14 TM    BCSCRNUM,PFKIMAXN   TEST MAX SCROLL DOWN                         
         BZ    SCROLL16                                                         
         IC    RE,SVTABN                                                        
         SR    RE,RF                                                            
         STC   RE,SVTOP#                                                        
         B     SCROLL20                                                         
SCROLL16 AR    RE,RF                                                            
         STC   RE,SVTOP#                                                        
         CLC   SVTOP#,SVTABN                                                    
         BL    *+8                                                              
         MVI   SVTOP#,0                                                         
*                                                                               
SCROLL20 GOTO1 DISPAGE,0                                                        
*                                                                               
SCROLLX  CLC   BOBYTE1,SVTOP#                                                   
         BE    *+8                                                              
         OI    CINDS,CISCROLL                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE A PAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALPAGE  NTR1  ,                                                                
         GOTO1 VALLINE,BOPARM,CONGRBH,CONNAMH,APBCREC,0                         
         BNE   EXITN                                                            
*                                                                               
         XC    NBPARMS,NBPARMS                                                  
VPAGE02  BAS   RE,NXTBLF           GET THE NEXT TABLE ENTRY                     
         BNE   VPAGE10                                                          
         OC    NBADSC,NBADSC                                                    
         BZ    VPAGE02                                                          
         GOTO1 VALTAB,BOPARM,NBADSC,NBAEL,NBATAB                                
         BNE   EXITN                                                            
         B     VPAGE02                                                          
*                                                                               
VPAGE10  B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A PAGE                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISPAGE  NTR1  ,                                                                
         LTR   R1,R1                                                            
         BZ    DPAGE01                                                          
         GOTO1 DISLINE,BOPARM,CONGRBH,CONNAMH,APBCREC,0                         
*                                                                               
DPAGE01  TWAXC CONDSCH,PROT=Y                                                   
*                                                                               
         XC    NBPARMS,NBPARMS                                                  
DPAGE02  BAS   RE,NXTBLF           GET THE NEXT TABLE ENTRY                     
         BNE   DPAGE10                                                          
         OC    NBADSC,NBADSC                                                    
         BZ    DPAGE02                                                          
         GOTO1 DISTAB,BOPARM,NBADSC,NBAEL,NBATAB                                
         B     DPAGE02                                                          
*                                                                               
DPAGE10  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE A TABLE ENTRY                                              *         
*                                                                     *         
* NTRY: P1=A(CONDSCH IN TWA)                                          *         
*       P2=A(BLFEL LIST ENTRY)                                        *         
*       P3=A(FMTTAB TABLE ENTRY)                                      *         
***********************************************************************         
         SPACE 1                                                                
VALTAB   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         USING CONDSCH,R2                                                       
         USING BLFELD,R3                                                        
         USING FMTTABD,R4                                                       
*                                                                               
         MVC   BLFLINF,FMTLINF                                                  
         MVC   BLFLINN,FMTLINN                                                  
         MVC   BLFCOLF,FMTCOLF                                                  
         MVC   BLFCOLN,FMTCOLN                                                  
         MVI   BLFHED1,AC#ESCL                                                  
         MVI   BLFOPT1,BLFOLFTQ                                                 
         TM    FMTINDS1,FMTINUM                                                 
         BZ    *+8                                                              
         MVI   BLFOPT1,BLFORGTQ                                                 
         MVC   BLFDATAC,BLFDATA    COPY FOR COST ELEMENT                        
         GOTO1 VALLINE,BOPARM,CONTLINH,CONTOPTH,BLFELD,FMTTABD                  
         BNE   EXITN                                                            
         CLI   BLFLINF,0                                                        
         BE    *+10                                                             
         MVC   BLFDATAC,BLFDATA                                                 
         GOTO1 (RF),(R1),CONCLINH,CONCOPTH,BLFELC,FMTTABD                       
         BNE   EXITN                                                            
*                                                                               
         B     EXITY                                                            
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY TABLE ENTRY                                                 *         
*                                                                     *         
* NTRY: P1=A(CONDSCH IN TWA)                                          *         
*       P2=A(BLFEL LIST ENTRY)                                        *         
*       P3=A(FMTTAB TABLE ENTRY)                                      *         
***********************************************************************         
         SPACE 1                                                                
DISTAB   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         USING CONDSCH,R2                                                       
         USING BLFELD,R3                                                        
         USING FMTTABD,R4                                                       
*                                                                               
         MVI   CONDSC,AC#ESCL                                                   
         MVC   CONDSC+1(2),FMTDIC                                               
         MVI   CONDSC+3,L'CONDSC                                                
*                                                                               
         MVCDD CONTWRD,AC#TIME                                                  
         MVCDD CONCWRD,AC#COST                                                  
*                                                                               
         GOTO1 DISLINE,BOPARM,CONTLINH,CONTOPTH,BLFELD,FMTTABD                  
         GOTO1 (RF),(R1),CONCLINH,CONCOPTH,BLFELC,FMTTABD                       
*                                                                               
         B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE A LINE                                                     *         
*                                                                     *         
* NTRY: P1=A(FIRST FIELD FOR LINE)                                    *         
*       P2=A(LAST FIELD FOR LINE)                                     *         
*       P3=A(ELEMENT)                                                 *         
*       P4=A(TABLE ENTRY)                                             *         
***********************************************************************         
         SPACE 1                                                                
VALLINE  NTR1  ,                                                                
         LM    R2,R5,0(R1)                                                      
         USING BLFELD,R4                                                        
         LR    R6,R2                                                            
         USING FHD,R6                                                           
         XR    R0,R0                                                            
         XR    R2,R2                                                            
VLINE02  ICM   R2,1,FHLN                                                        
         BZ    VLINE10                                                          
         TM    FHAT,FHATXH                                                      
         BZ    VLINE08                                                          
         TM    FHII,FHIIVA                                                      
         BO    *+6                                                              
         BCTR  R0,0                                                             
         GOTO1 VALFLD,BOPARM,FHD,(R4),(R5)                                      
         BNE   EXITL                                                            
VLINE08  BXLE  R6,R2,VLINE02                                                    
*                                                                               
VLINE10  LTR   R0,R0                                                            
         BZ    EXITY                                                            
         LTR   R5,R5                                                            
         BZ    EXITY                                                            
         LA    RF,CCHGS            UPDATE LIST OF LINE CHANGES                  
         OC    0(L'CCHGS,RF),0(RF)                                              
         BZ    *+12                                                             
         LA    RF,L'CCHGS(RF)                                                   
         B     *-14                                                             
         MVC   0(L'CCHGS,RF),BLFFLD                                             
         B     EXITY                                                            
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY A LINE                                                      *         
*                                                                     *         
* NTRY: P1=A(FIRST FIELD FOR LINE)                                    *         
*       P2=A(LAST FIELD FOR LINE)                                     *         
*       P3=A(ELEMENT)                                                 *         
*       P4=A(TABLE ENTRY)                                             *         
***********************************************************************         
         SPACE 1                                                                
DISLINE  NTR1  ,                                                                
         LM    R2,R5,0(R1)                                                      
         LR    R6,R2                                                            
         USING FHD,R6                                                           
         XR    R2,R2                                                            
DLINE02  ICM   R2,1,FHLN                                                        
         BZ    DLINE10                                                          
         TM    FHAT,FHATXH                                                      
         BZ    DLINE08                                                          
         GOTO1 DISFLD,BOPARM,FHD,(R4),(R5)                                      
DLINE08  BXLE  R6,R2,DLINE02                                                    
*                                                                               
DLINE10  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A FIELD                                         *         
*                                                                     *         
* NTRY: P1=A(FIELD)                                                   *         
*       P2=A(ELEMENT OR ERR THING)                                    *         
*       P3=A(FMTTAB ENTRY)                                            *         
***********************************************************************         
         SPACE 1                                                                
VALFLD   NTR1  ,                                                                
         LM    R4,R6,0(R1)                                                      
         USING FHD,R4                                                           
         NI    CINDS,FF-CIDISERR                                                
         TM    FHII,FHIIVA                                                      
         BO    *+8                                                              
         OI    CINDS,CICHG                                                      
         GOTO1 AFVAL,FHD                                                        
         BAS   RE,VALIDATE                                                      
         BNE   VALFLDN                                                          
         GOTO1 DISFLD,BOPARM,(R4),(R5),(R6)                                     
         B     EXITY                                                            
VALFLDN  TM    CINDS,CIDISERR      TEST DISPLAY ERROR                           
         BZ    VALFLDN2                                                         
         GOTO1 DISFLD,BOPARM,(R4),(R5),(R6)                                     
VALFLDN2 NI    FHII,FF-FHIIVA                                                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXITN                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   FVILEN,0                                                         
         BNE   EXITN                                                            
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITN                                                            
         DROP  R4                                                               
*                                                                               
VALIDATE NTR1  ,                                                                
         XR    RF,RF                                                            
         IC    RF,FVIXNU                                                        
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     EXITY                                                            
         B     VALFMT              FORMAT NUMBER                                
         B     VALLNG              LANGUAGE                                     
         B     VALGRB              GROUPING BASIS                               
         B     VALNAM              NAME                                         
         B     VALLIN              LINE NUMBERS    - TIME                       
         B     VALCOL              COLUMN NUMBERS  - TIME                       
         B     VALHED1             HEADING 1       - TIME                       
         B     VALHED2             HEADING 2       - TIME                       
         B     VALOPT              OPTIONS         - TIME                       
         B     VALLIN              LINE NUMBERS    - COST                       
         B     VALCOL              COLUMN NUMBERS  - COST                       
         B     VALHED1             HEADING 1       - COST                       
         B     VALHED2             HEADING 2       - COST                       
         B     VALOPT              OPTIONS         - COST                       
*                                                                               
         USING PBCRECD,R5                                                       
VALFMT   TM    FVIIND,FVINUM                                                    
         BZ    EXITN                                                            
         OC    BCFULL(3),BCFULL                                                 
         BNZ   EXITN                                                            
         CLI   BCFULL+3,0                                                       
         BE    EXITN                                                            
         MVC   PBCKFMT,BCFULL+3                                                 
         B     EXITY                                                            
*                                                                               
VALLNG   CLI   FVILEN,0                                                         
         BE    EXITN                                                            
*                                                                               
         L     R2,ALANG                                                         
         LA    R2,6(R2)                                                         
         USING LANGTABD,R2                                                      
VLNG02   CLI   LANGTABD,FF                                                      
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFELANG)                                           
         B     EXITN                                                            
         GOTO1 CMPWRD,LANGSHR                                                   
         BE    VLNG10                                                           
         GOTO1 CMPWRD,LANGSHRN                                                  
         BE    VLNG10                                                           
         GOTO1 CMPWRD,LANGFUL                                                   
         BE    VLNG10                                                           
         GOTO1 CMPWRD,LANGFULN                                                  
         BE    VLNG10                                                           
VLNG08   LA    R2,LANGTABL(R2)                                                  
         B     VLNG02                                                           
*                                                                               
VLNG10   MVC   PBCKLANG,LANGCODE                                                
         B     EXITY                                                            
         DROP  R2                                                               
*                                                                               
VALGRB   TM    FVIIND,FVINUM                                                    
         BZ    EXITN                                                            
         OC    BCFULL(3),BCFULL                                                 
         BNZ   EXITN                                                            
         CLI   BCFULL+3,0                                                       
         BE    EXITN                                                            
         MVC   PBCRGRPB,BCFULL+3                                                
         GOTO1 AGETGRB,BOPARM,PBCRGRPB,0                                        
         B     EXIT                                                             
*                                                                               
VALNAM   CLI   FVILEN,0                                                         
         BE    EXITN                                                            
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('NAMELQ',PBCRECD),0                 
         XC    BOELEM,BOELEM                                                    
         LA    R2,BOELEM                                                        
         USING NAMELD,R2                                                        
         MVI   NAMEL,NAMELQ                                                     
         IC    RF,FVILEN                                                        
         LA    RF,NAMLN1Q(RF)                                                   
         STC   RF,NAMLN                                                         
         MVC   NAMEREC,FVIFLD                                                   
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),PBCRECD,NAMELD                       
         B     EXITY                                                            
         DROP  R2                                                               
*                                                                               
         USING FMTTABD,R6                                                       
         USING BLFELD,R5                                                        
VALLIN   CLI   FVIFLD,C'*'                                                      
         BNE   VLIN02                                                           
         XC    BLFLINF(BLFLNQ-(BLFLINF-BLFELD)),BLFLINF                         
         B     EXITY                                                            
VLIN02   CLI   FVILEN,0                                                         
         BE    EXITY                                                            
         NI    BLFOPT2,FF-BLFOBOT                                               
         GOTO1 VALNOS,BOPARM,BLFLINF,BLFLINN,9,FMTLINMN,A(AE$PBMAX)             
         BL    EXITN               CC=LOW FOR ERROR                             
         BE    EXITY               CC=EQUAL FOR OKAY AND TOP                    
         NI    CINDS,FF-CIDISERR                                                
         TM    FMTINDS1,FMTITFP    BOTTOM ONLY ALLOWED FOR TOTALS               
         BZ    EXITN                                                            
         OI    BLFOPT2,BLFOBOT     CC=HIGH AND FROM THE BOTTOM                  
         B     EXITY                                                            
*                                                                               
VALCOL   CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         CLI   FVILEN,0                                                         
         BE    EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,BOFMAXWD                                                      
         GOTO1 VALNOS,BOPARM,BLFCOLF,BLFCOLN,(RF),FMTCOLMN,A(AE$FNFPL)          
         B     EXIT                                                             
*                                                                               
VALHED1  CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         CLI   FVILEN,0                                                         
         BE    EXITY                                                            
         MVC   BLFHED1,FVIFLD                                                   
         CLI   BLFHED1,C'*'                                                     
         BNE   EXITY                                                            
         MVC   BLFHED1+1(L'BLFHED1-1),BCSPACES                                  
         B     EXITY                                                            
*                                                                               
VALHED2  CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         CLI   FVILEN,0            TEST DEFAULT                                 
         BE    VHED202                                                          
*                                                                               
         MVC   BLFHED2,FVIFLD                                                   
         CLI   BLFHED2,C'*'                                                     
         BNE   EXITY                                                            
         MVC   BLFHED2+1(L'BLFHED2-1),BCSPACES                                  
         B     EXITY                                                            
*                                                                               
VHED202  CLI   BLFTYPE,BLFTCSTQ    TEST COST ELEMENT                            
         BNE   VHED204                                                          
         LA    RF,BLFELD                                                        
         SH    RF,=Y(BLFLNQ)       RF=A(TIME ELEMENT)                           
         CLI   BLFLINF-BLFELD(RF),0                                             
         BE    VHED204                                                          
         CLC   BLFHED1,BLFHED1-BLFELD(RF)                                       
         BNE   VHED204             IF HEADING 1 IS THE SAME                     
         MVC   BLFHED2,BLFHED2-BLFELD(RF) HEADING 2 IS THE SAME                 
         B     EXITY                                                            
*                                                                               
VHED204  MVI   BLFHED2,AC#ESUL     SET DEFUAULT AS UNDERLINING                  
         CLI   BLFHED1,C'*'        TEST HEADING 1 DEFINED                       
         BNE   EXITY                                                            
         MVI   BLFHED2,AC#ESCL     NO - SET DEFAULT TO DICTIONARY               
         B     EXITY                                                            
*                                                                               
VALOPT   LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         CLI   FVILEN,0                                                         
         BE    VALOPT02                                                         
         MVI   BLFOPT1,BLFOLFTQ                                                 
         GOTO1 CMPWRD,UC@LEFT                                                   
         BE    VALOPT02                                                         
         MVI   BLFOPT1,BLFOCTRQ                                                 
         GOTO1 CMPWRD,UC@CENTR                                                  
         BE    VALOPT02                                                         
         MVI   BLFOPT1,BLFORGTQ                                                 
         GOTO1 CMPWRD,UC@RIGHT                                                  
         BE    VALOPT02                                                         
         MVI   BLFOPT1,BLFOMAXQ                                                 
         GOTO1 CMPWRD,UC@MMIZE                                                  
         BNE   EXITN                                                            
*                                                                               
VALOPT02 OI    CINDS,CIDISERR                                                   
         GOTO1 AFMTHED,BOPARM,(1,BLFELD)                                        
         BH    EXITN                                                            
         GOTO1 (RF),(R1),(2,BLFELD)                                             
         BH    EXITN                                                            
         B     EXITY                                                            
         DROP  R4                                                               
         SPACE 1                                                                
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY A FILED                                          *         
*                                                                     *         
* NTRY: P1=(FIELD NO., A(FIELD)                                       *         
*       P2=A(ELEMENT OR ERR THING)                                    *         
*       P3=A(FMTTAB ENTRY)                                            *         
***********************************************************************         
         SPACE 1                                                                
DISFLD   NTR1  ,                                                                
         LM    R4,R6,0(R1)                                                      
         USING FHD,R4                                                           
         XR    RF,RF                                                            
         ICM   RF,1,0(R1)                                                       
         BNZ   DFLD02                                                           
         IC    RF,FHLN                                                          
         SH    RF,=Y(FHDAD)                                                     
         LA    RE,FHD(RF)                                                       
         IC    RF,0(RE)                                                         
DFLD02   MVC   FVIFLD,BCSPACES                                                  
         NI    FHII,FF-FHIIVA                                                   
         BAS   RE,DISPLAY                                                       
         BNE   *+8                                                              
         OI    FHII,FHIIVA                                                      
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),FVIFLD                                                   
         OI    FHOI,FHOITR                                                      
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
DISPLAY  NTR1  ,                                                                
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     EXITY                                                            
         B     DISFMT              FORMAT NUMBER                                
         B     DISLNG              LANGUAGE                                     
         B     DISGRB              GROUPING BASIS                               
         B     DISNAM              NAME                                         
         B     DISLIN              LINE NUMBERS    - TIME                       
         B     DISCOL              COLUMN NUMBERS  - TIME                       
         B     DISHED1             HEADING 1       - TIME                       
         B     DISHED2             HEADING 2       - TIME                       
         B     DISOPT              OPTIONS         - TIME                       
         B     DISLIN              LINE NUMBERS    - COST                       
         B     DISCOL              COLUMN NUMBERS  - COST                       
         B     DISHED1             HEADING 1       - COST                       
         B     DISHED2             HEADING 2       - COST                       
         B     DISOPT              OPTIONS         - COST                       
*                                                                               
         USING PBCRECD,R5                                                       
DISFMT   EDIT  PBCKFMT,(3,FVIFLD),0,ALIGN=LEFT                                  
         B     EXITY                                                            
*                                                                               
DISLNG   L     R2,ALANG                                                         
         LA    R2,6(R2)                                                         
         USING LANGTABD,R2                                                      
DLNG02   CLI   LANGTABD,FF                                                      
         BE    EXITN                                                            
         CLC   LANGCODE,PBCKLANG                                                
         BE    DLNG10                                                           
         LA    R2,LANGTABL(R2)                                                  
         B     DLNG02                                                           
*                                                                               
DLNG10   MVC   FVIFLD(L'LANGFULN),LANGFULN                                      
         B     EXITY                                                            
         DROP  R2                                                               
*                                                                               
DISGRB   EDIT  PBCRGRPB,(3,FVIFLD),0,ALIGN=LEFT                                 
         GOTO1 AGETGRB,BOPARM,PBCRGRPB,(L'CONGRBD,CONGRBD)                      
         OI    CONGRBDH+FHOID,FHOITR                                            
         B     EXITY                                                            
*                                                                               
DISNAM   GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('NAMELQ',PBCRECD),0                 
         CLI   12(R1),0                                                         
         BNE   EXITN                                                            
         L     R2,12(R1)                                                        
         USING NAMELD,R2                                                        
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   FVIFLD(0),NAMEREC                                                
         B     EXITY                                                            
         DROP  R2                                                               
*                                                                               
         USING FMTTABD,R6                                                       
         USING BLFELD,R5                                                        
DISLIN   CLI   BLFLINF,0                                                        
         BNE   *+12                                                             
         MVI   FVIFLD,C'*'                                                      
         B     EXITY                                                            
         XR    RF,RF                                                            
         CLI   FMTLINMN,1                                                       
         BNE   DLIN02                                                           
         CLI   FMTLINMX,1                                                       
         BNE   DLIN02                                                           
         LA    RF,X'80'                                                         
DLIN02   GOTO1 DISNOS,BOPARM,((RF),BLFLINF),BLFLINN                             
         TM    BLFOPT2,BLFOBOT                                                  
         BZ    EXITY                                                            
         LA    RF,FVIFLD+3                                                      
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         B     EXITY                                                            
*                                                                               
DISCOL   CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         GOTO1 DISNOS,BOPARM,BLFCOLF,BLFCOLN                                    
         B     EXITY                                                            
*                                                                               
DISHED1  LA    R2,BLFHED1                                                       
         B     DISHED                                                           
DISHED2  LA    R2,BLFHED2                                                       
*                                                                               
DISHED   CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         CLI   0(R2),AC#ESUL       TEST UNDERLINING                             
         BE    HEDUND                                                           
         CLI   0(R2),AC#ESCL       TEST DICTIONARY EQUATE                       
         BNE   DISHEDX                                                          
*                                                                               
HEDDIC   MVC   1(L'BLFHED1-1,R2),BCSPACES                                       
         MVC   1(2,R2),FMTDIC                                                   
         CLI   BLFCOLN,3                                                        
         BH    HEDDIC02                                                         
         MVI   0(R2),AC#ESCL3                                                   
         BE    HEDDIC04                                                         
         MVI   0(R2),AC#ESCL2                                                   
         B     HEDDIC04                                                         
HEDDIC02 MVC   3(1,R2),BLFCOLN                                                  
         CLI   BLFCOLN,L'BLFHED1                                                
         BNH   *+8                                                              
         MVI   3(R2),L'BLFHED1                                                  
         TM    FMTINDS1,FMTINUM                                                 
         BZ    HEDDIC04                                                         
         IC    RE,3(R2)                                                         
         BCTR  RE,0                                                             
         STC   RE,3(R2)                                                         
HEDDIC04 GOTO1 ,BOPARM,C'SL  ',(R2)                                             
         MVC   3(1,R1),CRECKEY+(PBCKLANG-PBCRECD)                               
         GOTO1 VDICTAT                                                          
         B     DISHEDX                                                          
*                                                                               
HEDUND   MVC   BLFHED2,BLFHED1                                                  
         LA    R0,L'BLFHED2                                                     
         LA    RF,BLFHED1+L'BLFHED1-1                                           
HEDUND02 CLI   0(RF),C' '                                                       
         BH    HEDUND04                                                         
         BCTR  RF,0                                                             
         BCT   R0,HEDUND02                                                      
         B     DISHEDX                                                          
HEDUND04 MVI   L'BLFHED1(RF),C'-'                                               
         BCTR  RF,0                                                             
         BCT   R0,HEDUND04                                                      
HEDUND06 CLI   1(RF),C' '                                                       
         BH    DISHEDX                                                          
         MVI   L'BLFHED1+1(RF),C' '                                             
         LA    RF,1(RF)                                                         
         B     HEDUND06                                                         
*                                                                               
DISHEDX  MVC   FVIFLD(L'BLFHED1),0(R2)                                          
         B     EXITY                                                            
*                                                                               
DISOPT   LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         MVC   FVIFLD(L'UC@LEFT),UC@LEFT                                        
         TM    BLFOPT1,BLFOLFTQ                                                 
         BO    EXITY                                                            
         MVC   FVIFLD(L'UC@CENTR),UC@CENTR                                      
         TM    BLFOPT1,BLFOCTRQ                                                 
         BO    EXITY                                                            
         MVC   FVIFLD(L'UC@RIGHT),UC@RIGHT                                      
         TM    BLFOPT1,BLFORGTQ                                                 
         BO    EXITY                                                            
         MVC   FVIFLD(L'UC@MMIZE),UC@MMIZE                                      
         TM    BLFOPT1,BLFOMAXQ                                                 
         BO    EXITY                                                            
         MVC   FVIFLD,BCSPACES                                                  
         B     EXITN                                                            
         DROP  R4                                                               
         SPACE 1                                                                
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO VALIDATE TWO NUMBERS SEPERATED BY A COMMA               *         
*                                                                     *         
* NTRY: P1 = (1ST NUMBER DEFAULT, A(OUTPUT FOR 1ST NUMBER))           *         
*       P2 = (2ND NUMBER DEFAULT, A(OUTPUT FOR 2ND NUMBER))           *         
*       P3 = MAXIMUM LINE/COLUMN NUMBER                               *         
*       P4 = A(MIMIMUM/MAXIMUM)                                       *         
*       P5 = BASE ERROR MESSAGE NUMBER                                *         
*   FVIFLD = INPUT                                                    *         
* EXIT: CC = LOW IF ERROR                                             *         
*       CC = HIGH IF OKAY AND FIRST NUMBER IS NEGATIVE                *         
*       CC = EQUAL IF OKAY AND FIRST NUMBER IS POSITIVE               *         
***********************************************************************         
         SPACE 1                                                                
VALNOS   NTR1  ,                                                                
         LM    R2,R6,0(R1)                                                      
*                                                                               
         XC    BOELEM,BOELEM                                                    
         ICM   RF,12,=C',='                                                     
         ICM   RF,2,BCCOMMA                                                     
         ICM   RF,1,BCEQUAL                                                     
         GOTO1 VSCANNER,BOPARM,FVIHDR,(2,BOELEM),(RF)                           
*                                                                               
         GOTO1 VALNO,BOPARM,BOELEM,(R2)                                         
         BL    EXITL                                                            
         GOTO1 (RF),(R1),BOELEM+SCBLKLQ,(R3)                                    
         BNE   EXITL                                                            
*                                                                               
         CLM   R4,1,0(R2)          TEST FIRST NUMBER OKAY                       
         BNL   *+12                                                             
         STCM  R6,3,FVMSGNO                                                     
         B     EXITL                                                            
*                                                                               
         CLC   0(1,R3),0(R5)       TEST MINIMUM                                 
         BL    *+14                                                             
         CLC   0(1,R3),1(R5)       TEST MAXIMUM                                 
         BNH   VNOS10                                                           
         L     R1,NBATAB           MINUMUM/MAXIMUM ERROR                        
         GOTO1 SUBDESC,FMTDIC-FMTTABD(R1)                                       
         GOTO1 SUBNUM,0(R5)                                                     
         LA    R6,1(R6)                                                         
         CLC   0(1,R5),1(R5)       TEST MINIMUM=MAXIMUM                         
         BE    VNOS02                                                           
         LA    R6,1(R6)                                                         
         GOTO1 SUBNUM,1(R5)                                                     
VNOS02   STCM  R6,3,FVMSGNO                                                     
         B     EXITL                                                            
*                                                                               
VNOS10   OI    CINDS,CIDISERR      TEST SUM OF NUMBERS OKAY                     
         XR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         XR    RF,RF                                                            
         IC    RF,0(R3)                                                         
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         CR    R4,RE                                                            
         BNL   *+12                                                             
         STCM  R6,3,FVMSGNO                                                     
         B     EXITL                                                            
*                                                                               
VALNOSY  CLI   BOELEM+(SC1STVAL-SCANBLKD),FF                                    
         BE    EXITH               CC=HIGH IF FIRST NUMBER IS NEGATIVE          
         B     EXITY                                                            
*                                                                               
VALNO    NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING SCANBLKD,R2                                                      
         MVI   SC1STVAL,0                                                       
         CLI   SC2NDLEN,0                                                       
         BNE   EXITL                                                            
         CLI   SC1STLEN,0                                                       
         BE    VALNOX                                                           
*                                                                               
         LA    R1,SC1STFLD         R1=A(INPUT)                                  
         XR    R0,R0                                                            
         IC    R0,SC1STLEN         R0=L(INPUT)                                  
         CLI   0(R1),C'-'          TEST 1ST CHARACTER FOR MINUS                 
         BNE   VALNO02                                                          
         LA    R1,1(R1)                                                         
         B     VALNO04                                                          
VALNO02  LR    RF,R0               TEST LAST CHARACTER FOR MINUS                
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         CLI   0(RF),C'-'                                                       
         BNE   VALNO10                                                          
*                                                                               
VALNO04  BCTR  R0,0                                                             
         MVI   SC1STVAL,FF         SET NUMBER IS NEGATIVE                       
*                                                                               
VALNO10  XR    RF,RF                                                            
         LTR   R0,R0                                                            
         BZ    EXITL                                                            
*                                                                               
VALNO12  TM    0(R1),C'0'          TEST NUMERICAL FIGURE                        
         BNO   EXITL                                                            
         IC    RE,0(R1)                                                         
         SLL   RE,28                                                            
         SRL   RE,28                                                            
         MH    RF,=Y(10)                                                        
         AR    RF,RE                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VALNO12                                                       
*                                                                               
         LA    RE,256                                                           
         CR    RF,RE                                                            
         BNL   EXITL                                                            
         STC   RF,0(R3)                                                         
*                                                                               
VALNOX   CLI   0(R3),0             CHECK NON-ZERO                               
         BE    EXITL                                                            
         CLI   SC1STVAL,FF                                                      
         BE    EXITH               CC=HIGH IF NUMBER IS NEGATIVE                
         B     EXITY                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
*  ROUTINE TO DISPLAY TWO NUMBERS SEPERATED BY A COMMA                *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' TO IGNORE 2ND NUMBER IF = 1                 *         
*             1-3 = A(1ST NUMBER)                                     *         
*       P2=       = A(2ND NUMBER)                                     *         
* EXIT: FVIFLD=OUTPUT                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISNOS   NTR1  ,                                                                
         LR    R4,R1               R4=A(PARAMETER LIST)                         
         LM    R2,R3,0(R4)                                                      
         LA    RF,FVIFLD                                                        
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         BAS   RE,DISNO                                                         
         TM    0(R4),X'80'         TEST IGNORE 2ND NUMBER IF = 1                
         BZ    *+12                                                             
         CLI   0(R3),1                                                          
         BE    EXIT                                                             
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BNE   *-8                                                              
         MVC   0(1,RF),BCCOMMA                                                  
         LA    RF,1(RF)                                                         
         IC    R1,0(R3)                                                         
         BAS   RE,DISNO                                                         
         B     EXIT                                                             
*                                                                               
DISNO    CVD   R1,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  0(3,RF),BODUB1                                                   
         CLI   0(RF),C'0'                                                       
         BNER  RE                                                               
         MVC   0(3,RF),1(RF)                                                    
         CLI   0(RF),C'0'                                                       
         BNER  RE                                                               
         MVC   0(3,RF),1(RF)                                                    
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CLEAR LIST                                                          *         
***********************************************************************         
         SPACE 1                                                                
CLRLST   NTR1  ,                                                                
         XC    NBPARMS,NBPARMS                                                  
*                                                                               
CREC02   BAS   RE,NXTBLF                                                        
         BL    CREC10                                                           
         LM    R2,R3,NBPARMS                                                    
         USING FMTTABD,R2                                                       
         USING BLFELD,R3                                                        
         MVI   BLFLINF,00                                                       
         MVI   BLFLINF+BLFLNQ,00                                                
         B     CREC02                                                           
*                                                                               
CREC10   B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET UP PRINT BLOCK (AND SET OVERLAP ERRORS)              *         
***********************************************************************         
         SPACE 1                                                                
PRTSET   NTR1  ,                                                                
         LA    RE,PBLOCK           SPACERIZE PRINT BLOCK                        
         LA    RF,PBLOCKL                                                       
         LA    R3,C' '                                                          
         SLL   R3,24                                                            
         MVCL  RE,R2                                                            
         LTR   R1,R1               TEST CALL TO FIND OVERLAY ERROR              
         BZ    *+12                                                             
         MVI   0(R1),FF            FIND ERROR HERE                              
         B     PSET10                                                           
*                                                                               
         XC    NBPARMS,NBPARMS     FIND TOP/BOTTOM DISPLACEMENTS                
         XC    CTIME,CTIME                                                      
         XC    CCOST,CCOST                                                      
PSET02   BAS   RE,NXTBLF                                                        
         BL    PSET04                                                           
         L     R1,NBAEL                                                         
         GOTO1 SETMAX,(R1)                                                      
         GOTO1 (RF),BLFLNQ(R1)                                                  
         B     PSET02                                                           
PSET04   GOTO1 SETDSP,BLFTTIMQ                                                  
         BNE   EXITN                                                            
         GOTO1 (RF),BLFTCSTQ                                                    
         BNE   EXITN                                                            
*                                                                               
PSET10   XC    NBPARMS,NBPARMS                                                  
PSET12   BAS   RE,NXTBLF                                                        
         BL    PSET20                                                           
         L     R2,NBATAB                                                        
         USING FMTTABD,R2                                                       
*                                                                               
         MVI   CPRTFLD,AC#ESCL                                                  
         MVC   CPRTFLD+1(2),FMTDIC                                              
         GOTO1 VDICTAT,BOPARM,C'SL  ',CPRTFLD                                   
         NI    CPRTFLD,FF-X'40'                                                 
         MVC   CPRTFLD+1(L'CPRTFLD-1),CPRTFLD                                   
*                                                                               
         TM    FMTINDS1,FMTINUM    TEST A NUMBER                                
         BZ    PSET18                                                           
         MVI   BODUB1,X'99'                                                     
         MVC   BODUB1+1(6),BODUB1                                               
         MVI   BODUB1+7,X'9D'                                                   
*                                                                               
         XC    BODUB2,BODUB2       SET NO. OF DECIMAL PLACES                    
         MVC   BODUB2+3(1),FMTNDECP                                             
         CLI   FMTNDECP,FMTNCURQ   OR AS PER CURRENCY                           
         BNE   *+10                                                             
         MVC   BODUB2,CSCURBIL                                                  
         CURED (P8,BODUB1),(16,CPRTFLD+116),BODUB2,COMMAS=YES,         *        
               MINUS=YES                                                        
*                                                                               
         LA    R0,16                                                            
         LA    RF,CPRTFLD+L'CPRTFLD-1                                           
PSET14   CLI   0(RF),C'9'                                                       
         BNE   *+10                                                             
         MVC   0(1,RF),CPRTFLD                                                  
         BCTR  RF,0                                                             
         BCT   R0,PSET14                                                        
*                                                                               
PSET18   GOTO1 PRTELM,BLFTTIMQ                                                  
         BNE   EXITN                                                            
         GOTO1 (RF),BLFTCSTQ                                                    
         BNE   EXITN                                                            
         B     PSET12                                                           
*                                                                               
PSET20   B     EXITY                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO SET TOP/BOTTOM MAXIMUM LINE NUMBERS                      *         
*                                                                     *         
* NTRY: P1 = A(BLFEL ELEMENT)                                         *         
***********************************************************************         
         SPACE 1                                                                
SETMAX   NTR1  ,                                                                
         LR    R3,R1                                                            
         USING BLFELD,R3                                                        
         LA    R4,CTIME                                                         
         CLI   BLFTYPE,BLFTCSTQ                                                 
         BNE   *+8                                                              
         LA    R4,CCOST                                                         
         USING CTIME,R4                                                         
         CLI   BLFLINF,0                                                        
         BE    EXIT                                                             
         CLI   BLFFLD,BLFFGRSQ                                                  
         BE    SETMAX02                                                         
         CLI   CTTOTTYP,0                                                       
         BNE   SETMAX04                                                         
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   SETMAX04                                                         
SETMAX02 MVC   CTTOTTYP,BLFFLD                                                  
SETMAX04 IC    RF,BLFLINF                                                       
         IC    RE,BLFLINN                                                       
         BCTR  RE,0                                                             
         AR    RF,RE                                                            
         LA    R1,CTMAXTOP                                                      
         TM    BLFOPT2,BLFOBOT                                                  
         BZ    *+8                                                              
         LA    R1,CTMAXBOT                                                      
         CLM   RF,1,0(R1)                                                       
         BNH   *+8                                                              
         STC   RF,0(R1)                                                         
         B     EXIT                                                             
         DROP  R3,R4                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO SET DISPLACEMENT THINGIES FROM OTHER THINGIES            *         
*                                                                     *         
* NTRY: R1 = BLFTTIMQ OR BLFTCSTQ                                     *         
***********************************************************************         
         SPACE 1                                                                
SETDSP   NTR1  ,                                                                
         STC   R1,BOBYTE1                                                       
         LA    R2,CTIME                                                         
         CLI   BOBYTE1,BLFTCSTQ                                                 
         BNE   *+8                                                              
         LA    R2,CCOST                                                         
         USING CTIME,R2                                                         
         CLI   CTTOTTYP,0                                                       
         BNE   SETDSP02                                                         
         MVC   FVMSGNO,=AL2(AE$TFDEF)                                           
         MVI   BOHALF1,BLFFGRSQ                                                 
         MVI   BOHALF2,BLFFNETQ                                                 
         GOTO1 PRTERR,BOPARM,BOBYTE1,BOHALF1,BOHALF2                            
         B     EXITN                                                            
SETDSP02 MVI   CTDISTOP,0          TOP DISPLACEMENT = 0                         
         MVC   CTDISBOT,CTMAXTOP   BOTTOM DISPLACEMENT = TOP MAX.               
         CLC   CTMAXBOT,CTMAXTOP   TEST BOTTOM MAX > TOP MAX                    
         BNH   EXITY                                                            
         MVC   CTDISBOT,CTMAXBOT   BOTTOM DISPLACEMNT = BOTTOM MAX.             
         IC    RF,CTMAXBOT                                                      
         IC    RE,CTMAXTOP         TOP DISLACEMENT                              
         SR    RF,RE                 = BOTTOM MAX - TOP MAX                     
         STC   RF,CTDISTOP                                                      
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT ELEMENT INTO PRINT BLOCK                             *         
***********************************************************************         
         SPACE 1                                                                
PRTELM   NTR1  ,                                                                
*                                                                               
         L     R3,NBAEL                                                         
         USING BLFELD,R3           R3=A(TIME/COST ELEMENT)                      
         CLM   R1,1,BLFTYPE                                                     
         BE    *+8                                                              
         LA    R3,BLFLNQ(R3)                                                    
         CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
*                                                                               
         IC    RE,BLFCOLF                                                       
         IC    RF,BLFCOLN                                                       
         AR    RF,RE                                                            
         BCTR  RF,0                                                             
         STC   RF,CCOLL            SAVE END POSITON OF LINE (1-132)             
*                                                                               
         LA    RF,CTIME                                                         
         CLI   BLFTYPE,BLFTCSTQ                                                 
         BNE   *+8                                                              
         LA    RF,CCOST                                                         
         USING CTIME,RF                                                         
         XR    R5,R5                                                            
         IC    R5,BLFLINF                                                       
         TM    BLFOPT2,BLFOBOT                                                  
         BZ    PELM02                                                           
         BCTR  R5,0                                                             
         LNR   R5,R5                                                            
         XR    RE,RE                                                            
         IC    RE,CTDISBOT                                                      
         AR    R5,RE                                                            
         B     PELM04                                                           
PELM02   XR    RE,RE                                                            
         IC    RE,CTDISTOP                                                      
         AR    R5,RE                                                            
         DROP  RF                                                               
*                                                                               
PELM04   MH    R5,=Y(L'PTL1)                                                    
         LA    R5,PTL1-L'PTL1-1(R5)                                             
         CLI   BLFTYPE,BLFTCSTQ                                                 
         BNE   *+8                                                              
         LA    R5,PCH1-PTH1(R5)                                                 
*                                                                               
         XR    RE,RE                                                            
         IC    RE,CCOLL                                                         
         AR    R5,RE               R5=END POSN. OF THE LINE                     
*                                                                               
         XR    R0,R0                                                            
         IC    R0,BLFLINN                                                       
PELM12   LR    R2,R5               R2=END OF FIELD                              
         LA    R1,CPRTFLD+L'CPRTFLD-1                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,BLFCOLN                                                       
PELM14   CLI   0(R2),C' '          TEST FOR OVERLAP                             
         BE    PELM18                                                           
         MVC   FVMSGNO,=AL2(AE$TFOVR)                                           
         B     PRTELMN                                                          
PELM18   MVC   0(1,R2),0(R1)                                                    
         BCTR  R1,0                                                             
         BCTR  R2,0                                                             
         BCT   RF,PELM14                                                        
*                                                                               
         CLI   BLFCOLF,1           TEST TOUCHED ON THE LEFT                     
         BE    PELM20                                                           
         CLI   0(R2),C' '                                                       
         BE    PELM20                                                           
         MVC   FVMSGNO,=AL2(AE$TFTCH)                                           
         LA    R2,1(R2)                                                         
         B     PRTELMN                                                          
*                                                                               
PELM20   CLI   CCOLL,L'REPP1       TEST TOUCHED ON THE RIGHT                    
         BE    PELM28                                                           
         LR    R2,R5                                                            
         LA    R2,1(R2)                                                         
         CLI   0(R2),C' '                                                       
         BE    PELM28                                                           
         MVC   FVMSGNO,=AL2(AE$TFTCH)                                           
         BCTR  R2,0                                                             
         B     PRTELMN                                                          
*                                                                               
PELM28   LA    R5,L'PTL1(R5)                                                    
         BCT   R0,PELM12                                                        
*                                                                               
         GOTO1 PRTHED,1            PRINT HEAD LINE 1                            
         BNE   PRTELMN                                                          
         GOTO1 (RF),2              PRINT HEAD LINE 2                            
         BNE   PRTELMN                                                          
*                                                                               
PRTELMY  B     EXITY                                                            
*                                                                               
PRTELMN  MVC   COVRLAP1,BLFFLD                                                  
         OC    COVRLAP2,COVRLAP2                                                
         BNZ   EXITN               TEST ALREADY RECURSED                        
*                                                                               
         MVC   COVRLAP2,COVRLAP1                                                
         GOTO1 PRTSET,(R2)         RECURSE TO FIND 1ST OVERLAP FIELD            
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 PRTERR,BOPARM,COVRLAP1+1,COVRLAP2,COVRLAP1                       
*                                                                               
         B     EXITN                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO PRINT HEADLINE                                           *         
*                                                                     *         
* R1=1 OR 2                                                           *         
***********************************************************************         
         SPACE 1                                                                
PRTHED   NTR1  ,                                                                
         STC   R1,BOBYTE1                                                       
*                                                                               
         GOTO1 AFMTHED,BOPARM,(BOBYTE1,BLFELD)                                  
         BL    EXITY                                                            
         L     R0,4(R1)            R0=LENGTH OF HEADLINE                        
         LA    R2,PTH1                                                          
         CLI   BLFTYPE,BLFTCSTQ                                                 
         BNE   *+8                                                              
         LA    R2,PCH1                                                          
         CLI   BOBYTE1,2                                                        
         BNE   *+8                                                              
         LA    R2,PTH2-PTH1(R2)                                                 
         A     R2,0(R1)            R2=A(PRINT LINE SPACE FOR HEADLINE)          
         LA    RE,BOELEM                                                        
*                                                                               
PHED12   CLI   0(RE),C' '                                                       
         BNH   PHED14                                                           
         CLI   0(R2),C' '          TEST OVERLAP                                 
         BNE   PRTHEDN                                                          
         MVC   0(1,R2),0(RE)       COPY HEADLINE                                
PHED14   LA    RE,1(RE)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,PHED12                                                        
PRTHEDY  B     EXITY                                                            
*                                                                               
PRTHEDN  MVC   FVMSGNO,=AL2(AE$THOVR)                                           
         MVC   FVXTRA(1),BOBYTE1                                                
         OI    FVXTRA,C'0'                                                      
         LTR   RB,RB               CC=NOT EQUAL                                 
         XIT1  REGS=(R2)                                                        
         B     EXITN                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* OVERLAPPING ERRORS OUTPUT                                           *         
*                                                                     *         
* NTRY: P1=A(TYPE)                                                    *         
*       P2=A(FIELD 1)                                                 *         
*       P3=A(FIELD 2)                                                 *         
*  FVMSGNO=MESSAGE NUMBER FOR TIME FIELD                              *         
* EXIT: FVMSGNO, FVADDR, FVPARMS SET UP                               *         
***********************************************************************         
         SPACE 1                                                                
PRTERR   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         CLI   0(R2),BLFTCSTQ      ADD 1 TO ERROR NO. IF COST TYPE              
         BNE   *+16                                                             
         ICM   RE,3,FVMSGNO                                                     
         LA    RE,1(RE)                                                         
         STCM  RE,3,FVMSGNO                                                     
         XC    BOADDR1,BOADDR1                                                  
         XC    BOADDR2,BOADDR2                                                  
*                                                                               
         XC    NBPARMS,NBPARMS                                                  
PERR02   BAS   RE,NXTBLF                                                        
         BL    PERR10                                                           
         L     R5,NBATAB                                                        
         USING FMTTABD,R5                                                       
         CLC   FMTFLD,0(R3)                                                     
         BNE   *+12                                                             
         LA    R6,BOADDR1                                                       
         B     PERR04                                                           
         CLC   FMTFLD,0(R4)                                                     
         BNE   PERR02                                                           
         LA    R6,BOADDR2                                                       
PERR04   GOTO1 SUBDESC,FMTDIC      ADD DESCRIPTION TO SUB. PARMS                
*                                                                               
         ICM   RF,15,NBADSC                                                     
         BZ    PERR02                                                           
         MVI   0(R6),1                                                          
         LA    RF,CONTLINH-CONDSCH(RF)                                          
         CLI   0(R2),BLFTCSTQ                                                   
         BNE   *+8                                                              
         LA    RF,CONCLINH-CONTLINH(RF)                                         
         STCM  RF,7,1(R6)                                                       
*                                                                               
         LA    R0,CCHGSN                                                        
         LA    RE,CCHGS                                                         
PERR06   CLC   FMTFLD,0(RE)                                                     
         BNE   *+14                                                             
         CLC   0(1,R2),1(RE)                                                    
         BE    PERR08                                                           
         LA    RE,L'CCHGS(RE)                                                   
         BCT   R0,PERR06                                                        
         B     PERR02                                                           
PERR08   OI    0(R6),2                                                          
         B     PERR02                                                           
*                                                                               
PERR10   XR    RF,RF               SET CURSOR TO FIELD ON SCREEN                
         ICM   RF,7,BOADDR2+1                                                   
         CLC   BOADDR1(1),BOADDR2                                               
         BNH   PERR12                                                           
         ICM   RF,7,BOADDR1+1      SWAP SUBSTITUTE PARAMETERS                   
         XC    FVPARMS+1(L'CONDSC),FVPARMS+2+L'CONDSC                           
         XC    FVPARMS+2+L'CONDSC(L'CONDSC),FVPARMS+1                           
         XC    FVPARMS+1(L'CONDSC),FVPARMS+2+L'CONDSC                           
PERR12   LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LA    RF,CONTLINH                                                      
         ST    RF,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT OUT                                                           *         
***********************************************************************         
         SPACE 1                                                                
PRTOUT   NTR1  ,                                                                
         L     R5,AREP                                                          
         USING REPD,R5                                                          
*                                                                               
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         XC    REPSUBID,REPSUBID                                                
         OC    REPSUBID,CSREPID                                                 
         BNZ   *+10                                                             
         MVC   REPSUBID,=C'AFL'                                                 
         MVCDD REPDESC,AC#FRMAT                                                 
         GOTO1 VDICTAT,BOPARM,C'SL  ',REPDESC                                   
*                                                                               
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI  REPACTN,REPAPUT                                                   
*                                                                               
         MVC   REPH1+119(L'BCUSERID),BCUSERID                                   
         MVC   REPH2+119(L'CSBPID),CSBPID                                       
         MVC   REPM1+017(L'CONFMT),CONFMT                                       
         MVC   REPM1+021(L'CONNAM),CONNAM                                       
         MVC   REPM1+106(L'CONLNGW),CONLNGW                                     
         MVC   REPM1+119(L'CONLNG),CONLNG                                       
         MVC   REPM2+017(L'CONGRB),CONGRB                                       
         MVC   REPM2+021(L'CONGRBD),CONGRBD                                     
*                                                                               
         MVI   REPPRNSA,3                                                       
         GOTO1 VREPORT,REPD                                                     
         GOTO1 PRTBLK,BLFTTIMQ                                                  
         MVI   REPPRNSA,5                                                       
         GOTO1 VREPORT,REPD                                                     
         GOTO1 PRTBLK,BLFTCSTQ                                                  
*                                                                               
PRTOUTX  MVI   REPACTN,REPACLO                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RPSPL)                                           
         MVI   FVPARMS,9                                                        
         MVC   FVPARMS+1(L'REPSUBID),REPSUBID                                   
         MVC   FVPARMS+1+L'REPSUBID(1),BCCOMMA                                  
         LA    RF,FVPARMS+1+L'REPSUBID+1                                        
         EDIT (2,REPREPNO),(5,(RF)),ALIGN=LEFT                                  
         LA    RE,CONTLINH                                                      
         ST    RE,FVADDR                                                        
         MVC   BASSRV,DQU                                                       
         OI    BASSRVH+FHOID,FHOITR                                             
         B     EXITY                                                            
         SPACE 1                                                                
PRTBLK   NTR1  ,                                                                
         STC   R1,BOBYTE1                                                       
*                                                                               
         MVI   REPP1,C'-'                                                       
         MVC   REPP1+1(L'REPP1-1),REPP1                                         
         MVCDD REPP1+50(32),AC#TIME,F                                           
         CLI   BOBYTE1,BLFTCSTQ                                                 
         BNE   *+10                                                             
         MVCDD REPP1+50(32),AC#COST,F                                           
         GOTO1 VDICTAT,BOPARM,C'SL  ',REPP1+50                                  
         MVI   REPPRNSA,2                                                       
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         LA    R4,PTH1                                                          
         CLI   BOBYTE1,BLFTCSTQ                                                 
         BNE   *+8                                                              
         LA    R4,PCH1                                                          
         USING PTH1,R4                                                          
*                                                                               
         MVC   REPP1,PTH1                                                       
         GOTO1 VREPORT,REPD                                                     
         MVC   REPP1,PTH2                                                       
         GOTO1 VREPORT,REPD                                                     
         LA    R0,PTLN                                                          
         LA    R2,PTL1                                                          
PBLK02   MVC   REPP1,0(R2)                                                      
         GOTO1 VREPORT,REPD                                                     
         LA    R2,L'PTL1(R2)                                                    
         BCT   R0,PBLK02                                                        
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 1                                                                
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CREATE BLFEL LIST FROM FILE RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
RECTOLST NTR1  ,                                                                
         L     RE,ABLFLST                                                       
         LA    RF,BLFLSTL                                                       
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    NBPARMS,NBPARMS                                                  
         XR    R0,R0                                                            
*                                                                               
RLST02   BAS   RE,NXTBLF                                                        
         BL    RLST10                                                           
         LM    R2,R3,NBPARMS                                                    
         USING FMTTABD,R2                                                       
         USING BLFELD,R3                                                        
         MVI   BLFEL,BLFELQ                                                     
         MVI   BLFLN,BLFLNQ                                                     
         MVC   BLFFLD,FMTFLD                                                    
         MVC   BLFELC,BLFELD                                                    
         MVI   BLFTYPE,BLFTTIMQ                                                 
         MVI   BLFELC+(BLFTYPE-BLFELD),BLFTCSTQ                                 
*                                                                               
         CLC   TWASCRN,CSSCRN      TEST FIRST TIME                              
         BNE   RLST08                                                           
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('BLFELQ',APBCREC),         *        
               (L'BLFFLD+L'BLFTYPE,BLFFLD)                                      
         CLI   12(R1),0                                                         
         BNE   RLST04                                                           
         L     RE,12(R1)                                                        
         MVC   BLFELD(BLFLNQ),0(RE)                                             
RLST04   GOTO1 (RF),(R1),,,(L'BLFELD+L'BLFTYPE,BLFFLD+BLFLNQ)                   
         CLI   12(R1),0                                                         
         BNE   RLST08                                                           
         L     RE,12(R1)                                                        
         MVC   BLFELC(BLFLNQ),0(RE)                                             
*                                                                               
RLST08   BCT   R0,RLST02                                                        
*                                                                               
RLST10   LPR   R0,R0                                                            
         STC   R0,SVTABN           SAVE NO. OF TABLE ENTRIES USED               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* CREATE FILE RECORD FORM BLFEL LIST                                  *         
***********************************************************************         
         SPACE 1                                                                
LSTTOREC NTR1  ,                                                                
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('BLFELQ',APBCREC),0                 
*                                                                               
         XC    NBPARMS,NBPARMS                                                  
*                                                                               
LREC02   BAS   RE,NXTBLF                                                        
         BL    LREC10                                                           
         LM    R2,R3,NBPARMS                                                    
         USING FMTTABD,R2                                                       
         USING BLFELD,R3                                                        
         CLI   BLFLINF,0                                                        
         BE    LREC04                                                           
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBCREC,BLFELD                       
LREC04   CLI   BLFLINF+BLFLNQ,0                                                 
         BE    LREC08                                                           
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBCREC,BLFELC                       
*                                                                               
LREC08   B     LREC02                                                           
*                                                                               
LREC10   B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SAVE ELEMENT LIST IN TEMPSTR                                        *         
***********************************************************************         
         SPACE 1                                                                
SAVLST   NTR1  ,                                                                
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(BLFLSTL)                                                 
         GOTO1 VDMGR,BOPARM,DMWRITE,TEMPSTR,(4,0),ABLFLST,,(RF)                 
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* RESTORE ELEMENT LIST IN TEMPSTR                                     *         
***********************************************************************         
         SPACE 1                                                                
RESLST   NTR1  ,                                                                
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(BLFLSTL)                                                 
         GOTO1 VDMGR,BOPARM,DMREAD,TEMPSTR,(4,0),ABLFLST,,(RF)                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ THE RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
READ     NTR1  ,                                                                
         L     R2,APBCREC                                                       
         USING PBCRECD,R2                                                       
*                                                                               
         NI    SVINDS,FF-SVIPADD-SVIPRES                                        
         MVC   IOKEY,CRECKEY       READ DIRECTORY RECORD                        
         LA    R1,IORDD+IOACCDIR                                                
         CLI   BCPFKEY,PFKCSAVQ                                                 
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    READ10                                                           
         CLI   IOERR,IOEDEL        TEST RECORD DELETED                          
         BNE   READ02                                                           
         OI    SVINDS,SVIPRES                                                   
         NI    SVINDS,FF-SVIPDEL                                                
         B     READ10                                                           
READ02   CLI   IOERR,IOERNF                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    SVINDS,SVIPADD                                                   
         XC    CRACCHA,CRACCHA     SET UP RECORD IF NOT ON FILE                 
         XC    PBCRECD(256),PBCRECD                                             
         MVC   PBCKEY,CRECKEY                                                   
         LA    RE,PBCRFST-PBCRECD+1                                             
         STCM  RE,3,PBCRLEN                                                     
         B     READX                                                            
*                                                                               
READ10   MVC   CRECDIR,IOKEY       GET FILE RECORD                              
         MVC   IODAOVER,CRECDA                                                  
         LA    R1,IOGET+IO1+IOACCMST                                            
         CLI   BCPFKEY,PFKCSAVQ                                                 
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETRAC,BOPARM,PBCRECD,0,CRACCHA                                 
*                                                                               
READX    GOTO1 AFMTBLK,BOPARM,('FBGET',CFMTBLK),PBCRECD                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WITE THE RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
WRITE    NTR1  ,                                                                
         L     R2,APBCREC                                                       
         USING PBCRECD,R2                                                       
         TM    SVINDS,SVIPDEL+SVIPRES                                           
         BZ    *+8                 DELETE/RESTORE RECORD                        
         XI    PBCRSTAT,PBCSDELT                                                
*                                                                               
         TM    SVINDS,SVIPADD      TEST RECORD ON FILE                          
         BO    WRITE10                                                          
         GOTO1 APUTRAC,BOPARM,('RACTCHA',PBCRECD)                               
         CLC   CRECSTA,PBCRSTA     TEST CHANGE IN STATUS                        
         BE    WRITE02                                                          
         MVC   CRECSTA,PBCRSTA     WRITE BACK ACCDIR RECORD                     
         MVC   IOKEY,CRECDIR                                                    
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    SVINDS,SVINSEL      TEST SELECTION FROM LIST                     
         BZ    WRITE02                                                          
         L     RF,ALSVALS                                                       
         USING LSVALSD,RF                                                       
         USING TLSTD,LSTLST                                                     
         MVC   TLCDIR,IOKEY        UPDATE TSARIO RECORD                         
         DROP  RF                                                               
         GOTO1 ATSARIO,TSAPUT                                                   
WRITE02  MVC   FVMSGNO,=AL2(AI$RCENK)                                           
         TM    SVINDS,SVIPRES                                                   
         BZ    *+10                                                             
         MVC   FVMSGNO,=X'FF08'                                                 
         TM    SVINDS,SVIPDEL                                                   
         BZ    *+10                                                             
         MVC   FVMSGNO,=X'FF07'                                                 
         LA    R1,IOWRITE                                                       
         B     WRITE20                                                          
*                                                                               
WRITE10  GOTO1 APUTRAC,BOPARM,('RACTADD+RACTCHA',PBCRECD)                       
         MVC   FVMSGNO,=AL2(AI$RAENX)                                           
         LA    R1,IOADD                                                         
*                                                                               
WRITE20  GOTO1 AIO,IO1+IOACCMST(R1)                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETRAC,BOPARM,APBCREC,0,CRACCHA                                 
*                                                                               
         NI    SVINDS,FF-SVIPWRT-SVIPADD-SVIPDEL-SVIPRES                        
         MVI   FVOMTYP,GTMINF                                                   
         LA    RE,CONFMTH                                                       
         ST    RE,FVADDR                                                        
*                                                                               
WRITEX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET NEXT FMTTAB ENTRY                                    *         
*                                                                     *         
* NTRY: R2=A(CURRENT FMTTAB ENTRY) OR 0                               *         
* EXIT: R2=A(NEXT FMTTAB ENTRY)                                       *         
*       R3=A(NEXT BLFEL IN ELEMENT LIST)                              *         
*       R4=A(POSTION IN TWA FOR TABLE ENTRY)                          *         
*       CC=LOW FOR END OF TABLE                                       *         
*       CC=HIGH IF AFTER END OF SCREEN                                *         
*       CC=EQUAL IF ENTRY ON SCREEN                                   *         
***********************************************************************         
         SPACE 1                                                                
NXTBLF   NTR1  ,                                                                
         LM    R2,R4,NBPARMS                                                    
*                                                                               
         LTR   R2,R2                                                            
         BZ    NBLF02                                                           
*                                                                               
         LA    R3,BLFLNQ*2(R3)     BUMP R3 TO NEXT LIST ENTRY                   
         IC    RF,NBTAB#                                                        
         LA    RF,1(RF)                                                         
         STC   RF,NBTAB#                                                        
         B     NBLF18                                                           
*                                                                               
NBLF02   L     R2,AFMTTAB                                                       
         L     R3,ABLFLST                                                       
         MVI   NBTAB#,0                                                         
         XR    R4,R4                                                            
         USING FMTTABD,R2                                                       
*                                                                               
NBLF12   CLI   FMTTABD,EOT                                                      
         BE    NXTBLFN                                                          
*                                                                               
         GOTO1 TESTSC,FMTSYS       TEST SYSTEM/COUNTRY FILTER                   
         BE    NXTBLFY                                                          
*                                                                               
NBLF18   LA    R2,FMTTABL(R2)                                                   
         B     NBLF12                                                           
*                                                                               
NXTBLFN  B     EXITL                                                            
*                                                                               
NXTBLFY  CLC   NBTAB#,SVTOP#      TEST REACHED TOP OF SCREEN YET                
         BL    NXTBLFY2                                                         
         BNE   *+12                                                             
         LA    R4,CONDSCH                                                       
         B     NXTBLFY2                                                         
         LTR   R4,R4               TEST PAST END-OF-SCREEN                      
         BZ    NXTBLFH                                                          
         LA    R4,CONLINL(R4)                                                   
         CLI   FHLND(R4),L'CONDSCH+L'CONDSC                                     
         BE    NXTBLFY2                                                         
         XR    R4,R4                                                            
         B     NXTBLFH                                                          
*                                                                               
NXTBLFY2 MVC   SVBOT#,NBTAB#       BOTTOM'S UP                                  
         STM   R2,R4,NBPARMS                                                    
         B     EXITY                                                            
*                                                                               
NXTBLFH  STM   R2,R4,NBPARMS                                                    
         B     EXITH                                                            
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD DESCRIPTION TO ERROR MESSAGE SUBSTITUTION PARMS      *         
*                                                                     *         
* R1=A(DICTIONARY REF#)                                               *         
***********************************************************************         
         SPACE 1                                                                
SUBDESC  NTR1  ,                                                                
         LA    R2,FVPARMS                                                       
         XR    RF,RF                                                            
         ICM   RF,1,0(R2)                                                       
         BZ    *+8                                                              
         BXH   R2,RF,*-8                                                        
         MVI   0(R2),L'CONDSC+1                                                 
         MVI   1(R2),AC#ESCL                                                    
         MVC   2(2,R2),0(R1)                                                    
         MVI   4(R2),L'CONDSC                                                   
         GOTO1 VDICTAT,BOPARM,C'SL  ',1(R2)                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD NUMBER TO ERROR MESSAGE SUBSTITUTION PARMS           *         
*                                                                     *         
* R1=A(1 BYTE NUMBER)                                                 *         
***********************************************************************         
         SPACE 1                                                                
SUBNUM   NTR1  ,                                                                
         LA    R2,FVPARMS                                                       
         XR    RF,RF                                                            
         ICM   RF,1,0(R2)                                                       
         BZ    *+8                                                              
         BXH   R2,RF,*-8                                                        
         MVI   0(R2),4                                                          
         EDIT (1,(R1)),(3,1(R2)),ALIGN=LEFT                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* COMPARE INPUT WITH SOME KIND OF A WORD                              *         
*                                                                     *         
* NTRY: R1=A(WORD)                                                    *         
***********************************************************************         
         SPACE 1                                                                
CMPWRD   LA    RF,2                                                             
         CLI   FVXLEN,2                                                         
         BH    *+8                                                              
         IC    RF,FVXLEN                                                        
         EX    RF,*+6                                                           
         BR    RE                                                               
         CLC   FVIFLD(0),0(R1)                                                  
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO TEST SYSTEM./COUNTRY FILTERS                             *         
*                                                                     *         
* NTRY: R1=A(SYSTEM/COUNTRY)                                          *         
***********************************************************************         
         SPACE 1                                                                
TESTSC   OC    0(2,R1),0(R1)       TEST ALL SYSTEMS/COUTRIES                    
         BZR   RE                  YES - CC=EQUAL                               
*                                                                               
         CLI   0(R1),0             TEST ALL SYSTEMS                             
         BE    TSC02                                                            
*&&UK*&& CLI   0(R1),SYSUK         MATCH ON UK SYSTEM                           
*&&US*&& CLI   0(R1),SYSUS         MATCH ON US SYSTEM                           
         BNER  RE                  RETURN WITH CC=NOT EQUAL                     
*                                                                               
TSC02    CLI   1(R1),0             TEST ALL COUNTRIES                           
         BER   RE                                                               
         CLC   CUCTRY,1(R1)        MATCH ON CONNECTED COUNTRY                   
         BER   RE                                                               
         TM    1(R1),CTRYNOT       TEST ALL BUT A COUNTRY                       
         BZ    TESTSCN                                                          
         MVC   BYTE,1(R1)                                                       
         XI    BYTE,CTRYNOT                                                     
         CLC   BYTE,CUCTRY                                                      
         BNE   TESTSCY                                                          
*                                                                               
TESTSCN  LTR   RE,RE               CC=NOT EQUAL                                 
         BR    RE                                                               
TESTSCY  CR    RE,RE               CC=EQUAL                                     
         BR    RE                                                               
         EJECT                                                                  
FF       EQU   X'FF'                                                            
ACCMST   DC    C'ACCMST '                                                       
DMREAD   DC    C'DMREAD  '                                                      
DMWRITE  DC    C'DMWRT   '                                                      
TEMPSTR  DC    C'TEMPSTR '                                                      
DQU      DC    CL(L'BASSRV)'=DQU'                                               
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,51,AC#FMTRP,32,C                                              
         SPEC  H2,51,AC#FMTRP,32,CU                                             
         SPEC  H1,107,AC#USRID,12,L                                             
         SPEC  H2,107,AC#REQR,12,L                                              
         SPEC  M1,1,AC#FRMNO,16,L                                               
         SPEC  M2,1,AC#GRPNG,16,L                                               
         SPEC  END                                                              
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* FALANG                                                                        
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORKS                                                                    
       ++INCLUDE ACCLBWORKC                                                     
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   AIO1                                                             
APBCREC  DS    A                   A(PRODUCTION BILL RECORD)                    
         ORG   AIO4                                                             
ABLFLST  DS    A                   A(LIST OF BLFELS)                            
BLFLSTL  EQU   (BLFLNQ*2*22)                                                    
         SPACE 1                                                                
BLFELD   DSECT                     DEFINE DATA PART OF ELEMENT                  
BLFDATAL EQU   BLFLNQ-(BLFLINF-BLFELD)                                          
         ORG   BLFLINF                                                          
BLFDATA  DS    XL(BLFDATAL)                                                     
         ORG   BLFELD+BLFLNQ                                                    
BLFELC   DS    XL(BLFLNQ)          COST ELEMENT                                 
         ORG   BLFELC+(BLFDATA-BLFELD)                                          
BLFDATAC DS    XL(BLFDATAL)        COST ELEMENT DATA                            
         ORG   BLFELC+BLFLNQ                                                    
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF3D                                                       
CONLINS  EQU   CONPFKH-CONDSCH     SIZE OF DATA TYPE LINES                      
CONLINL  EQU   CONDSC2H-CONDSCH    LENGTH OF DATA TYPE LINE                     
CONLINN  EQU   CONLINS/CONLINL     NO. OF DATA TYPE LINES                       
         SPACE 1                                                                
***********************************************************************         
* SAVED W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
         ORG OSVALS                                                             
SVWORK   DS    0XL(SVWORKL)                                                     
*                                                                               
SVINDS   DS    XL1                 SAVED INDICATORS                             
SVIPWRT  EQU   X'80'               WRITE IS PENDING                             
SVIPADD  EQU   X'40'               RECORD TO BE ADDED TO FILE                   
SVINSEL  EQU   X'20'               NTRSES VIA SELECT FROM LIST                  
SVIPDEL  EQU   X'10'               SELECTED FOR DELETION                        
SVIPRES  EQU   X'08'               SELECTED FOR RESTORATION                     
SVTOP#   DS    XL1                 TOP TABLE NUMBER ON PAGE                     
SVBOT#   DS    XL1                 BOTTOM TABLE NUMBER ON PAGE                  
SVTABN   DS    XL1                 NUMBER OF ENTRIES IN TABLE                   
*                                                                               
SVKEY    DS    XL42                SAVED RECORD KEY                             
SVRACCHA DS    XL(RACLNQ)          SAVED RECORD ACTIVITY CHANGE DETAILS         
*                                                                               
SVWORKL  EQU   *-SVINDS                                                         
         ORG   OSVALS+OSVALSL                                                   
         SPACE 1                                                                
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
CWORKD   DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6A                                                               
WORK     DS    XL100                                                            
BYTE     DS    XL1                                                              
*                                                                               
CINDS    DS    XL1                 INDICATORS                                   
CISCROLL EQU   X'80'               SCROLLING SCROLLED                           
CICHG    EQU   X'40'               A FIELD HAS BEEN CHANGED                     
CIDISERR EQU   X'20'               REDISPLAY ON ERROR                           
CCHGSN   EQU   CONLINN*2                                                        
CCHGS    DS    (CCHGSN)AL2         LINE CHANGE LIST                             
COVRLAP1 DS    AL2                 FIELD/TYPE OVERLAP 1                         
COVRLAP2 DS    AL2                 FIELD/TYPE OVERLAP 2                         
*                                                                               
CCOLL    DS    XL1                 END OF FIELD COLUMN NUMBER                   
CPRTFLD  DS    CL132               OUTPUT PRINT LINE                            
*                                                                               
CRECDIR  DS    0XL(ACCKLEN)        CURRENT DIRECTORY RECORD                     
CRECKEY  DS    XL42                CURRENT RECORD KEY                           
CRECSTA  DS    XL8                 RECORD STATUS                                
CRECDA   DS    XL4                 DISK ADDRESS                                 
CRACCHA  DS    XL(RACLNQ)          CURRENT CHANGE ACTIVITY DETAILS              
*                                                                               
CTIME    DS    0XL5                * TIME PARAS. DATA *                         
CTMAXTOP DS    XL1                 MAXIMUM LINE NUMBER FROM THE TOP             
CTMAXBOT DS    XL1                 MAXIMUM LINE NUMBER FROM THE BOTTOM          
CTDISTOP DS    XL1                 DISPLACEMENT FOR TOP LINES                   
CTDISBOT DS    XL1                 DISPLACEMNT FOR BOTTOM LINES                 
CTTOTTYP DS    XL1                 BLFEL TOTAL TYPE                             
CCOST    DS    0XL5                * COST PARAS. DATA *                         
CCMAXTOP DS    XL1                 MAXIMUM LINE NUMBER FROM THE TOP             
CCMAXBOT DS    XL1                 MAXIMUM LINE NUMBER FROM THE BOTTOM          
CCDISTOP DS    XL1                 DISPLACEMENT FOR TOP LINES                   
CCDISBOT DS    XL1                 DISPLACEMNT FOR BOTTOM LINES                 
CCTOTTYP DS    XL1                 BLFEL TOTAL TYPE                             
*                                                                               
CFMTBLK  DS    XL(FBLKL)                                                        
*                                                                               
         SPACE 1                                                                
         DS    0A                                                               
NBPARMS  DS    0XL16               * NXTBLF PARAMETERS *                        
NBATAB   DS    A                   A(FMTTAB TABLE ENTRY)                        
NBAEL    DS    A                   A(TIME ELEMENT)                              
NBADSC   DS    A                   A(CONDSCH IN TWA)                            
         DS    AL3                 N/D                                          
NBTAB#   DS    XL1                 TABLE NUMBER                                 
         ORG   NBPARMS+L'NBPARMS                                                
         SPACE 1                                                                
PBLOCK   DS    0X                  * PRINT BLOCK *                              
PTH1     DS    CL132               TIME HEADING 1                               
PTH2     DS    CL132               TIME HEADING 2                               
PTL1     DS    CL132               TIME PRINT LINE 1                            
PTL2     DS    CL132               TIME PRINT LINE 2                            
PTL3     DS    CL132               TIME PRINT LINE 3                            
PTL4     DS    CL132               TIME PRINT LINE 4                            
PTL5     DS    CL132               TIME PRINT LINE 5                            
PTL6     DS    CL132               TIME PRINT LINE 6                            
PTL7     DS    CL132               TIME PRINT LINE 7                            
PTL8     DS    CL132               TIME PRINT LINE 8                            
PTL9     DS    CL132               TIME PRINT LINE 9                            
PTLN     EQU   (*-PTL1)/L'PTL1     NO. OF TIME PRINT LINES                      
*                                                                               
PCH1     DS    CL132               COST HEADING 1                               
PCH2     DS    CL132               COST HEADING 2                               
PCL1     DS    CL132               COST PRINT LINE 1                            
PCL2     DS    CL132               COST PRINT LINE 2                            
PCL3     DS    CL132               COST PRINT LINE 3                            
PCL4     DS    CL132               COST PRINT LINE 4                            
PCL5     DS    CL132               COST PRINT LINE 5                            
PCL6     DS    CL132               COST PRINT LINE 6                            
PCL7     DS    CL132               COST PRINT LINE 7                            
PCL8     DS    CL132               COST PRINT LINE 8                            
PCL9     DS    CL132               COST PRINT LINE 9                            
PCLN     EQU   (*-PCL1)/L'PCL1     NO. OF COST PRINT LINES                      
*                                                                               
PBLOCKL  EQU   *-PBLOCK                                                         
*                                                                               
         ORG   CWORKD+OVERWRKL                                                  
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049ACCLB0C   05/01/02'                                      
         END                                                                    
