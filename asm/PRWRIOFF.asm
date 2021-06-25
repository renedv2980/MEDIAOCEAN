*          DATA SET PRWRIOFF   AT LEVEL 052 AS OF 07/08/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE T00AA2A                                                                  
*                                                                               
*        TITLE  PRWRIOFF - PRINT WRITER OFFLINE ROUTINES                        
         TITLE 'PRWRIOFF - CHANGE LOG '                                         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT ITMF-37644  07/08/19 INCREASE CLIENT BUFFER (YET AGAIN)        *         
* AKAT ITMF-31736  12/21/18 INCREASE CLIENT BUFFER                    *         
***********************************************************************         
*                                                                               
*  BOBY 12/14/04 INCREASE PRODUCT BUFFER TO 48000                               
*                                                                               
*  BOBY 11/15/93 CREATION                                                       
*                                                                               
         TITLE  'PRWRIOFF - PRINT WRITER OFFLINE ROUTINES'                      
T00AA2   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T00AA2,RR=RE,CLEAR=YES                                         
*                                                                               
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND         RE-ESTABLSIH COMMON WORKING STORAGE          
         USING GEND,RC                                                          
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING T405FFD,RA                                                       
*                                                                               
         ST    RE,RELOPOFF         SAVE RELOCATION FACTOR                       
*                                                                               
         SRL   RF,24               RF HAS INDEX TO ROUTINE IN HIGH              
         SLL   RF,2                   ORDER BYTE                                
         LA    RF,BRANCH(RF)       POINT TO ENTRY IN BRANCH TABLE               
*                                                                               
         CLI   0(RF),0             IF IT IS NOT AN ADDRESS                      
         BNE   0(RF)                  GO TO BRANCH STATEMENT                    
*                                                                               
         L     RF,0(RF)            ELSE GO TO THAT ADDRESS                      
         A     RF,RELOPOFF         RELOCATE ADDRESS                             
         BASR  RE,RF                                                            
*                                                                               
         XIT1                                                                   
*                                                                               
         DS    0A                  ALIGNMENT                                    
BRANCH   DC    A(TRACDRON)         TRACE DRONE                                  
         DC    A(DOFLOW)           FLOWCHARTING                                 
         DC    A(INITDRV)          INIT DRIVER                                  
         DC    A(GENHD)            GENERAL HEADLINE HOOK ROUTINE                
         DC    A(GENFT)            GENERAL FOOTING ROUTINE                      
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRWRIOFF - TRACE DRONE - TRACDRON'                              
***********************************************************************         
*                                                                     *         
*        TRACING DRONE                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
TRACDRON NMOD1 0,**#TRD                                                         
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
*                                                                               
         LA    RC,SPOOLEND         RESET WORKING STORAGE POINTER                
         USING GEND,RC                                                          
*                                                                               
         CLI   TRACEOPT,C'Y'       DRONE TRACING OPTION                         
         BNE   TRACDRNX                                                         
*                                                                               
         L     R3,ADPGPROG                                                      
*                                                                               
TRACD2   CLI   0(R3),0                                                          
         BE    TRACDRNX                                                         
         ZIC   R4,1(R3)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)                                                       
         LA    R4,1(R4)                                                         
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         GOTO1 HEXOUT,DMCB,(R3),BLOCK,(R4),=C'SEP'                              
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),BLOCK                                                       
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         LA    R5,BLOCK+1(R4)                                                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)                                                       
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         LA    R3,1(R3,R4)                                                      
         B     TRACD2                                                           
*                                                                               
TRACDRNX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRWRIOFF - INITIALIZE DRIVER - INITDRV'                         
***********************************************************************         
*                                                                     *         
*        INITIALIZE DRIVER                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
INITDRV  NMOD1 0,**#IDR                                                         
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
*                                                                               
         LA    RC,SPOOLEND         RESET WORKING STORAGE POINTER                
         USING GEND,RC                                                          
*                                                                               
* INITIALIZE TO RUN DRIVER - LOADS PHASES                                       
*                            SETS GLOBAL ADDRESSES                              
*                                                                               
*        GET CORE FOR ESTIMATE BUFFER                                           
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(500*1024)             500K BUFFER                            
*                                                                               
         OC    PBAESTBF,PBAESTBF   CHECK FOR MULTIPLE ENTRIES                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,PBAESTBF         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         ST    R3,PBLESTBF         SAVE RETURNED BUFFER LENGTH                  
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         GOTO1 CALLOV,DMCB,X'B1000000',0,0  LOAD T405B1(GLOBAL STORAGE)         
*                                                                               
         L     R4,DMCB                     FOR DRIVER                           
         ST    R4,AGLOBAL                                                       
*                                  THIS ALSO CONTAINS BUFFERS                   
         LA    R2,16(R4)                                                        
         L     R1,0(R2)                                                         
         LA    R2,4+8(R1,R2)                                                    
*                                                                               
         L     R1,0(R2)                                                         
         ST    R1,PBLMEDBF         LENGTH OF MEDIA BUFFER                       
         LA    R2,4(R2)                                                         
         ST    R2,PBAMEDBF         A(MEDIA BUFFER)                              
*                                                                               
         LA    R2,8(R1,R2)                                                      
         ST    R2,PBAOFFBF         A(USER AREA)                                 
*                                                                               
*        GET CORE FOR CLIENT BUFFER                                             
*                                                                               
         OC    PBACLTBF,PBACLTBF                                                
         BZ    *+6                 CHECKING FOR MULTIPLE ISSUES                 
         DC    H'0'                                                             
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',400000,400000                    L27          
*                                                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    RE,PBACLTBF         CLIENT BUFFER ADDRESS                        
*                                                                               
         L     RF,8(R1)                                                         
         ST    RF,PBLCLTBF         CLIENT BUFFER LENGTH                         
*                                                                               
*        GET CORE FOR PRODUCT BUFFER                                            
*                                                                               
         OC    PBAPRDBF,PBAPRDBF                                                
         BZ    *+6                 CHECKING FOR MULTIPLE ISSUES                 
         DC    H'0'                                                             
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',60000,60000                    L27            
*                                                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    RE,PBAPRDBF         PRODUCT BUFFER ADDRESS                       
*                                                                               
         L     RF,8(R1)                                                         
         ST    RF,PBLPRDBF         PRODUCT BUFFER LENGTH                        
*                                                                               
*        GET CORE FOR REP BUFFER                                                
*                                                                               
         OC    PBAREPBF,PBAREPBF                                                
         BZ    *+6                 CHECKING FOR MULTIPLE ISSUES                 
         DC    H'0'                                                             
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',70016,70016                    L27            
*                                                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   4(8,RE),=CL8'*REPBUF*' LABEL BUFFER                              
         L     RF,8(R1)            BUFFER LENGTH                                
         SH    RF,=H'16'           HEADER LENGTH                                
         ST    RF,12(RE)           USABLE BUFFER LENGTH                         
         LA    RE,16(RE)           START OF BUFFER                              
*                                                                               
         ST    RE,PBAREPBF         REP    BUFFER ADDRESS                        
*                                                                               
*        GET CORE FOR AD BUFFER                                                 
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',700000,700000                    L27          
*                                                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    RE,PBAADBF          AD CODE BUFFER ADDRESS                       
*                                                                               
         L     RF,8(R1)                                                         
         ST    RF,PBLADBF          AD CODE BUFFER LENGTH                        
*                                                                               
*                                                                               
*        GET CORE FOR DRD BUFFER                                                
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',100000,100000                                 
*                                                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    RE,PBADRDBF         DRD BUFFER ADDRESS                           
*                                                                               
         L     RF,8(R1)                                                         
         ST    RF,PBLDRDBF         DRD BUFFER LENGTH                            
*                                                                               
***      =================  ***                                                 
         USING GLOBALD,R4                                                       
***      =================  ***                                                 
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'  LOAD T00A3A (DRIVER)                  
         L     R2,DMCB                                                          
         ST    R2,DRIVER                                                        
*                                                                               
         GOTO1 CALLOV,DMCB,(9,0),0,0     LOAD T40509 (PRINTPAK DRIVER)          
*                                                                               
         L     R2,DMCB                                                          
         ST    R2,GLASYSDR                                                      
*                                                                               
         ST    RC,GLAWORKD                                                      
*                                                                               
         MVC   GLAPROG,ADPGPROG                                                 
         MVC   GLBOXOPT,BOXOPT                                                  
         MVC   GLLFTOPT,LEFTOPT                                                 
         MVC   GLRNKMAX,MAXRANK    VALUE                           L20          
         MVC   GLSPACE,SPACOPT                                                  
         MVC   GLDOWNLD,SPECOPT                                    L22          
         MVC   GLDWNLD2,SPECOPT2                                   L22          
         MVC   GLDLCHAR,SPECCHAR                                   L22          
         MVI   SPECOPT,0           CLEAR                           L22          
         MVI   SPECOPT2,0          CLEAR                           L22          
         MVI   GLTWORKD,GLTSPOOL                                                
         MVC   GLINDS,DRINDS                                                    
         MVC   GLINDS2,PBUNITS                                                  
         MVI   PBUNITS,0                                                        
         MVC   GLFHEADL,MYFIRSTH                                                
         MVI   GLNORBOX,X'40'                                                   
         CLI   BOXOPT,C'N'         IF THERE ARE NO BOXES                        
         BNE   *+8                                                              
         MVI   GLNORBOX,0             TURN OFF OPTION                           
         MVI   PERTOTSW,C'Y'        SIGNIFY PERIOD TOTAL ENCOUNTERED            
*                                                                               
         ZIC   RE,GLFHEADL                                                      
         LA    RE,3(RE)                                                         
         LA    RF,14                                                            
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         STC   RE,GLLHEADL                                                      
         MVI   GLDETHED,C'Y'        I GET HEADS AT DETAIL TIME                  
         CLI   DOWNOPT,C'Y'         OPTION TO DOWNLOAD                          
         BNE   *+8                                                              
         OI    GLDOWNLD,X'80'                                                   
*                                                                               
         CLI   TRACEOPT,C'Y'        OPTION TO TRACE                             
         BNE   *+8                                                              
         MVI   GLTRACE,C'Y'                                                     
*                                                                               
         CLC   FOOTCOM,SPACES        CHECK FOR FOOTING COMMENT                  
         BNH   DRFOOTX                                                          
*                                                                               
         L     RF,FOOTINGA           SET FOOTING ADDRESS                        
         STCM  RF,15,FOOTHOOK                                                   
         MVC   FOOTLNS,FOOT#LNS      SET NUMBER OF FOOTLINES                    
*                                                                               
DRFOOTX  DS    0H                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* INITIALIZATION OF PRINT RELATED FIELDS                                        
*                                                                               
***      =================  ***                                                 
DRI6     L     R4,ABOX                                                          
         USING BOXD,R4                                                          
***      =================  ***                                                 
         CLI   WIDEOPT,C'Y'                                                     
         BE    DRIWIDE                                                          
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   BOXFONT,0                                                        
         LA    R1,H1               PRINT ADDRESSES FOR STANDARD                 
         ST    R1,AH1                                                           
         LA    R1,H4                                                            
         ST    R1,AH4                                                           
         LA    R1,P                                                             
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'132'                                                   
         LA    R1,REGSPECS                                                      
         ST    R1,SPECS                                                         
         CLI   NARROPT,C'Y'                                                     
         BNE   INITDRVX                                                         
         LA    R1,NARSPECS                                                      
         ST    R1,SPECS                                                         
         B     INITDRVX                                                         
*                                                                               
DRIWIDE  MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXFONT,1                                                        
***      =================  ***                                                 
         L     R4,BOXAWIDE                                                      
         USING WIDED,R4                                                         
***      =================  ***                                                 
         LA    R1,XHEAD1                                                        
         ST    R1,AH1                                                           
         LA    R1,XHEAD4                                                        
         ST    R1,AH4                                                           
         LA    R1,XP                                                            
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'198'                                                   
         LA    R1,WIDSPECS                                                      
         ST    R1,SPECS                                                         
         B     INITDRVX                                                         
*                                                                               
INITDRVX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
REGSPECS DS    0C                                                               
         SSPEC H1,2,RUN            SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
NARSPECS DS    0C                                                               
         SSPEC H1,60,RUN           SPECS FOR NARROW PRINTING                    
         SSPEC H2,60,REQUESTOR                                                  
         SSPEC H4,60,REPORT                                                     
         SSPEC H4,73,PAGE                                                       
         DC    X'00'                                                            
*                                                                               
WIDSPECS DS    0C                                                               
         WSPEC H1,2,RUN            SPECS FOR WIDE PRINTING                      
         WSPEC H2,2,REQUESTOR                                                   
         WSPEC H1,129,AGYNAME                                                   
         WSPEC H2,129,AGYADD                                                    
         WSPEC H4,129,REPORT                                                    
         WSPEC H4,142,PAGE                                                      
         DC    X'00'                                                            
         TITLE 'PRWRIOFF - FLOWCHARTING - DOFLOW'                               
***********************************************************************         
*                                                                     *         
*                 FLOWCHARTING                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
DOFLOW   NMOD1 0,**#DFL                                                         
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
*                                                                               
         LA    RC,SPOOLEND         RESET WORKING STORAGE POINTER                
         USING GEND,RC                                                          
*                                                                               
*        ADD TITLE ELEMENTS IF STACKING                                         
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF NOT STACKING                         
         BNO   DFLTTLX                                                          
*                                                                               
         L     RF,ASTACKBF         POINT TO STACK BUFFER                        
         USING STACKBFD,RF         ESTABLISH BUFFER                             
*                                                                               
         LM    R3,R5,STBFBXLE      LOAD BXLE REGISTERS FOR BUFFER               
*                                                                               
         DROP  RF                                                               
*                                                                               
DFLTTLLP DS    0H                                                               
*                                                                               
*        ADD ONE COLUMN FOR EACH STACK KEYWORD                                  
*        THEY WILL PRINT VERTICALLY WITH COLUMN HEADINGS                        
*                                                                               
         LA    R0,DRLENIN          CLEAR DRONEBLOCK AREA                        
         LA    R1,DRINFEND-DRLENIN                                              
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    BLOCK(42),BLOCK                                                  
         MVC   BLOCK(2),=X'0500'   LENGTH OF SPLIT FIELD                        
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(5),=C'TITLE'                                            
*                                                                               
         LR    R0,R4               SAVE BUFFER ENTRY LENGTH                     
         LA    R4,BLOCK            POINT TO CREATED SCANNER BLOCK               
*                                                                               
         GOTO1 VCOLDRON            VALIDATE TITLE AS KEYWORD                    
         CLI   DRERROR,0                                                        
         JNE   *+2                                                              
*                                                                               
         LR    R4,R0               RESTORE BUFFER ENTRY LENGTH                  
*                                                                               
*        USE WIDTH ENTERED ON STACK KEYWORD                                     
*                                                                               
         MVC   DRLENO,STACKWID     SET OUTPUT WIDTH                             
         MVC   DRPOSO(2),=C'P+'    PRINT ON NEXT LINE                           
         NI    DRHEAD1,X'FF'-X'80'   NO HEADLINES                               
*                                                                               
         CLC   DRLABELI-DRLENIN(L'DRLABELI,R3),STACK1ST IF FIRST COL            
         BNE   DFLTTL10                                                         
*                                                                               
         MVI   DRPOSO+1,1          PRINT ON FIRST LINE                          
*                                                                               
         B     DFLTTL10            NO HEADLINES                                 
*                                                                               
         OI    DRHEAD1,X'80'       PRINT HEADLINE                               
*                                                                               
*        TRANSFER SAVED HEADLINES TO HEADLINES FOR THIS FIELD                   
*                                                                               
         LA    R0,2                ONLY INTERESTED IN 1ST TWO HEADLINES         
         LA    R1,STACKH1L         HEAD 1 SAVEAREA                              
         LA    R2,DRHEAD1          POINT TO FIRST HEADLINE                      
*                                                                               
DFLWHDLP DS    0H                                                               
*                                                                               
         USING DRHEADD,R2                                                       
*                                                                               
         CLI   0(R1),X'FF'         IF THERE IS NO HEADING                       
         BNE   *+12                                                             
         NI    DRHEAD,X'FF'-X'80'     SKIP HEADLINE PRINTING                    
         B     DFLWHDCN                                                         
*                                                                               
         CLI   0(R1),0             IF NO OVERRIDE, USE DEFAULT HEADING          
         BE    DFLWHDCN                                                         
*                                                                               
         OI    DRHEAD,X'80'        PRINT HEADLINE                               
*                                                                               
         MVC   DRHLITL,0(R1)       SAVE LITERAL LENGTH                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)          GET LENGTH OF HEADING                        
         BZ    *+10                NO LENGTH                                    
         BCTR  RE,0                DECREMENT FOR EXECUTE                        
         EX    RE,*+8              MOVE IN THE HEADER LITERAL                   
         B     *+10                                                             
         MVC   DRHLIT,1(R1)        SET HEADLINE                                 
*                                                                               
DFLWHDCN DS    0H                                                               
*                                                                               
         LA    R2,L'DRH1ALL(R2)    POINT TO NEXT HEADLINE                       
         LA    R1,STACKH2L         POINT TO SECOND SAVEAREA                     
         BCT   R0,DFLWHDLP                                                      
*                                                                               
         DROP  R2                                                               
*                                                                               
DFLTTL10 DS    0H                                                               
*                                                                               
         MVI   DRLITLNI,1          INPUT IS LITERAL TO FORCE                    
         MVI   DRLITI,C' '           DRIVER TO HANDLE IT                        
         MVC   DROPTSO,DROPTSO-DRLENIN(R3)  COPY OPTIONS                        
*                                                                               
*        COMBINE HEADLINES INTO AN OUTPUT LITERAL                               
*                                                                               
         LA    R7,DRH1ALL-DRLENIN(R3)  POINT TO FIRST HEADLINE                  
DRH1     USING DRH1ALL,R7          ESTABLISH HEADLINE AREA                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,STACKWID          MAX LENGTH TO OUTPUT LITERAL                
         LA    R1,4                 MAX 4 HEADLINES                             
         LA    R2,DRLITO            START OF OUTPUT LITERAL                     
         XC    DRLITO,DRLITO        INIT OUTPUT LITERAL                         
*                                                                               
DFLLITLP DS    0H                                                               
*                                                                               
         TM    DRH1.DRHEAD1,X'80'  SKIP IF NOT PRINTING HEADLINE                
         BNO   DFLLIT10                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,DRH1.DRH1LITL  LENGTH OF FIRST HEADLINE                     
         BZ    DFLLIT10            BLANK HEADLINE                               
*                                                                               
         LA    RE,DRH1.DRH1LIT(RF) POINT TO LAST BYTE OF LITERAL                
         BCTR  RE,0                                                             
*                                                                               
         CLI   0(RE),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BH    *+14                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         B     DFLLIT10            BLANK HEADLINE                               
*                                                                               
         CR    RF,R0               CHECK IF IT WILL FIT                         
         BH    DFLLITDN                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),DRH1.DRH1LIT  MOVE HEADLINE TO OUTPUT                    
*                                                                               
         LA    RF,2(RF)            LENGTH INCLUDING TRAILING SPACE              
         LA    R2,0(RF,R2)         NEXT AVAILABLE SPOT                          
         SR    R0,RF               LENGTH LEFT IN OUTPUT LITERAL                
         BNP   DFLLITDN            NO ROOM LEFT                                 
*                                                                               
DFLLIT10 DS    0H                                                               
*                                                                               
DFLLITCN DS    0H                                                               
*                                                                               
         LA    R7,L'DRH1ALL(R7)    BUMP TO NEXT HEADLINE                        
         BCT   R1,DFLLITLP                                                      
*                                                                               
DFLLITDN DS    0H                                                               
*                                                                               
         BCTR  R2,0                ELIMINATE TRAILING SPACE                     
         CLI   0(R2),C' '                                                       
         BNH   *+8                                                              
         LA    R2,1(R2)            LAST WASN'T A SPACE                          
*                                                                               
         LA    RF,DRLITO           START OF LITERAL                             
         SR    R2,RF               LITERAL LENGTH                               
         BP    *+12                IF ALL HEADS BLANK                           
         LA    R2,1                                                             
         MVI   DRLITO,X'41'           FORCE TO PRINT                            
*                                                                               
         STC   R2,DRLITLNO         PASS ON LENGTH                               
*                                                                               
         DROP  DRH1                                                             
*                                                                               
         GOTO1 GCOLDRON            GENERATE TITLE KEYWORD                       
         CLI   DRERROR,0                                                        
         JNE   *+2                                                              
*                                                                               
DFLTTLCN DS    0H                                                               
*                                                                               
         BXLE  R3,R4,DFLTTLLP                                                   
*                                                                               
DFLTTLX  DS    0H                                                               
*                                                                               
*        IF FLOWCHARTING NON-NUMERIC                                            
*        THEN MUST ADD 'STACK' KEYWORD                                          
*                                                                               
DFLSTCK  DS    0H                                                               
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF ALREADY STACKING                     
         BO    DFLSTCKX                                                         
*                                                                               
         CLI   DRTYPEI,C'C'        IF CHARACTER                                 
         BE    *+8                                                              
         CLI   DRTYPEI,C'X'           HEX                                       
         BE    *+8                                                              
         CLI   DRTYPEI,C'D'           DATE                                      
         BE    *+8                                                              
         B     DFLSTCKX                                                         
*                                  ADD STACK KEYWORD                            
*                                                                               
         LA    R0,DRINFEND-DRLENIN   LENGTH OF MOVE                             
         LA    R2,DRLENIN                                                       
         L     R1,AIO1                                                          
         BAS   RE,MOVERE           SAVE MASTER DRONEBLK                         
*                                                                               
         XC    BLOCK(42),BLOCK                                                  
         MVC   BLOCK(2),=X'0502'   LENGTH OF SPLIT FIELD                        
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(8),=C'STACK'                                            
         MVC   BLOCK+22(2),=C'NP'                                               
*                                                                               
         LA    R4,BLOCK            POINT TO CREATED SCANNER BLOCK               
*                                                                               
         GOTO1 VROWDRON            VALIDATE STACK AS KEYWORD                    
         CLI   DRERROR,0                                                        
         JNE   *+2                                                              
*                                                                               
         NI    DRFLAGO,X'FF'-X'80' MAKE SURE THERE IS NO PRINTING               
         NI    DRHEAD1,X'FF'-X'80'                                              
         NI    DRHEAD2,X'FF'-X'80'                                              
         NI    DRHEAD3,X'FF'-X'80'                                              
         NI    DRHEAD4,X'FF'-X'80'                                              
*                                                                               
* MAY NEED TO ALTER WIDTH                                                       
*                                                                               
         GOTO1 GROWDRON            GENERATE STACK KEYWORD                       
         CLI   DRERROR,0                                                        
         JNE   *+2                                                              
*                                                                               
*        RESTORE MASTER DRONEBLK                                                
*                                                                               
         LA    R0,DRINFEND-DRLENIN   LENGTH OF MOVE                             
         L     R2,AIO1                                                          
         LA    R1,DRLENIN                                                       
         BAS   RE,MOVERE                                                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,KEYWORD#         BUMP KEYWORD NUMBER                          
         LA    RF,1(RF)                                                         
         STC   RF,KEYWORD#                                                      
*                                                                               
DFLSTCKX DS    0H                                                               
*                                                                               
         L     R5,PDADATES         ESTABLISH DATEAREA                           
         USING DATELSTD,R5                                                      
*                                                                               
         ZIC   R7,FLOWSTRT         START OF DATE RANGE (EG W6)                  
         BCTR  R7,0                                                             
*                                                                               
         LA    R7,EXPLIST(R7)      POINT TO CORRECT PLACE IN EXPLIST            
         ZAP   DFLCTR,=P'1'        INIT ITERATION COUNTER                       
*                                                                               
DFLLOOP  DS    0H                                                               
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF NOT STACKING                         
         BNO   DFLLP10                                                          
*                                                                               
         L     RF,ASTACKBF         POINT TO STACK BUFFER                        
         USING STACKBFD,RF         ESTABLISH BUFFER                             
*                                                                               
         LM    R3,R5,STBFBXLE      LOAD BXLE REGISTERS FOR BUFFER               
*                                                                               
         DROP  RF                                                               
*                                                                               
DFLSTKLP DS    0H                                                               
*                                                                               
         STM   R3,R5,DFLBXLE       SAVE IN LOCAL STORAGE                        
*                                                                               
         USING STBFENTD,R3         ESTABLISH BUFFER ENTRY                       
*                                                                               
         LA    R1,DRINFEND-DRLENIN DRONE BLOCK LENGTH                           
         LR    RF,R1               DRONE BLOCK LENGTH                           
         LA    R0,STBFDBLK         FROM                                         
         LA    RE,DRLENIN          TO                                           
*                                                                               
         MVCL  RE,R0               COPY BUFFER                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         MVC   DRLENO,STACKLNO     SET COLUMN WIDTH                             
*                                                                               
         MVC   DRPOSO(2),=C'P+'    PRINT ON NEXT LINE                           
*                                                                               
         NI    DRHEAD1,X'FF'-X'80' DON'T PRINT HEADLINES                        
         NI    DRHEAD2,X'FF'-X'80' DON'T PRINT HEADLINES                        
         NI    DRHEAD3,X'FF'-X'80' DON'T PRINT HEADLINES                        
         NI    DRHEAD4,X'FF'-X'80' DON'T PRINT HEADLINES                        
*                                                                               
         CLC   DRLABELI,STACK1ST   IF FIRST COLUMN                              
         BNE   DFLLP05                                                          
*                                                                               
         MVI   DRPOSO+1,1             PRINT ON FIRST LINE (P1)                  
*                                                                               
*        PRINT FIRST 2 COLUMN HEADINGS                                          
*                                                                               
         CLI   STACKH1L,0          IF STACK HEADLINE EXISTS                     
         BE    DFLLP03                                                          
         CLI   STACKH1L,X'FF'      IF STACK HEADLINE EXISTS                     
         BE    DFLLP03                                                          
******** B     DFLLP04                                                          
*                                                                               
         OI    DRHEAD1,X'80'          PRINT HEADLINE                            
         MVC   DRH1LITL,STACKH1L      PASS LITERAL LENGTH TO DRONE              
         XC    DRH1LIT,DRH1LIT        INIT HEADLINE AREA                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,DRH1LITL                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DRH1LIT(0),STACKH1  PASS LITERAL TO DRONE                        
*                                                                               
DFLLP03  DS    0H                                                               
*                                                                               
         CLI   STACKH2L,0          IF STACK HEADLINE EXISTS                     
         BE    DFLLP04                                                          
         CLI   STACKH2L,X'FF'      IF STACK HEADLINE EXISTS                     
         BE    DFLLP04                                                          
*                                                                               
         OI    DRHEAD2,X'80'          PRINT HEADLINE                            
         MVC   DRH2LITL,STACKH2L      PASS LITERAL LENGTH TO DRONE              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,DRH2LITL                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DRH2LIT(0),STACKH2  PASS LITERAL TO DRONE                        
*                                                                               
DFLLP04  DS    0H                                                               
*                                                                               
DFLLP05  DS    0H                                                               
*                                                                               
DFLLP10  DS    0H                                                               
*                                                                               
         MVC   DRARGSI+8(1),TYPEDATE    THIS ENTRY DATE TYPE                    
*                                                                               
         CLI   TYPEDATE,0    IF NO PREVIOUS VALID DATE TYPE                     
         BNE   *+10                USE DATETYPE IN SCREEN                       
         MVC   DRARGSI+8(1),PBQDATYP                                            
*                                                                               
         CLI   DRTYPEI+1,C' '      IF THIS INFO NOT SPECIFIED                   
         BH    *+8                                                              
         MVI   DRTYPEI+1,C'+'         FORCE ADDITIVE NATURE                     
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF NOT STACKING                         
         BNO   DFLLP15                                                          
*                                                                               
         CLI   FLOWSTRT,0          IF NO DATES SPECIFIED                        
         BNE   DFLLP15                                                          
*                                                                               
         MVC   DRARGSI+9(6),PBQBST      USE PERIOD DATES                        
         B     DFLCHDN                                                          
*                                                                               
DFLLP15  DS    0H                                                               
*                                                                               
         LA    R0,DRINFEND-DRLENIN   LENGTH OF MOVE                             
         LA    R2,DRLENIN                                                       
         L     R1,AIO1                                                          
         BAS   RE,MOVERE           SAVE MASTER DRONEBLK                         
*                                                                               
         L     R5,PDADATES                                                      
         USING DATELSTD,R5                                                      
*                                                                               
         LA    RE,WEEKLIST+52*6    FIND DATE TYPE AND POINT TO DATELIST         
         CLI   PBQ$DTYP,C'W'       WEEKLY                                       
         BE    DFLTYPFD                                                         
*                                                                               
         LA    RE,MNTHLIST+12*6    MONTHLY                                      
         CLI   PBQ$DTYP,C'M'                                                    
         BE    DFLTYPFD                                                         
*                                                                               
         LA    RE,QURTLIST+4*6     QUARTERLY                                    
         CLI   PBQ$DTYP,C'Q'                                                    
         BE    DFLTYPFD                                                         
*                                                                               
         LA    RE,YEARLIST+1*6     YEARLY                                       
         CLI   PBQ$DTYP,C'Y'                                                    
         BE    DFLTYPFD                                                         
*                                                                               
         DC    H'0'                MUST HAVE A VALID DATE TYPE                  
*                                                                               
DFLTYPFD DS    0H                                                               
*                                                                               
         MVC   DRH3OPTS,DRH1OPTS   COPY HEADLINE 1 OPTIONS                      
*                                                                               
         CLI   0(R7),0             PROBLEMS IF AT END OF LIST                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RF,0(R7)            INDEX TO CORRECT DATES                       
         BCTR  RF,0                                                             
         MH    RF,=H'6'                                                         
         AR    RF,RE                                                            
*                                                                               
         MVC   DRARGSI+9(6),0(RF)       START AND END DATES                     
         MVC   DRARGSI+15(1),PBQ$DTYP   DATE TYPE                               
*                                                                               
*        PUT DATES IN HEADLINES                                                 
*                                                                               
         CLC   DRPOSO(2),=C'P+'    SKIP IF NOT FIRST FOR COLUMN                 
         BE    DFLCHDX1                                                         
*                                                                               
         LA    R6,DRH3ALL          POINT TO 3RD HEADLINE                        
DRH3     USING DRH3ALL,R6          ESTABLISH HEADLINE AREA                      
*                                                                               
         CLI   DRH3.DRH3LITL,0     IF HEADLINE ALREADY USED                     
         BE    *+8                                                              
         LA    R6,L'DRH3ALL(R6)    BUMP TO NEXT HEADLINE                        
*                                                                               
         CLI   DRARGSI+15,C'M'     IF MONTH DATES                               
         BNE   DFLTYPCN                                                         
*                                     COLUMN HEADING IS MMM/YY                  
         GOTO1 DATCON,DMCB,(3,DRARGSI+9),(9,DRH3.DRH3LIT)                       
*                                                                               
         MVI   DRH3.DRH3LITL,6     SET TITLE LENGTH TO 6                        
*                                                                               
         TM    PBQPTSW,PBQPTDYQ    IF GROUPING BY YEAR                          
         BNO   *+14                                                             
         MVI   DRH3.DRH3LITL,3         PRINT ONLY MONTH                         
         MVC   DRH3.DRH3LIT+3(3),SPACES                                         
*                                                                               
         B     DFLCHDX                                                          
*                                                                               
DFLTYPCN DS    0H                                                               
*                                                                               
*        FORMAT DATES IN COLUMN HEADING                                         
*                                                                               
         CLI   DRARGSI+15,C'Q'     IF QUARTERLY DATES                           
         BNE   DFLCHDQN                                                         
*                                                                               
         CLI   DRLENO,9            AND COLUMN WIDTH AT MOST 9                   
         BH    DFLCHDQ1                                                         
*                                                                               
         ZIC   RF,0(R7)            FORMAT QUARTER DATES AS 'Q##'                
         CVD   RF,DUB                                                           
         OI    DUB+7,15                                                         
         UNPK  DRH3.DRH3LIT(3),DUB+6(2)                                         
         MVI   DRH3.DRH3LIT,C'Q'                                                
         MVI   DRH3.DRH3LITL,3                                                  
*                                                                               
         B     DFLCHDX                                                          
*                                                                               
DFLCHDQ1 DS    0H                  FORMAT COLUMN HEADING AS MMM-MMM/YY          
*                                                                               
         MVI   DRH3.DRH3LITL,10                                                 
         GOTO1 DATCON,DMCB,(3,DRARGSI+9),(4,DRH3.DRH3LIT)                       
         MVI   DRH3.DRH3LIT+3,C'-'                                              
         GOTO1 DATCON,DMCB,(3,DRARGSI+12),(9,DRH3.DRH3LIT+4)                    
*                                                                               
         B     DFLCHDX                                                          
*                                                                               
DFLCHDQN DS    0H                                                               
*                                                                               
         CLI   DRARGSI+15,C'Y'     IF YEARLY DATES                              
         BNE   DFLCHDYN                                                         
*                                                                               
*        PRINT 4 DIGITS FOR YEAR                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,DRARGSI+12),(23,WORK)                             
*                                                                               
         MVC   DRH3.DRH3LIT(4),WORK                                             
         MVI   DRH3.DRH3LITL,4                                                  
*                                                                               
         B     DFLCHDX                                                          
*                                                                               
DFLCHDYN DS    0H                  ALL ELSE USE MMM/DD                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,DRARGSI+9),(4,DRH3.DRH3LIT)                       
         MVI   DRH3.DRH3LITL,5                                                  
*                                                                               
DFLCHDX  DS    0H                                                               
*                                                                               
         OI    DRH3.DRHEAD3,X'80'  MAKE SURE HEADLINE PRINTS                    
*                                                                               
         CLC   DRH3.DRH3LITL,DRLENO     DEFAULT TO SMALLER                      
         BNH   *+10                                                             
         MVC   DRH3.DRH3LITL,DRLENO                                             
*                                                                               
DFLCHDX1 DS    0H                                                               
*                                                                               
DFLCHDN  DS    0H                                                               
*                                                                               
         MVI   DRARGSI+7,C'$'      INDICATE FLOWCHART KEYWORD                   
         MVC   DRARGSO+7(8),DRARGSI+7 COPY DATE RANGE TO OUTPUT                 
*                                  DON'T INCLUDE RANGE TYPE                     
         MVI   DRNARGSO,16         FORCE MAXIMUM ARGUMENTS                      
         MVI   DRNARGSI,16                                                      
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF STACKING                             
         BO    *+10                   LABEL ALREADY IN DRONEBLOCK               
         MVC   DRLABELI,MYLABEL                                                 
*                                                                               
*        COPY FILTERAREA                                                        
*                                                                               
         L     R3,AFLTTAB          POINT TO FILTER SAVEAREAS                    
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,DRARGSI+6      ORIGINAL KEYWORD#                            
         BZ    *+6                                                              
         BCTR  RE,0                DECREMENT FOR INDEXING                       
         LA    RF,FTBENTL          FILTER AREA LENGTH                           
         MR    RE,RE               INDEX INTO FILTER AREA                       
         LA    R3,0(RF,R3)         AREA FOR THIS KEYWORD                        
*                                                                               
         L     R1,AFLTTAB          POINT TO FILTER SAVEAREAS                    
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,KEYWORD#       CURRENT KEYWORD#                             
         BZ    *+6                                                              
         BCTR  RE,0                DECREMENT FOR INDEXING                       
         LA    RF,FTBENTL          FILTER AREA LENGTH                           
         MR    RE,RE               INDEX INTO FILTER AREA                       
         LA    R1,0(RF,R1)         AREA FOR THIS KEYWORD                        
*                                                                               
         MVC   0(FTBENTL,R1),0(R3) COPY FILTERAREA                              
*                                                                               
         MVC   DRARGSI+6(1),KEYWORD# PASS ALONG KEYWORD NUMBER                  
         MVC   DRARGSO+6(1),KEYWORD# PASS ALONG KEYWORD NUMBER                  
*                                                                               
         GOTO1 GCOLDRON            GENERATE COLUMN                              
*                                                                               
         DROP  DRH3                                                             
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF NOT STACKING                         
         BNO   DFLLBLX                                                          
*                                                                               
         SR    RF,RF                                                            
         L     R6,DROLDBUF         POINT TO START OF BUFFER ENTRY               
         USING DEISD,R6            ESTABLISH AS INPUT LABEL ELEMENT             
*                                                                               
DFLCHDN3 DS    0H                                                               
         CLI   DEISEL,0            SKIP IF END OF BUFFER                        
         BE    DFLLBLX                                                          
         CLI   DEISEL,X'20'        FIND INPUT LABEL ELEMENT                     
         BE    DFLCHDN7                                                         
         IC    RF,DEISLEN          BUMP TO NEXT ELEMENT                         
         LA    R6,DEISD(RF)                                                     
         B     DFLCHDN3                                                         
*                                                                               
DFLCHDN7 DS    0H                                                               
         UNPK  DEISLAB+1(2),DFLCTR ADD ITERATION COUNTER TO LABEL               
         OI    DEISLAB+2,X'F0'     FORCE ZONE                                   
*                                                                               
DFLLBLX  DS    0H                                                               
*                                                                               
         TM    STACKSW,STACKYQ     SKIP IF NOT STACKING                         
         BNO   DFLCONT                                                          
*                                                                               
         L     R3,DFLBXLE          RE-POINT TO CURRENT BUFFER ENTRY             
         USING STBFENTD,R3         ESTABLISH ENTRY                              
*                                                                               
         CLC   =CL8'ICOMPUTE',DRRTNI-DRLENIN+STBFDBLK IF COMPUTE DONE           
         BNE   DFLCOMX             DDRONE CLEARS DRBLOCK ON RETURN              
*                                                                               
         MVC   BLOCK+42(42),STBFSBLK RESTORE SCANNER BLOCK FOR COMPUTE          
         LA    R4,BLOCK+42         SET SCAN BLOCK POINTER                       
*                                                                               
         GOTO1 GCMPDRON            GENERATE COMPUTE DATA                        
*                                                                               
         DROP  R3                                                               
*                                                                               
*                                                                               
         SR    RF,RF                                                            
         L     R6,DROLDBUF         POINT TO START OF BUFFER ENTRY               
         USING DEFRMD,R6           ESTABLISH AS COMP FORMULA ELEMENT            
*                                                                               
DFLCMPLP DS    0H                                                               
*                                                                               
         CLI   DEFRMEL,0           STOP AT END OF BUFFER                        
         BE    DFLCMPDN                                                         
*                                                                               
         CLI   DEFRMEL,X'52'       FIND COMP FORMULA ELEMENT                    
         BNE   DFLCMPCN                                                         
*                                                                               
         CLI   DEFRMON,0           SKIP IF NOT A LABEL                          
         BNE   DFLCMPCN                                                         
*                                                                               
         UNPK  DEFRMLAB+1(2),DFLCTR ADD ITERATION COUNTER TO LABEL              
         OI    DEFRMLAB+2,X'F0'    FORCE ZONE                                   
*                                                                               
DFLCMPCN DS    0H                                                               
*                                                                               
         IC    RF,DEFRMLEN         BUMP TO NEXT ELEMENT                         
         LA    R6,DEFRMD(RF)                                                    
         B     DFLCMPLP                                                         
*                                                                               
DFLCMPDN DS    0H                                                               
*                                                                               
DFLCOMX  DS    0H                                                               
*                                                                               
         B     DFLSCKCN                                                         
*                                                                               
DFLSCKCN DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,KEYWORD#         BUMP KEYWORD NUMBER                          
         LA    RF,1(RF)                                                         
         STC   RF,KEYWORD#                                                      
*                                                                               
         LM    R3,R5,DFLBXLE       RELOAD BXLE REGISTERS                        
         BXLE  R3,R4,DFLSTKLP      HANDLE NEXT IN STACK                         
*                                                                               
         B     DFLCONT1                                                         
*                                                                               
DFLCONT  DS    0H                                                               
*                                                                               
         LA    R0,DRINFEND-DRLENIN   RE-COPY MASTER DRONEBLK                    
         L     R2,AIO1             BECAUSE GENERATION DESTROYS BLOCK            
         LA    R1,DRLENIN                                                       
         BAS   RE,MOVERE                                                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,KEYWORD#         BUMP KEYWORD NUMBER                          
         LA    RF,1(RF)                                                         
         STC   RF,KEYWORD#                                                      
*                                                                               
DFLCONT1 DS    0H                                                               
*                                                                               
         CLI   0(R7),0             DONE IF NO DATES GIVEN                       
         BE    DOFLOWX                                                          
*                                                                               
         LA    R7,1(R7)            BUMP DATE POINTER                            
         AP    DFLCTR,=P'1'        BUMP ITERATION COUNTER                       
*                                                                               
         CLI   FLOWEND,0           DONE IF NO DATE RANGE                        
         BE    DOFLOWX                                                          
*                                                                               
         CLC   0(1,R7),FLOWEND     OR NO MORE DATES AVAILABLE                   
         BH    DOFLOWX                                                          
*                                                                               
         ZIC   RF,MYPOSO           BUMP POSITION COUNTER                        
         LA    RF,1(RF)                                                         
         STC   RF,MYPOSO                                                        
*                                                                               
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
*                                                                               
         B     DFLLOOP                                                          
*                                                                               
DOFLOWX  DS    0H                                                               
*                                                                               
         LA    R0,DRLENIN          CLEAR DRONEBLOCK AREA                        
         LA    R1,DRINFEND-DRLENIN                                              
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XIT1                                                                   
*                                                                               
*===================================================*                           
* SUBROUTINE MOVES REC AT R2 TO R1 FOR LENGTH IN R0 *                           
* RETURN ON RE (R0-R2 AND RF ARE DESTROYED)         *                           
*===================================================*                           
         SPACE 1                                                                
MOVERE   LA    RF,256                                                           
         CR    R0,RF                                                            
         BNH   MOVERE2                                                          
         MVC   0(256,R1),0(R2)                                                  
         AR    R1,RF                                                            
         AR    R2,RF                                                            
         SR    R0,RF                                                            
         B     MOVERE                                                           
*                                                                               
MOVERE2  LTR   RF,R0                                                            
         BZR   RE                                                               
         BCTR  RF,0                                                             
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),0(R2) *EXECUTED*                                         
*                                                                               
         DS    CL256               FOR NEW PROCESSOR                            
DFLBXLE  DS    3A                  BXLE REGISTERS SAVEAREA                      
DFLCTR   DS    PL2                 ITERATION COUNTER                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRWRIOFF - GENERAL HEADLINE HOOK - GENHD'                       
***********************************************************************         
*                                                                     *         
*        GENERAL HEADLINE HOOK ROUTINES                               *         
*                                                                     *         
***********************************************************************         
         SPACE  2                                                               
         DS    0D                                                               
GENHD    NMOD1 0,**#GHD                                                         
*                                                                               
         USING SPOOLD,R8                                                        
         USING SYSD,R9                                                          
*                                                                               
         LA    RC,SPOOLEND         RESET WORKING STORAGE POINTER                
         USING GEND,RC                                                          
*                                                                               
         L     R2,AH1              DEAL WITH MAIN TITLE                         
*                                                                               
         LA    R2,32(R2)           DETERMINE TITLE START                        
*                                                                               
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,16(R2)                                                        
*                                                                               
         LA    RF,TITLE            POINT TO DEFAULT TITLE                       
*                                                                               
         LH    RE,=Y(RP2TITLE-SYSD)    POINT TO SECOND REPORT TITLE             
         LA    RE,SYSD(RE)                                                      
*                                                                               
         CLC   0(L'RP2TITLE,RE),SPACES   SKIP IF NOT DOING A SECOND RPT         
         BNH   VGENHR2X                                                         
*                                                                               
         L     R1,AGLOBAL          ESTABLISH DRIVER CONTROL BLOCK               
         USING GLOBALD,R1                                                       
*                                                                               
         CLI   GLRECNO,2           IF WE ARE DOING SECOND REPORT                
         BNE   *+6                                                              
         LR    RF,RE                  POINT TO 2ND RPT TITLE                    
*                                                                               
         DROP  R1                                                               
*                                                                               
VGENHR2X DS    0H                                                               
*                                                                               
         MVC   0(L'TITLE,R2),0(RF)     (TITLES ARE ALREADY CENTERED)            
*                                                                               
         A     R2,PWIDTH                                                        
*                                                                               
         ST    RF,DMCB                                                          
         LA    R1,L'TITLE                                                       
         STC   R1,DMCB                                                          
*                                                                               
         GOTO1 UNDERLIN,DMCB,,(X'BF',(R2))                                      
*                                                                               
         A     R2,PWIDTH                                                        
         LA    R2,14(R2)                                                        
*                                                                               
         MVC   0(L'SUBTITLE,R2),SUBTITLE   AND THE SUBTITLE                     
*                                                                               
         L     R2,AH4                                                           
         A     R2,PWIDTH           R2=(AHADLINE 5)                              
*                                                                               
         LA    R4,96               DISPLACEMENT TO 2ND HALF OF HEAD             
*                                                                               
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R4,128                                                           
*                                                                               
         LA    R3,0(R4,R2)                                                      
*                                                                               
         CLI   PBQTEST,C'D'        ONLY DELETED BUY S             L23           
         BNE   CK4TSTBT                                           L23           
*                                                                               
         MVC   0(36,R3),=C'** ONLY DELETED INSERTIONS INCLUDED*' L23            
         B     AR3PWID                                             L23          
*                                                                               
CK4TSTBT CLI   PBQTEST,C'T'        TEST ONLY BUYS                 L10           
         BNE   CK4TSTBY                                      '    L10           
*                                                                               
         MVC   0(36,R3),=C'* ONLY PROPOSED INSERTIONS INCLUDED*'                
         B     AR3PWID                                                          
*                                                                               
CK4TSTBY CLI   PBQTEST,C'Y'        INCLUDE TEST BUYS                            
         BNE   CK4SPDSC                                                         
*                                                                               
         MVC   0(34,R3),=C'*INCLUDES ANY PROPOSED INSERTIONS*'                  
*                                                                               
AR3PWID  DS    0H                                                               
*                                                                               
         OC    PBQDELST,PBQDELST                                   L24          
         BZ    AR3PWID1                                            L24          
*                                                                               
         A     R3,PWIDTH                                           L24          
         CLI   PBQDELND,0                                                       
         BNE   DLSNCE                                                           
         MVC   0(13,R3),=C'DELETED SINCE'                                       
         LA    R4,14(R3)                                                        
         B     GDATCON                                                          
*                                                                               
DLSNCE   MVC   0(12,R3),=C'DELETED FROM'                          L24           
         LA    R4,13(R3)                                           L24          
*                                                                  L24          
GDATCON  GOTO1 DATCON,DMCB,(3,PBQDELST),(5,(R4))                   L24          
         CLI   PBQDELND,0                                          L24          
         BE    AR3PWID1                                            L24          
         LA    R4,8(R4)                                            L24          
         MVI   0(R4),C'-'                                          L24          
         LA    R4,1(R4)                                            L24          
         GOTO1 (RF),(R1),(3,PBQDELND),(5,(R4))                     L24          
         SPACE 3                                                   L24          
AR3PWID1 A     R3,PWIDTH                                                        
CK4SPDSC CLI   PBQADDGS,C'Y'                                      L16           
         BNE   CK4SPDSX                                           L16           
         MVC   0(36,R3),=C'*GST ADDED TO NET AND GROSS ORDERED*'    L16         
         A     R3,PWIDTH                                          L16           
         B     CK4SPDSD                                            L21          
CK4SPDSX CLI   PBQADDGS,C'O'                                      L21           
         BNE   CK4SPDSD                                           L21           
         MVC   0(12,R3),=C'*GST ADDED *'                          L21           
         A     R3,PWIDTH                                          L21           
CK4SPDSD DS    0H                                                               
         TM    PBQHLDSW,PBQRELOQ   IF RELEASED BUYS ONLY                        
         BNO   *+14                                                             
         MVC   0(30,R3),=C'** RELEASED INSERTIONS ONLY **'                      
         A     R3,PWIDTH                                          L21           
*                                                                               
         TM    PBQHLDSW,PBQHLDOQ   IF HELD BUYS ONLY                            
         BNO   *+14                                                             
         MVC   0(27,R3),=C'** HELD INSERTIONS ONLY  **'                         
         A     R3,PWIDTH                                          L21           
*                                                                               
         TM    PBQHLDSW,PBQHSRNQ   IF NO *RATE BUYS                             
         BNO   *+14                                                             
         MVC   0(31,R3),=C'** *RATE INSERTIONS EXCLUDED **'                     
         A     R3,PWIDTH                                          L21           
*                                                                               
         TM    PBQHLDSW,PBQHSROQ   IF *RATE BUYS ONLY                           
         BNO   *+14                                                             
         MVC   0(27,R3),=C'** *RATE INSERTIONS ONLY **'                         
         A     R3,PWIDTH                                          L21           
*                                                                               
         CLC   PBQSPDS1,SPACES                                                  
         BH    PRNTDSCR                                                         
         CLC   PBQSPDS1,SPACES                                                  
         BNH   SEEIFLTR                                                         
PRNTDSCR MVC   0(18,R3),=C'** SPACE FILTER **'                                  
         CLC   PBQSPDS1,SPACES                                                  
         BNH   SEEIFSP2                                                         
*                                                                               
         LA    RF,19(R3)           POINT TO FILTER DESCRIPTION                  
*                                                                               
         TM    PBQSPD1L,X'80'      IF NEGATIVE FILTER                           
         BNO   *+12                                                             
         MVI   0(RF),C'-'             SET MINUS SIGN                            
         LA    RF,1(RF)               BUMP POINTER                              
*                                                                               
         MVC   0(17,RF),PBQSPDS1                                                
*                                                                               
         A     R3,PWIDTH                                                        
*                                                                               
SEEIFSP2 CLC   PBQSPDS2,SPACES                                                  
         BNH   SEEIFLTR                                                         
*                                                                               
         LA    RF,19(R3)           POINT TO FILTER DESCRIPTION                  
*                                                                               
         TM    PBQSPD2L,X'80'      IF NEGATIVE FILTER                           
         BNO   *+12                                                             
         MVI   0(RF),C'-'             SET MINUS SIGN                            
         LA    RF,1(RF)               BUMP POINTER                              
*                                                                               
         MVC   0(17,RF),PBQSPDS2                                                
*                                                                               
         A     R3,PWIDTH                                                        
*                                                                               
SEEIFLTR CLI   PBQFLTDA,0                                                       
         BE    HDHOOKX      OUT                                                 
         CLI   PBQFLTDA,C'A'                                                    
         BNE   *+14                                                             
         MVC   0(21,R3),=C'++ UNCLEARED ITEMS ++'                               
         B     HDHOOKX                                                          
*                                                                               
         CLI   PBQFLTDA,C'B'                                                    
         BNE   *+14                                                             
         MVC   0(18,R3),=C'++ BILLED ITEMS ++'                                  
         B     HDHOOKX                                                          
*                                                                               
         CLI   PBQFLTDA,C'P'                                                    
         BNE   *+14                                                             
         MVC   0(16,R3),=C'++ PAID ITEMS ++'                                    
         B     HDHOOKX                                                          
*                                                                               
         CLI   PBQFLTDA,C'I'                                                    
         BNE   *+14                                                             
         MVC   0(20,R3),=C'++ BILLABLE ITEMS ++'                                
         B     HDHOOKX                                                          
*                                                                               
         CLI   PBQFLTDA,C'O'                                                    
         BNE   *+14                                                             
         MVC   0(19,R3),=C'++ ORDERED ITEMS ++'                                 
         B     HDHOOKX                                                          
*                                                                               
         CLI   PBQFLTDA,C'U'                                                    
         BNE   *+14                                                             
         MVC   0(21,R3),=C'++ UNORDERED ITEMS ++'                               
*                                                                               
HDHOOKX  DS    0H                                                               
         CLI   PBQCDFLT,C'Y'                                      L11           
         BNE   *+10                                               L11           
         MVC   0(18,R3),=C'** CD PUBS ONLY **'                                  
         CLI   PBQCDFLT,C'N'                                      L11           
         BNE   *+10                                               L11           
         MVC   0(22,R3),=C'** NON-CD PUBS ONLY **'                              
         L     R2,AH4              LINE UP THE LEFT SIDE                        
         LA    R2,1(R2)                                                         
         LA    R3,15                                                            
***      =================  ***                                                 
         L     R1,AGLOBAL                                                       
         USING GLOBALD,R1                                                       
***      =================  ***                                                 
         ZIC   R4,GLFHEADL                                                      
         DROP  R1                                                               
         SH    R4,=H'6'                                                         
         BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         LA    R3,12                                                            
         BAS   RE,GETLONG                                                       
         LA    R3,13(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
*                                                                               
         L     R2,AH4                                                           
         A     R2,PWIDTH           R2=(AHADLINE 5)                              
*                                                                               
*        PRINT ANY DATE FILTERS                                                 
*                                                                               
         MVC   WORK,SPACES                                                      
         LA    R3,WORK                                                          
*                                                                               
VGHBRN   DS    0H                                                               
*                                                                               
         OC    PBQBRNST,PBQBRNST   CHECK FOR RUN DATES                          
         BZ    VGHBRNN                                                          
*                                                                               
         LA    R4,PBQBRNST         POINT TO RUN DATES                           
         MVC   WORK(10),=C'BILLS RUN '  TITLE                                   
         LA    R3,10(R3)           STARTING POINT FOR DATES                     
         B     VGHDTES             GO PRINT DATES                               
*                                                                               
VGHBRNN  DS    0H                                                               
*                                                                               
VGHBDU   DS    0H                                                               
*                                                                               
         OC    PBQBDUST,PBQBDUST   CHECK FOR DUE DATES                          
         BZ    VGHBDUN                                                          
*                                                                               
         LA    R4,PBQBDUST         POINT TO DUE DATES                           
         MVC   WORK(10),=C'BILLS DUE '  TITLE                                   
         LA    R3,10(R3)           STARTING POINT FOR DATES                     
         B     VGHDTES             GO PRINT DATES                               
*                                                                               
VGHBDUN  DS    0H                                                               
*                                                                               
VGHBIV   DS    0H                                                               
*                                                                               
         OC    PBQBIVST,PBQBIVST   CHECK FOR INVOICE DATES                      
         BZ    VGHBIVN                                                          
*                                                                               
         LA    R4,PBQBIVST         POINT TO INVOICE DATES                       
         MVC   WORK(15),=C'INVOICES DATED '  TITLE                              
         LA    R3,15(R3)           STARTING POINT FOR DATES                     
         B     VGHDTES             GO PRINT DATES                               
*                                                                               
VGHBIVN  DS    0H                                                               
*                                                                               
VGHBPS   DS    0H                                                               
*                                                                               
         OC    PBQBPSST,PBQBPSST   CHECK FOR POSTING DATES                      
         BZ    VGHBPSN                                                          
*                                                                               
         LA    R4,PBQBPSST         POINT TO POSTING DATES                       
         MVC   WORK(12),=C'BILLS POSTED '  TITLE                                
         LA    R3,12(R3)           STARTING POINT FOR DATES                     
         B     VGHDTES             GO PRINT DATES                               
*                                                                               
VGHBPSN  DS    0H                                                               
*                                                                               
VGHATV   DS    0H                                                               
*                                                                               
         OC    PBQATVST,PBQATVST   CHECK FOR ACTIVITY DATES                     
         BZ    VGHATVN                                                          
*                                                                               
         LA    R4,PBQATVST         POINT TO ACTIVITY DATES                      
         MVC   WORK(13),=C'BUY ACTIVITY '  TITLE                                
         LA    R3,13(R3)           STARTING POINT FOR DATES                     
         B     VGHDTES             GO PRINT DATES                               
*                                                                               
VGHATVN  DS    0H                                                               
*                                                                               
         B     VGHDTESX            NO FILTER DATES TO PRINT                     
*                                                                               
VGHDTES  DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,(R4)),(5,(R3)) START DATE                         
*                                                                               
         OC    3(3,R4),3(R4)       SKIP IF NO END DATE                          
         BZ    VGHCNTR                                                          
*                                                                               
         LA    R3,8(R3)            POINT TO END OF PRINTED START DATE           
         MVI   0(R3),C'-'          SET SEPARATOR                                
         LA    R3,1(R3)            BUMP TO NEXT PRINT PPOSITION                 
*                                                                               
         LA    R4,3(R4)            POINT TO END DATE                            
*                                                                               
         GOTO1 (RF),(R1),(3,(R4)),(5,(R3))  PRINT END DATE                      
*                                                                               
VGHCNTR  DS    0H                                                               
*                                                                               
         GOTO1 CENTER,DMCB,WORK,36   CENTER TITLE                               
*                                                                               
         LA    R4,46(R2)           FIND STARTING POINT FOR DATES                
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R4,16(R4)                                                        
*                                                                               
         MVC   0(36,R4),WORK       PRINT DATE RANGE                             
*                                                                               
VGHDTESX DS    0H                                                               
*                                                                               
*  LOAD HEADINGS EVERY TIME SINCE HEAD MAY CHANGE BASED ON REQUEST              
*     EG REQUEST MAY BE FOR ALL PRODUCTS - EACH PROD ON SEPERATE PAGE           
*                                                                               
         ICM   R1,15,PBASVHED      POINT TO HEADLINE SAVEAREA                   
         BZ    XXITT               NO STANARD COMMENT                           
         L     R2,AH4              POINT TO START OF VARIABLE HEADLINES         
*                                                                               
         TM    PBASVHED,X'40'      IF TIME TO PRINT COMMENT HEADINGS            
         BNO   *+12                                                             
         L     R1,AH4                SWAP POINTERS                              
         L     R2,PBASVHED                                                      
*                                                                               
         LA    R4,10               NUMBER OF HEADLINES TO BE DAVED              
         L     RF,PWIDTH           LENGTH OF HEADLINE                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
HDHLOOP  DS    0H                                                               
*                                                                               
         EX    RF,HDHCLC           STOP COPYING AT 1ST BLANK LINE               
         BE    HDHLOOP2                                                         
*                                                                               
         EX    RF,HDHMVC           ELSE COPY LINE                               
*                                                                               
HDHCONT  DS    0H                                                               
*                                                                               
         A     R1,PWIDTH           BUMP POINTERS                                
         A     R2,PWIDTH                                                        
         BCT   R4,HDHLOOP                                                       
*                                                                               
         B     HDHDONE             END OF HEADLINES                             
*                                                                               
HDHLOOP2 DS    0H                                                               
*                                                                               
         EX    RF,HDHBLANK         BLANK OUT REST OF HEADLINES                  
*                                                                               
HDHCONT2 DS    0H                                                               
*                                                                               
         A     R1,PWIDTH           BUMP POINTERS                                
         A     R2,PWIDTH                                                        
         BCT   R4,HDHLOOP2                                                      
*                                                                               
HDHDONE  DS    0H                                                               
*                                                                               
         OI    PBASVHED,X'80'  TABLE LOADED                                     
*                                                                               
         B     XXITT                                                            
*                                                                               
HDHCLC   CLC   0(0,R2),HDHSPACS    CHECKS FOR BLANK LINE                        
HDHMVC   MVC   0(0,R1),0(R2)       COPIES A HEADLIEN                            
HDHBLANK MVC   0(0,R1),HDHSPACS    BLANKS OUT A LINE                            
*                                                                               
HDHSPACS DC    CL198' '            WORK SPACES                                  
*                                                                               
         EJECT                                                                  
* SUBSIDIARY ROUTINES FOR GENHEAD                                               
*                                                                               
         SPACE 1                                                                
GETLONG  NTR1                                                                   
*                                                                               
* INPUT  : R2=A(FIELD ON FIRST LINE)                                            
*          R3=MAX WIDTH                                                         
*          R4=NUMBER OF LINES                                                   
* OUTPUT : FULL=WIDEST FOUND                                                    
*                                                                               
GETLONG2 ST    R3,FULL                                                          
         LTR   R3,R3                                                            
         BZ    XXITT                                                            
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                R1=END OF PRESENT FIELD                      
         LR    R0,R4                                                            
*                                                                               
GETLONG4 CLI   0(R1),C' '          SEE IF ANYTHING SIGNIFICANT                  
         BH    XXITT                                                            
         A     R1,PWIDTH           ON EACH OF THE LINES                         
         BCT   R0,GETLONG4                                                      
         BCTR  R3,0                                                             
         B     GETLONG2                                                         
         SPACE 2                                                                
SHUFFLE  NTR1                                                                   
*                                                                               
* INPUT  : R2=A(START DATA ON FIRST LINE)                                       
*          R3=A(FROM DATA)                                                      
*          R4=NUMBER OF LINES                                                   
*                                                                               
         LA    RF,96               DISPLACEMENT TO 2ND HALF OF HEAD             
*                                                                               
         CLI   WIDEOPT,C'Y'        WIDE REPORT                                  
         BNE   *+8                                                              
         LA    RF,128                                                           
*                                                                               
         CLI   NARROPT,C'Y'        NARROW REPORT                                
         BNE   *+8                                                              
         LA    RF,59                                                            
*                                                                               
         L     RE,AH4              POINT TO FIRST HEADLINE                      
         LA    RF,0(RF,RE)         START OF RIGHT HAND TITLES                   
*                                                                               
         SR    RF,R3               MAX LENGTH TO MOVE                           
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
SHUFFLE2 DS    0H                                                               
*                                                                               
         EX    RF,SHUFSAVE         SAVE DATA                                    
         EX    RF,SHUFCLR          CLEAR DATA AREA                              
         EX    RF,SHUFMVC          MOVE DATA TO NEW AREA                        
*                                                                               
         A     R2,PWIDTH                                                        
         A     R3,PWIDTH                                                        
*                                                                               
         BCT   R4,SHUFFLE2                                                      
*                                                                               
XXITT    MVI   FLDINSET,0            CLEAR TEMP WORK AREA                       
         XIT1                                                                   
*                                                                               
SHUFSAVE MVC   SHUFWORK(0),0(R3)   SAVE OLD DATA                                
SHUFCLR  MVC   0(0,R3),SPACES      CLEAR OLD AREA                               
SHUFMVC  MVC   0(0,R2),SHUFWORK    MOVE DATA TO NEW AREA                        
         DS    CL256               FOR NEW PROCESSOR                            
SHUFWORK DS    XL128               WORKAREA                                     
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRWRIOFF - PRINT FOOT COMMENTS - GENFT'                         
***********************************************************************         
*                                                                     *         
*        ROUTINE TO PRINT GENERAL FOOT LINES                          *         
*                                                                     *         
*NTRY    FOOTCOM  = 6 BYTE STANDARD COMMENT CODE                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
GENFT    NMOD1 0,**#GFT                                                         
*                                                                               
***      =================  ***                                                 
         USING SPOOLD,R8                                                        
         USING GEND,RC                                                          
         USING SYSD,R9                                                          
         USING T405FFD,RA                                                       
***      =================  ***                                                 
         LA    RC,SPOOLEND         RESET WORKING STORAGE POINTER                
*                                                                               
         CLI   PRNTSW,C'C'         EXIT IF COVER SHEET JUST PRINTED             
         BNE   *+12                  FOOTING NOT NEEDED ON COVER SHEET          
         MVI   PRNTSW,C'Y'           RESET SWITCH TO SOMETHING PRINTED          
         B     GENFOOTZ                                                         
*                                                                               
*        SEARCH FOR COMMENT IN TABLE                                            
*                                                                               
         L     R1,AP1              SAVE CURRENT DETAIL LINE                     
         MVC   GFTCPSAV,0(R1)                                                   
*                                                                               
         MVI   0(R1),C' '                                                       
         L     RF,PWIDTH           DETAIL LINE LENGTH                           
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),0(R1)       CLEAR PRINT LINE                             
*                                                                               
         ICM   R4,15,GFTCNEXT      POINT TO NEXT ENTRY IN TABLE                 
         BNZ   GFTCTBLX            TABLE ALREADY INITIALIZED                    
*                                                                               
*        READ COMMENT RECORD AND ADD TO TABLE                                   
*                                                                               
GFTCRD   DS    0H                                                               
*                                                                               
         MVC   AIO,AIO3            USE I/O AREA 3 FOR RECORD                    
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R5,KEY                                                           
         USING PCOMRECD,R5         ESTABLISH AS COMMENT RECORD                  
*                                                                               
         MVC   PCOMKAGY,PBAGY      SET AGENCY                                   
*                                                                               
         MVC   PCOMKMED,PBMED      SET MEDIA                                    
*                                                                               
         CLI   PCOMKMED,C'C'       IF REPORTING ALL MEDIA                       
         BE    *+8                                                              
         CLI   PCOMKMED,C'*'                                                    
         BNE   *+8                                                              
         MVI   PCOMKMED,C'M'       USE MAGAZINE FOR COMMENTS                    
*                                                                               
         MVI   PCOMKRCD,X'40'      SET UP AS COMMENT RECORD                     
         MVC   PCOMKNUM,FOOTCOM    SET COMMENT NUMBER                           
*                                                                               
         GOTO1 HIGH                READ RECORD                                  
*                                                                               
         CLC   KEY(L'PCOMKEY),KEYSAVE MAKE SURE WE GOT CORRECT ONE              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
*        SAVE COMMENT LINES                                                     
*                                                                               
         XC    GFTCMAXL,GFTCMAXL   INIT MAX COMMENT LINE LENGTH                 
*                                                                               
         LA    R4,GFTCENT                                                       
GFT      USING GFTCENT,R4          ESTABLISH AS TABLE ENTRY                     
         XC    GFT.GFTCENT(GFTCENTL),GFT.GFTCENT  INIT ENTRY IN TABLE           
*                                                                               
         STCM  R4,15,GFTCNEXT      SET TO PRINT FIRST LINE OF COMMENT           
*                                                                               
         L     R5,AIO3                                                          
         LA    R6,33(R5)           POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
GFTCRDLP DS    0H                                                               
*                                                                               
PCOM     USING PCOMCELM,R6         ESTABLSH COMMENT DATA ELEMENT                
*                                                                               
         CLI   PCOM.PCOMCELM,0     EOR                                          
         BE    GFTCRDDN                                                         
*                                                                               
         CLI   PCOM.PCOMCELM,X'40' ONLY WANT COMMENT ELEMENTS                   
         BNE   GFTCRDCN                                                         
*                                                                               
         CLC   =C'++START',PCOM.PCOMDT  IF ++START                              
         BE    GFTCRDCN              SKIP - FOR ESTIMATE REPORT                 
*                                                                               
         CLC   =C'++END',PCOM.PCOMDT    IF ++END                                
         BE    GFTCRDCN              SKIP - FOR ESTIMATE REPORT                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PCOM.PCOMCELM+1  ELEMENT'S LENGTH                             
         SH    RF,=Y(PCOMDT-PCOMCELM) DETERMINE COMMENT'S LENGTH                
         BNP   GFTCRDCN            MUST HAVE SOME LENGTH                        
*                                                                               
         MVC   GFT.GFTCID,FOOTCOM  SET COMMENT ID                               
*                                                                               
         LA    R1,PCOM.PCOMDT      POINT TO START OF COMMENT                    
*                                                                               
         CLI   0(R1),C'+'          SKIP UNLESS LINES TO BE SKIPPED              
         BNE   GFTCRD30                                                         
*                                                                               
         PACK  DUB,1(1,R1)         NUMBER OF LINES TO SKIP                      
         CVB   RE,DUB                                                           
         STC   RE,GFT.GFTCSPAC     SET SPACING FOR SPOOL                        
*                                                                               
         LA    R1,2(R1)            POINT TO BODY OF COMMENT                     
         SH    RF,=H'2'            ADJUST COMMENT LENGTH                        
         BP    GFTCRD30            GO PROCESS LINE OF COMMENT                   
*                                                                               
         B     GFTCRDCN            FIND NEXT COMMENT ELEMENT                    
*                                                                               
GFTCRD30 DS    0H                                                               
*                                                                               
         CH    RF,GFTCMAXL         SAVE MAXIMUM COMMENT LENGTH                  
         BNH   *+8                                                              
         STH   RF,GFTCMAXL                                                      
*                                                                               
         BCTR  RF,0                DECREMENT LENGTH FOR EXECUTE                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   GFT.GFTCLINE(0),0(R1)   MOVE COMMENT TO TABLE                    
*                                                                               
         LA    R4,GFTCENTL(R4)     BUMP TABLE ENTRY POINTER                     
         XC    GFT.GFTCENT(GFTCENTL),GFT.GFTCENT  INIT ENTRY IN TABLE           
*                                                                               
GFTCRDCN DS    0H                                                               
*                                                                               
         IC    RF,PCOM.PCOMCELM+1   BUMP TO NEXT ELEMENT                        
         LA    R6,0(RF,R6)                                                      
         B     GFTCRDLP                                                         
         DROP  PCOM                                                             
*                                                                               
GFTCRDDN DS    0H                                                               
*                                                                               
*        DETERMINE STARTING PRINT POSITION FOR COMMENT                          
*                                                                               
         LA    RE,132              DEFAULT PAGE WIDTH                           
*                                                                               
         CLI   NARROPT,C'Y'        IF NARROW REPORT                             
         BNE   *+8                                                              
         LA    RE,80               WIDTH IS 80                                  
*                                                                               
         CLI   WIDEOPT,C'Y'        IF WIDE REPORT                               
         BNE   *+8                                                              
         LA    RE,165              WIDTH IS 165                                 
*                                                                               
         SH    RE,GFTCMAXL         EXCESS SPACE OUTSIDE COMMENTS                
         SRL   RE,1                HALVE                                        
         BNM   *+6                                                              
         SR    RE,RE               DEFAULT TO 1ST POSITION IF NO ROOM           
*                                                                               
         CLI   LEFTOPT,C'Y'        IF LEFT OPTION ON                            
         BNE   *+6                                                              
         SR    RE,RE               DEFAULT TO 1ST POSITION IF NO ROOM           
*                                                                               
         STH   RE,GFTCDISP         SAVE DISPLACEMENT TO COMMENT START           
*                                                                               
         ICM   R4,15,GFTCNEXT      POINT TO START OF COMMENT                    
*                                                                               
GFTCRDX  DS    0H                                                               
*                                                                               
GFTCTBLX DS    0H                                                               
*                                                                               
*        PRINT COMMENT                                                          
*                                                                               
GFTCPRT  DS    0H                                                               
*                                                                               
         ICM   R3,15,ABOX          POINT TO BOX CONTROL BLOCK                   
         BZ    GFTCBOXX              NOT USING BOXES                            
         USING BOXD,R3                                                          
*                                                                               
         CLI   BOXSTAT,C'I'        IF INSIDE BOXES                              
         BNE   GFTCBOXX                                                         
*                                                                               
         MVI   BOXREQ,C'C'         ASK TO CLOSE BOXES                           
*                                                                               
         MVI   CLEARHED,C'N'       STOP HEADLINE CLEARING                       
         MVI   SKIPSPEC,C'Y'       STOP NEW PAGE LOGIC IN SPOOL                 
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  CLOSE BOXES                                  
*                                                                               
GFTCBOXX DS    0H                                                               
*                                                                               
         MVI   SKIPSPEC,C'Y'       STOP NEW PAGE LOGIC IN SPOOL                 
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT A SPACING LINE                         
*                                                                               
         SR    R0,R0                                                            
*                                                                               
GFTCPRTL DS    0H                                                               
*                                                                               
         CLI   GFT.GFTCSPAC,0      SKIP IF NO SPACING REQUIRED                  
         BE    GFTCPRT1                                                         
*                                                                               
         MVI   SKIPSPEC,C'Y'       STOP NEW PAGE LOGIC IN SPOOL                 
         MVC   SPACING,GFT.GFTCSPAC  SET LINES TO SPACE BEFORE PRINTING         
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT BLANK LINES                            
*                                                                               
GFTCPRT1 DS    0H                                                               
*                                                                               
         L     RE,AP1              POINT TO PRINT AREA                          
         AH    RE,GFTCDISP         ADD ON DISPLACEMENT FOR CENTERING            
*                                                                               
         LH    RF,GFTCMAXL         GET LENGTH OF PRINT AREA USED                
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),GFT.GFTCLINE    PUT OUT COMMENT LINE                     
*                                                                               
         MVI   SKIPSPEC,C'Y'       STOP NEW PAGE LOGIC IN SPOOL                 
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT COMMENT LINE                           
*                                                                               
GFTCPRTC DS    0H                                                               
*                                                                               
         LA    R4,GFTCENTL(R4)     BUMP TO NEXT LINE OF COMMENT                 
*                                                                               
         CLC   GFT.GFTCID,FOOTCOM  TEST FOR END OF COMMENT                      
         BE    GFTCPRTL              NO - PRINT NEXT LINE                       
*                                                                               
GFTCPRTD DS    0H                                                               
*                                                                               
         MVI   SKIPSPEC,C'N'       RESET SPEC SKIPPING                          
*                                                                               
GENFOOTX DS    0H                                                               
*                                                                               
         L     R1,AP1              POINT TO CURRENT DETAIL LINE AREA            
         L     RF,PWIDTH           DETAIL LINE LENGTH                           
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),GFTCPSAV    RESTORE PRINT LINE                           
*                                                                               
         MVI   CLEARHED,C'Y'       RESUME CLEARING HEADLINES                    
*                                                                               
GENFOOTZ DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    CL256               FOR NEW PROCESSOR                            
GFTCTAB  DS    0D                  COMMENT STORAGE TABLE                        
*                                                                               
GFTCPSAV DS    XL256               PRINT LINE SAVE AREA                         
*                                                                               
GFTCNEXT DC    XL4'00'             FIRST LINE OF COMMENT IN TABLE               
GFTCMAXL DC    H'0'                MAXIMUM LENGTH OF A COMMENT LINE             
GFTCDISP DC    H'0'                DISP OF COMMENT INTO PRINTLINE               
*                                                                               
GFTCENT  DS    0X                  ENTRY IN TABLE                               
GFTCID   DS    XL(L'PCOMKNUM)      COMMENT ID                                   
GFTCSPAC DS    X                   SPACING BEFORE PRINTING LINE                 
GFTCLINE DS    XL70                COMMENT LINE                                 
GFTCENTL EQU   *-GFTCENT           LENGTH OF TABLE ENTRY                        
         DS    16XL(GFTCENTL)      REST OF TABLE                                
GFTCTABX EQU   *-1                                                              
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE   'PRWRIOFF DSECTS / STORAGE'                                    
       ++INCLUDE PRWRIWORKD                                                     
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*        INCLUDE DDSPOOLD                                                       
*        INCLUDE DDSPLWORKD                                                     
*        INCLUDE PRWRIFFD                                                       
*        INCLUDE DDGENTWA                                                       
*        INCLUDE CTGENDIC                                                       
*        INCLUDE CTGENFILE                                                      
*        INCLUDE CTGENADVD                                                      
*        INCLUDE FAFACTS                                                        
*        INCLUDE FATIOB                                                         
*        INCLUDE DDCOMFACS                                                      
*        INCLUDE DRGLOBAL                                                       
*        INCLUDE DDBIGBOX                                                       
*        INCLUDE DDWIDED                                                        
*        INCLUDE DDOFFICED                                                      
*        INCLUDE DRONEBLKHD                                                     
*        INCLUDE DDCOREQUS                                                      
*        INCLUDE DDMASTD                                                        
*        INCLUDE DDPERVALD                                                      
*        INCLUDE PRVALTABD                                                      
*        INCLUDE PRGLOBEQUS                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PRWRIFFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE CTGENDIC                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENADVD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DRONEBLKHD                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE PRVALTABD                                                      
       ++INCLUDE PRGLOBEQUS                                                     
         EJECT                                                                  
         PRINT ON                                                               
* PRGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE PRDATELSTD                                                     
WEELLENT EQU   *-WEEKLIST                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052PRWRIOFF  07/08/19'                                      
         END                                                                    
