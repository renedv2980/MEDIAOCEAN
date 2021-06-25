*          DATA SET PPREPXK02N AT LEVEL 034 AS OF 05/01/02                      
*PHASE PPXK02A,*                                                                
*INCLUDE SORTER                                                                 
*INCLUDE GETINS                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         SPACE 2                                                                
*===========================================================*                   
* THIS VERSION IS USED FOR WEEKLY ADVERTISER FILE TRANSFERS *                   
*===========================================================*                   
         TITLE 'PPXK02 - ADV  FILE DATA TRANSFER'                               
**************************                                                      
* REGISTER USEAGE                                                               
*  RB,RC BASE REGISTERS                                                         
*  R9,RA COVERS PRINT WORK DSECT                                                
***************************                                                     
         SPACE                                                                  
*                                                                               
PPXK02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPXK02                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
         MVI   RC2DSECT,C'Y'                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING PPXK02,RB,RC      TWO BASE REGESTERS                             
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    XK10                                                             
         CLI   MODE,RUNLAST                                                     
         BE    XK30                                                             
         B     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*===============================================================*               
* RUNFRST PROCESSING                                            *               
*===============================================================*               
         SPACE 1                                                                
XK10     DS    0H                                                               
*                                                                               
         XC    BINTABL,BINTABL                                                  
         LA    R1,BINTABL                                                       
         USING BSPARA,R1                                                        
         MVC   BSPADD,=A(TOTBWORK) ADD OF WORK AREA                             
         MVI   BSPADD,1                                                         
         MVC   BSPSTRT,=A(TOTBUFF)                                              
         MVI   BSPKEYD,0           KEY DISPLACEMENT                             
         MVC   BSPLENK,=F'7'       KEY LENGTH                                   
         LA    RF,61               NUMBER OF RECORDS                            
         ST    RF,BSPEND                                                        
         DROP  R1                                                               
*                                                                               
         L     R1,=A(TOTBUFF)                                                   
         L     R0,=A(TOTBUFFX)                                                  
         BAS   RE,CLEAR                                                         
         OPEN  (FILEIN,(INPUT))       OPEN ADVERTISER BACKUP TAPE               
         LTR   RF,RF                                                            
*        BZ    *+2                                                              
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
*        BZ    *+2                                                              
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         B     EXIT                                                             
*        B     XK30                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2004'                                  
         EJECT                                                                  
*===============================================================*               
* CLTFRST PROCESSING                                            *               
*===============================================================*               
         SPACE 1                                                                
XK30     DS    0H                                                               
         MVI   TOTIND,C'-'         SET FLAG FOR DELETING RECORDS                
*                                                                               
* READ CONTROL FILE A0 PROFILE TO GET OPTIONS FOR ADVERTISER                    
* FOR EVERY ADV POWER CODE                                                      
*                                                                               
         L     R3,=A(AGCTAB1)                                                   
         USING AGCLDSCT,R3                                                      
*                                                                               
         XC    WORK,WORK                                                        
PROFLOOP MVC   WORK(4),=C'P000'                                                 
         MVC   WORK+4(2),AGCLAPC                                                
         MVI   WORK+6,C'M'                                                      
         MVC   WORK+7(3),AGCLACLT                                               
         L     R4,AGCLPROF                                                      
*                                                                               
         B     XK51                                                             
         GOTO1 GETPROF,DMCB,WORK,(R4),DATAMGR                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*==========================================================*                    
* MUST READ CLIENT HEADERS FOR DDS ADV AGENCIES NOW        *                    
* WHILE STILL ON THE ADV PRTFILE AND SAVE THEM             *                    
*==========================================================*                    
         SPACE 1                                                                
XK51     XC    KEY,KEY                                                          
         MVC   KEY(2),AGCLAPC      POWER CODE                                   
XK51A    L     RF,AGCLMEDP                                                      
         MVC   KEY+2(1),0(RF)      MEDIA                                        
         MVI   KEY+3,2                                                          
         MVC   KEY+4(3),AGCLACLT    CLIENT                                      
*                                                                               
         DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AREC,AGCLADV        SET SAVE AREA ADDRESS                        
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R3,AGCLLEN(R3)                                                   
         CLI   0(R3),255           END OF TABLE                                 
         BNE   PROFLOOP                                                         
         DROP  R3                                                               
*                                                                               
         SPACE 1                                                                
         EJECT                                                                  
*===================================================================            
* AT THIS POINT ALL CLIENT HEADERS HAVE BEEN READ IN FOR EACH OF THE            
*  ADVERTISERS.    THE PROCESS NOW WILL BE TO OPEN THE AGENCIES ONE             
* AT A TIME AND PUT TO THE SORT ALL PRODUCTS, ESTIMATES AND BUYS THAT           
* MEET REQUIREMENTS (TEST BUYS, BUYS FROM A START DATE).  WHEN ALL              
* AGENCIES ARE PROCESSED, SORT THE EXTRACTED RECORDS.  BEGIN READING            
* THE ADVERTISER BACKED UP TAPE AND DROP ALL PRD, EST, AND BUYS  FOR            
* THAT ADVERTISER AND AT THE SAME TIME MERGING THE SORTED RECORDS.              
*                                                                               
*===================================================================            
*                                                                               
         L     RF,=A(AGCTAB1)                                                   
         ST    RF,THISOPEN                                                      
         BAS   RE,OPENPRT           READ ALL THE AGENCIES RECORDS AND           
*                                   PASS TO SORT                                
         BAS   RE,READTAPE                                                      
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)         POINT TO SORT RECORD I/O                     
         BZ    *-2                  MUST HAVE SORTED SOMETHING                  
*** R2 POINTS TO SORT I/O                                                       
         BAS   RE,PROCESS           BEGIN READING TAPE AND DROPPING             
*                                   AND MERGING RECORDS FROM SORT               
         SH    R5,=H'4'                                                         
         MVC   0(8,R5),=X'0800FFFFFFFF' EOF                                     
         PUT   FILEOUT,(R5)                                                     
         GOTO1 =V(SORTER),DMCB,=C'END'    CLOSE SORT                            
         CLOSE FILEIN                                                           
         CLOSE FILEOUT                                                          
*                                                                               
*---> OPEN ADVERTISER FILE  AND BEGIN PRINTING REPORT                           
*                                                                               
         L     R5,=A(AGCTAB1)                                                   
         ST    R5,THISOPEN                                                      
ALLDONE  DS    0H                                                               
         USING AGCLDSCT,R5                                                      
         CLI   AGCLINDI,255        PROCESSED THIS AGENCY                        
         BNE   ALLCONT                                                          
         LA    R5,AGCLLEN(R5)                                                   
         CLI   0(R5),255                                                        
         BE    LASTDONE                                                         
         B     ALLDONE                                                          
***********                                                                     
*---> OPEN                                                                      
***********                                                                     
ALLCONT  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'PRINT'                                
         L     RE,THISOPEN         AGENCY POWER CODE                            
*                                                                               
         XC    CNDATA,CNDATA                                                    
         LA    R4,CNDATA                                                        
         USING CND,R4                                                           
         MVC   CNAGY(2),0(RE)      MOVE ALPHA AGENCY CODE                       
*                                                                               
         GOTO1 VCONFID,DMCB,(R4),(1,FULL)                                       
*                                                                               
         OC    FULL,FULL           TEST ID FOUND                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,UTL                                                           
         MVC   4(1,RE),CNPSE       MOVE PRINT SYSTEM NUMBER                     
         MVC   SVAGYB,CNPCD        AND SAVE PRINT AGENCY NUMBER                 
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'PRINT',GLIST,=A(IOAREA)               
*                                                                               
*      BEGIN PRINTING REPORT                                                    
*                                                                               
         MVC   THISOPEN,=A(AGCTAB1)                                             
         BAS   RE,XKTOTPRT                                                      
         L     R5,THISOPEN                                                      
         MVI   AGCLINDI,255        FINISHED WITH THIS AGENCY                    
         B     ALLDONE THRU ALL TABLES                                          
***                                                                             
LASTDONE DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*===============================================================*               
* DROP PROD, EST AND BUYS FROM BACKUP TAPE.  ADD $ FOR DROPPED BUYS             
*  AND MERGE NEW SORTED BUYS TO OUTPUT TAPE                                     
*===============================================================*               
         SPACE 1                                                                
PROCESS  NTR1                                                                   
*=============================================================*                 
* R2 POINTS TO SORT RECORD                                                      
* R5 POINTS TO TAPE RECORD                                                      
*=============================================================*                 
         SPACE  3                                                               
RDSTART  L     R5,=A(TAPEIO)                                                    
         CLC   0(4,R2),=X'FFFFFFFF'  REACHED END OF SORTED RECS                 
         BE    PROTAPE             PROCESS TAPE                                 
         CLC   0(4,R5),=X'FFFFFFFF'   REACHED END OF TAPE RECS                  
         BE    RDSORT                                                           
         CLC   0(25,R5),0(R2)      TAPE TO SORT KEY                             
         BH    RDSORT              PROCESS SORTED RECORD                        
         BL    PROTAPE             PROCESS TAPE                                 
         SPACE 3                                                                
*********                                                                       
*------> TAPE RECORD IS EQ SORT RECORD -  BYPASS TAPE RECORD                    
*********                                                                       
RDTAPE   CLC   0(4,R5),=X'FFFFFFFF'                                             
         BE    EXIT                BOTH FILES AT END                            
         BAS   RE,READTAPE                                                      
         B     RDSTART                                                          
         SPACE 3                                                                
*********                                                                       
*------> TAPE RECORD IS GT SORT RECORD -  PUT SORTED TO TAPE                    
*********                                                                       
RDSORT   CLC   0(4,R2),=X'FFFFFFFF'  MAY COME FROM RDSTART                      
         BE    EXIT                                                             
         LR    R4,R2                                                            
         BAS   RE,PUTTAPE                                                       
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BNZ   RDSTART              MUST HAVE SORTED SOMETHING                  
         LA    R2,=X'FFFFFFFF'                                                  
         B     RDSTART                                                          
         SPACE 3                                                                
*********                                                                       
*------> TAPE RECORD IS LT SORT RECORD -  PUT TAPE R. TO TAPE                   
*********                                                                       
PROTAPE  CLC   0(4,R5),=X'FFFFFFFF'  MAY COME FROM RDSTART                      
         BE    EXIT                                                             
         L     RF,=A(AGCTAB1)                                                   
         USING AGCLDSCT,RF                                                      
RDCKAG   CLI   0(RF),255                                                        
         BE    RDPUTIT                                                          
         CLC   0(2,R5),AGCLAPC     SAME ADVERTISER PC                           
         BNE   RDBMP                                                            
         CLC   4(3,R5),AGCLACLT     SAME CLIENT                                 
         BNE   RDBMP                                                            
         CLI   3(R5),X'06'         PRODUCT CODE                                 
         BE    RDTAPE              BYPASS                                       
         CLI   3(R5),X'07'         EST CODE                                     
         BE    RDTAPE              BYPASS                                       
         CLI   3(R5),X'20'         BUY                                          
         BE    DELCOUNT            ADD $ TO DELETED TABLE                       
         B     RDPUTIT                                                          
RDBMP    LA    RF,AGCLLEN(RF)                                                   
         B     RDCKAG                                                           
*                                                                               
RDPUTIT  LR    R4,R5                                                            
         BAS   RE,PUTTAPE                                                       
         B     RDSTART                                                          
*                                                                               
DELCOUNT MVI   TOTIND,C'-'                                                      
         ST    R5,DUB                                                           
         BAS   RE,XKTOT            ADD $ TO TABLE                               
         B     RDTAPE                                                           
         EJECT                                                                  
*                                                                               
*  READ TAPE (THE BACKED UP BUYFILE FOR THE ADVERTISER)                         
*                                                                               
READTAPE NTR1                                                                   
XK34     L     R0,=A(TAPEIO)                                                    
         SH    R0,=H'4'                                                         
         GET   FILEIN,(R0)                                                      
         AP    INCNT,=P'1'                                                      
*                                                                               
XK34A    DS    0H                                                               
         L     RE,=A(TAPEIO)                                                    
         SH    RE,=H'4'            POINT TO RECLEN                              
         AH    RE,0(RE)            POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       CLEAR END OF RECORD                          
         B     EXIT                                                             
*                                                                               
****                                                                            
XK50     DS    0H                  END OF INPUT TAPE                            
         L     RE,=A(TAPEIO)                                                    
         SH    RE,=H'4'            POINT TO RECLEN                              
         MVC   0(8,RE),=X'0800FFFFFFFF' EOF                                     
         B     EXIT                                                             
         SPACE 1                                                                
THISAGCY DS    CL2                                                              
         EJECT                                                                  
OPENPRT  NTR1  0                   CLOSE PREVIOUS PRINT SYSTEM                  
OPENPRTA DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'PRINT'                                
         L     RE,THISOPEN         AGENCY POWER CODE                            
*                                                                               
         XC    CNDATA,CNDATA                                                    
         LA    R4,CNDATA                                                        
         USING CND,R4                                                           
         MVC   CNAGY(2),0(RE)      MOVE ALPHA AGENCY CODE                       
*                                                                               
         GOTO1 VCONFID,DMCB,(R4),(1,FULL)                                       
*                                                                               
         OC    FULL,FULL           TEST ID FOUND                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,UTL                                                           
         MVC   4(1,RE),CNPSE       MOVE PRINT SYSTEM NUMBER                     
         MVC   SVAGYB,CNPCD        AND SAVE PRINT AGENCY NUMBER                 
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'PRINT',FLIST,=A(IOAREA)               
*                                                                               
         XC    KEY,KEY                                                          
         L     RE,THISOPEN                                                      
         USING AGCLDSCT,RE                                                      
         MVC   KEY(2),0(RE)        SET AGENCY IN KEY (LEFT ALIGN)               
         L     RF,AGCLMEDP         MEDIA POINTER                                
         ST    RF,THISMEDA                                                      
         DROP  RE                                                               
         MVC   KEY+2(1),0(RF)                                                   
         MVI   KEY+3,2            RECORD                                        
         MVC   KEY+4(3),2(RE)    SET CLIENT                                     
         GOTO1 HIGH                READ AGENCY'S CLTHDR                         
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R8,=A(SVCLTHDR)                                                  
         ST    R8,AREC                                                          
         GOTO1 GETPRT                                                           
*==================================================================             
* READ ALL PRODUCTS, ESTIMATES AND BUYS FOR THIS AGENCY AND PASS TO             
*  THE SORT.                                                                    
* READ ALL CLIENT PRODUCTS     *                                                
*==================================================================             
         SPACE 1                                                                
*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%%*%*%*%*%*%*%*%*%*                  
*%*%*%*  BAS   RE,BLDPUB INCUDE WHEL PUB LINK IS READY                          
*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%*%%*%*%*%*%*%*%*%*%*                  
         BAS   RE,RDPRDHI       READ PRODUCTS                                   
         B     *+8                                                              
PRODPRSQ BAS   RE,RDPRDSQ                                                       
*                                                                               
         BNE   PROCESTS                                                         
         BAS   RE,CONVERT                                                       
         BAS   RE,PUTSORT                                                       
         B     PRODPRSQ            PRODUCT PROCESSING                           
*                                                                               
*  PASS ALL ESTIMATES                                                           
*                                                                               
PROCESTS BAS   RE,RDESTHI                                                       
         B     *+8                                                              
PROCESQ  BAS   RE,RDESTSQ                                                       
*                                                                               
         BNE   PROCBUYS                                                         
         BAS   RE,CONVERT                                                       
         BAS   RE,PUTSORT                                                       
         B     PROCESQ             ESTIMATE PROCESSING                          
*                                                                               
*                                                                               
PROCBUYS BAS   RE,RDBUYHI                                                       
         B     *+8                 READ BUYS                                    
PROCBSQ  BAS   RE,RDBUYSQ                                                       
*                                                                               
         BNE   NEXTAGCY            PROCESS NEXT ITEM IN TABLE                   
         BAS   RE,CONVERT                                                       
         BAS   RE,PUTSORT                                                       
         B     PROCBSQ             BUY PROCESSING                               
*                                                                               
NEXTAGCY L     RF,THISOPEN                                                      
         LA    RF,AGCLLEN(RF)                                                   
         CLI   0(RF),255                                                        
         BE    EXIT                                                             
         ST    RF,THISOPEN                                                      
         B     OPENPRTA                                                         
         EJECT                                                                  
********************************************8                                   
         EJECT                                                                  
* DS    0D                                                                      
AGCTAB1  DS    0D                                                               
*                                                                               
*=  DSECT  =(AGCLDSCT)                                                          
*  .!!!!!....                                                                   
*  AGCLAGY-------> AGENCY POWER CODE                                            
*  AGCLCLT------ ! ---> AGENCY CLIENT CODE                                      
*  AGCLAPC------ ! -- ! -------> ADVERTISER POWER CODE                          
*  AGCLACLT----- ! -- ! ------ ! ----> ADVERTISER CLIENT CODE                   
*  AGCLCLTH----- ! -- ! ------ ! --- ! ----> ADDR OF CLI  HEADER                
*  AGCLINDI----- ! -- ! ------ ! --- ! --- ! -------> STATUS BYTES              
*  AGCLMEDP----- ! -- ! ------ ! --- ! --- ! ------ ! ----->  MEDIA             
*                !    !        !     !     !        !      !  TABLE             
*                !    !        !     !     !        !      !                    
*                V    V        V     V     V        V      V                    
         DC    C'JW',C'EAK',C'SJ',C'JD ',A(CLI1),XL4'0',A(JWMD),A(PROF1C        
               ),A(ATAB1)                                                       
*        DC    C'JW',C'EAK',C'XX',C'JEA',A(CLI1),XL4'0',A(JWMD),A(PROF1         
*              !     !-----AGCLATL ADD OF DOLLAR TABLE FOR THIS CLI             
*              !----- AGCLPROF ADDRESS OF THIS PROFILE                          
         DC    C'YR',C'EK ',C'SJ',C'GP ',A(CLI2),XL4'0',A(YRMD),A(PROF2C        
               ),A(ATAB2)                                                       
         DC    X'FFFFFF'                                                        
*        DC    C'YR',C'EK ',C'XX',C'YEA',A(CLI2),XL4'0',A(YRMD),A(PROF2         
*              !        !-----AGCLATL ADD OF DOLLAR TABLE FOR THIS CLI          
*              !----- AGCLPROF ADDRESS OF THIS PROFILE                          
*                                                                               
         DC    C'AY',C'SDG',C'YY',C'ASD',A(CLI2),XL4'0',A(YRMD),A(PROF4C        
               ),A(ATAB3)                                                       
*              !        !-----AGCLATL ADD OF DOLLAR TABLE FOR THIS CLI          
*              !----- AGCLPROF ADDRESS OF THIS PROFILE                          
*                                                                               
         DC    C'JW',C'SD ',C'YY',C'JSD',A(CLI1),XL4'0',A(JWMD),A(PROF3C        
               ),A(ATAB4)                                                       
*              !        !-----AGCLATL ADD OF DOLLAR TABLE FOR THIS CLI          
*              !----- AGCLPROF ADDRESS OF THIS PROFILE                          
*                                                                               
         DC    18X'FF'                                                          
         SPACE 3                                                                
JWMD     DC    CL5'MNOST',X'FF'    MEDIA FOR J WALTER                           
YRMD     DC    CL5'MNOST',X'FF'              AYER                               
         ORG                                                                    
*                                                                               
FLIST    DC    CL8' PRTFILE'                                                    
         DC    CL8' PRTDIR '                                                    
         DC    CL8' PUBFILE'                                                    
         DC    CL8' PUBDIR '                                                    
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
GLIST    DC    CL8' CTFIILE'                                                    
         DC    CL8'X       '                                                    
         EJECT                                                                  
*=====================================================================*         
* FIND NEXT ADVERTISER CLIENT CODE                                    *         
*=====================================================================*         
FINDCLI  NTR1                                                                   
         L     R7,=A(AGCTAB1)                                                   
         USING AGCLDSCT,R7                                                      
FINDCLIA TM    AGCLINDI+1,AGPCLIP  PROCESSED THIS CLIENT                        
         BNO   FINDC               READ CLIENT HEADER                           
         LA    R7,AGCLLEN(R7)      TO NEXT ENTRY                                
         CLI   0(R7),255           END OF TABLE                                 
         BE    EXIT                                                             
         B     FINDCLIA                                                         
*                                                                               
FINDC    L     RF,AGCLMEDP         MEDIA                                        
         MVC   KEY+2(1),0(RF)                                                   
         DROP  R7                                                               
         EJECT                                                                  
*============================================================*                  
* PUT RECORD TO SORT                                         *                  
*============================================================*                  
PUTSORT  NTR1                                                                   
         SR    R0,R0                                                            
         L     R4,=A(IOAREA)                                                    
         ICM   R0,3,25(R4)         GET REC LENGTH                               
         AH    R0,=H'4'                                                         
         SH    R4,=H'4'                                                         
         SLL   R0,16               LEFT ALIGN                                   
         ST    R0,0(R4)            SET REC LENGTH FOR SORT                      
         AP    AGYCNT,=P'1'                                                     
         PUT   =V(SORTER),DMCB,=C'PUT',(R4)                                     
         CLI   QOPT1,C'Y'          SEE IF TEST RUN                              
         BNE   EQXIT                                                            
         CP    AGYCNT,=P'1000'                                                  
         BH    EQXIT                                                            
         GOTO1 =V(PRNTBL),DMCB,(6,=C'SORT'),(R4),100,=C'1D'                     
* PRINT DURING TEST                                                             
         B     EQXIT                                                            
         SPACE 1                                                                
         EJECT                                                                  
*============================================================*                  
* PUT RECORD TO TAPE ** R4 MUST POINT TO I/O AREA            *                  
*============================================================*                  
PUTTAPE  NTR1                                                                   
         SR    R0,R0                                                            
         ICM   R0,3,25(R4)         GET REC LENGTH                               
         AH    R0,=H'4'                                                         
         SH    R4,=H'4'                                                         
         SLL   R0,16               LEFT ALIGN                                   
         ST    R0,0(R4)            SET REC LENGTH FOR SORT                      
         AP    AGYCNT,=P'1'                                                     
         PUT   FILEOUT,(R4)                                                     
         CLI   QOPT1,C'Y'          SEE IF TEST RUN                              
         BNE   EQXIT                                                            
         CP    AGYCNT,=P'1000'                                                  
         BH    EQXIT                                                            
         GOTO1 =V(PRNTBL),DMCB,(6,=C'TAPE'),(R4),100,=C'1D'                     
* PRINT DURING TEST                                                             
         B     EQXIT                                                            
         SPACE 1                                                                
         EJECT                                                                  
*============================================================*                  
* FIND AGENCY LINK TO ADVERTISER PUBLICATION                 *                  
*============================================================*                  
BLDPUB   NTR1                                                                   
         L     R2,THISOPEN                                                      
         USING AGCLDSCT,R2                                                      
         L     R0,=A(AGPUBTAB)                                                  
         L     R1,=A(AGPUBTAX)                                                  
         BAS   RE,CLEAR                                                         
         L     R4,=A(AGPUBTAB)                                                  
         L     R3,=A(AGPUBTAX)                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,X'FE'                                                        
         MVI   KEY+1,C'M'    MEDIA                                              
         MVC   KEY+2(2),AGCLAGY AGENCY                                          
         MVC   KEY+4(2),AGCLAPC ADVERTISER AGENCY CODE                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PUBDIR',KEY,KEY                       
         B     PUBCKKEY                                                         
*                                                                               
PUBRSEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'PUBDIR',KEY,KEY                       
*                                                                               
PUBCKKEY CLC   KEY(6),KEYSAVE                                                   
         BE    PUBTOTAB            LOAD PUB TO TABLE                            
         CLC   KEY(1),KEYSAVE      RECORD TYPE CHANGE                           
         BNE   EXITNOW                                                          
         CLC   KEY+2(4),KEYSAVE+2  SAME AGENCY AND ADVERTISER                   
         BNE   EXITNOW             FINISHED                                     
*                                                                               
*    MEDIA MUST HAVE CHANGED                                                    
*                                                                               
PUBTOTAB MVC   0(1,R4),KEY+1        MEDIA                                       
         MVC   1(12,R4),KEY+6       MOVE AGYPUB AND LINK TO ADV                 
         LA    R4,13(R4)                                                        
         CR    R4,R3                REACHED END OF TABLE                        
         BNH   PUBRSEQ                                                          
         DC    H'0'                =======>PUB TABLE EXCEEDED                   
EXITNOW  XIT                                                                    
*                                                                               
         SPACE  3                                                               
         DROP  R2                                                               
*============================================================*                  
* BUILD TABLE OF ALL ADVERTISER PRODUCTS                     *                  
*============================================================*                  
         SPACE 1                                                                
XKBLDPRD NTR1                                                                   
*                                                                               
*                                                                               
         L     R1,=A(PRDTAB)                                                    
         L     R0,=A(PRDTABX)                                                   
         BAS   RE,CLEAR                                                         
*                                                                               
         L     R4,=A(PRDTAB)                                                    
         L     R7,=A(AGCTAB1)                                                   
         USING AGCLDSCT,R7                                                      
         L     R2,AGCLMEDP          MEDIA POINTER                               
*                                                                               
XKBLDPA  XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),0(R2)      MEDIA                                        
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),AGCLAPC     FIRST ADVERTISER'S CLIENT CODE              
         MVC   KEY+7(3),=C'AAB'                                                 
         MVI   AGCLINDI+1,AGPRODP  PROCESSED PRODUCTS FOR THIS ENTRY            
         GOTO1 HIGH                SKIP CLTHDR                                  
         CLC   KEY(6),KEYSAVE      SAME AGY-RECTYPE-CLIENT                      
         BE    BLDPRD4A                                                         
         DC    X'0',C'MISS PRD '   LEN OF 10                                    
*                                                                               
BLDPRD2  GOTO1 SEQ                                                              
*                                                                               
BLDPRD4  CLC   KEY(6),KEYSAVE      SAME AGY-RECTYPE-CLIENT                      
         BNE   TONEXTCL                                                         
BLDPRD4A MVC   0(3,R4),KEY         SAVE AG/MED                                  
         MVC   3(6,R4),KEY+4            CLIENT/PRODUCT                          
         LA    R4,9(R4)            TO NEXT ENTRY                                
         CLI   0(R4),255                                                        
         BNE   BLDPRD2                                                          
         DC    X'0',C'PRD TBL '       PRODUCT TABLE TOO SMALL                   
*                                                                               
TONEXTCL CLC   KEY(2),KEYSAVE        SAME AGENCY                                
         BNE   EXIT                                                             
         LA    R2,1(R2)              BUMP TO NEXT MEDIA                         
         CLI   0(R2),255             REACHED END                                
         BNE   XKBLDPA               DO NEXT MEDIA                              
*                                                                               
         LA    R7,AGCLLEN(R7)                                                   
         CLI   0(R7),255             FINISHED WITH CLIENT TABLE                 
         BE    EXIT                                                             
         L     R2,AGCLMEDP           NEXT SET OF MEDIAE                         
         B     XKBLDPA             PROCESS NEXT CLIENT ENTRY                    
         EJECT                                                                  
*================================================================*              
* READ ALL ESTIMATES                                             *              
*================================================================*              
RDESTSQ  NTR1                                                                   
         GOTO1 SEQ                                                              
         B     ESTCOMM                                                          
RDESTHI  NTR1                                                                   
         XC    KEY,KEY                                                          
         L     RE,THISOPEN                                                      
         MVC   KEY(2),0(RE)        SET AGENCY IN KEY (LEFT ALIGN)               
         L     RF,AGCLMEDP         MEDIA POINTER                                
         ST    RF,THISMEDA                                                      
ESTRDHI  MVC   KEY+2(1),0(RF)                                                   
         MVI   KEY+3,7            RECORD                                        
         MVC   KEY+4(3),AGCLCLT  SET CLIENT                                     
         GOTO1 HIGH                READ AGENCY'S CLTHDR                         
ESTCOMM  CLC   KEY(7),KEYSAVE                                                   
         BE    ESTREAD                                                          
         CLC   KEY(2),KEYSAVE      HAS AGENCY CHANGED                           
         BNE   NEQXIT              PROCESS ESTIMATES                            
         CLC   KEY+2(1),KEYSAVE+2  MEDIA CHANGED                                
         BE    ESTMEDEQ                                                         
ESTMEDCH L     RF,THISMEDA                                                      
         LA    RF,1(RF)                                                         
         CLI   0(RF),255           END OF MEDIA TABLE                           
         BE    NEQXIT              RESET MEDIA TABLE                            
         ST    RF,THISMEDA                                                      
         L     RE,THISOPEN                                                      
         B     ESTRDHI                                                          
*                                                                               
*  AGENCY / MEDIA SAME  M/B RECORD TYPE CHANGED                                 
*                                                                               
ESTMEDEQ CLC   KEY+3(1),KEYSAVE+3                                               
         BNE   NEQXIT                                                           
* MUST BE A CLIENT CHANGE WITHIN THE SAME AG/M ID-- BUMP TO NEXT MEDIA          
         B     ESTMEDCH                                                         
*                                                                               
ESTREAD  MVC   AREC,=A(IOAREA)                                                  
         GOTO1 GETPRT                                                           
         B     EQXIT                                                            
*                                                                               
         EJECT                                                                  
*================================================================*              
* READ ALL BUYS                                                 *               
*================================================================*              
RDBUYSQ  NTR1                                                                   
RDBSEQ   GOTO1 SEQ                                                              
         B     COMMBUY                                                          
*                                                                               
RDBUYHI  NTR1                                                                   
         XC    KEY,KEY                                                          
         L     RE,THISOPEN                                                      
         USING AGCLDSCT,RE                                                      
         MVC   KEY(2),0(RE)        SET AGENCY IN KEY (LEFT ALIGN)               
         L     RF,AGCLMEDP         MEDIA POINTER                                
         ST    RF,THISMEDA                                                      
BUYRDHI  MVC   KEY+2(1),0(RF)                                                   
         MVI   KEY+3,X'20'        RECORD                                        
         MVC   KEY+4(3),AGCLCLT  SET CLIENT                                     
         GOTO1 HIGH                READ FIRST BUY                               
COMMBUY  CLC   KEY(7),KEYSAVE                                                   
         BE    BUYREAD                                                          
         CLC   KEY(2),KEYSAVE      HAS AGENCY CHANGED                           
         BNE   NEQXIT              PROCESS ESTIMATES                            
         CLC   KEY+2(1),KEYSAVE+2  MEDIA CHANGED                                
         BE    BUYMEDEQ                                                         
BUYMEDCH L     RF,THISMEDA                                                      
         LA    RF,1(RF)                                                         
         CLI   0(RF),255           END OF MEDIA TABLE                           
         BE    NEQXIT              RESET MEDIA TABLE                            
         ST    RF,THISMEDA                                                      
         L     RE,THISOPEN                                                      
         B     BUYRDHI                                                          
*                                                                               
*  AGENCY / MEDIA SAME  M/B RECORD TYPE CHANGED                                 
*                                                                               
BUYMEDEQ CLC   KEY+3(1),KEYSAVE+3                                               
         BNE   NEQXIT                                                           
* MUST BE A CLIENT CHANGE WITHIN THE SAME AG/M ID-- BUMP TO NEXT MEDIA          
         B     BUYMEDCH                                                         
*                                                                               
BUYREAD  DS    0H                                                               
         L     RE,THISOPEN                                                      
         L     RF,AGCLPROF                                                      
         OC    0(3,RF),0(RF)       ANY DATE FILTER                              
         BZ    RDBUY                                                            
         CLC   0(3,RF),KEY+16   FROM THIS DATE ON                               
         BL    RDBSEQ                                                           
RDBUY    MVC   AREC,=A(IOAREA)                                                  
         GOTO1 GETPRT                                                           
* INCLUDE TEST BUYS                                                             
         L     RE,THISOPEN                                                      
         L     RF,AGCLPROF                                                      
         CLI   3(RF),C'T'          TEST BUYS                                    
         BNE   DOLLARS                                                          
         L     R3,=A(IOAREA)                                                    
         USING BUYRECD,R3                                                       
         CLI   PBDBFD,C'T'         TEST BUY                                     
         BE    RDBSEQ                                                           
*                                                                               
         DROP  RE                                                               
*                                                                               
DOLLARS  L     R4,AGCLATL          POINT TO TOTALS (BY MEDIA)                   
         GOTO1 =V(GETINS),DMCB,PBUYKEY,GROSS,PBUYKPRD                           
         TM    PBUYKEY+27,X'80'                                                 
         BNO   DOLL1                                                            
         XC    GROSS(16*4),GROSS                                                
*                                                                               
DOLL1    CLC   0(1,R4),PBUYKMED                                                 
         BE    DOLLADD                                                          
         LA    R4,ATABLEN(R4)                                                   
         CLI   0(R4),255                                                        
         BNE   DOLL1                                                            
         DC    H'0'                                                             
*                                                                               
DOLLADD  L     R5,GROSS                                                         
         CVD   R5,DUB                                                           
         AP    0(8,R4),DUB        GROSS ORDERED                                 
         L     R5,PGROSS                                                        
         CVD   R5,DUB                                                           
         AP    8(8,R4),DUB        GROSS PAID                                    
         L     R5,PAID                                                          
         CVD   R5,DUB                                                           
         AP    16(8,R4),DUB        NET PAID                                     
         B     EQXIT                                                            
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*================================================================*              
* CONVERT THIS RECORD                                            *              
*================================================================*              
CONVERT  NTR1                                                                   
         L     RE,THISOPEN                                                      
         USING AGCLDSCT,RE                                                      
         L     RF,=A(IOAREA)                                                    
         USING BUYRECD,RF                                                       
         MVC   PBUYKAGY,AGCLAPC     ADVERTISER'S POWER CODE                     
         MVC   PBUYKCLT,AGCLAPC     ADV CLIENT CODE                             
         CLI   PBUYKRCD,X'20'                                                   
         BNE   EQXIT               MB PROD OR EST                               
         BAS   RE,PUBLOOK          FIND CONVERTED PUB                           
         B     EQXIT                                                            
         EJECT                                                                  
*================================================================*              
* FIND CROSS REFERENCE PUB                                       *              
*================================================================*              
PUBLOOK  NTR1                                                                   
         B     EXIT *+*+*+*+*+*+*+ REMOVE WHEN PUB LINKS ARE DONE               
         MVC   WORK,PBUYKMED                                                    
         MVC   WORK+1(6),PBUYKPUB                                               
         LA    R1,=A(AGPUBTAB)                                                  
PUBCK    CLC   WORK(7),0(R1)                                                    
         BE    PUBFND                                                           
         LA    R1,13(R1)                                                        
         CLI   0(R1),0                                                          
         BNE   PUBCK                                                            
         DC    H'0'                                                             
PUBFND   MVC   PBUYKPUB(6),7(R1)                                                
         B     EQXIT                                                            
         DROP  RE,RF                                                            
         EJECT                                                                  
*================================================================*              
* READ ALL PRODUCTS FOR THIS AGENCY/CLIENT                       *              
*================================================================*              
RDPRDSQ  NTR1                                                                   
         GOTO1 SEQ                                                              
         B     PRDCOMM                                                          
RDPRDHI  NTR1                                                                   
         XC    KEY+7(18),KEY+7     SAVE AG/M/ID/CLI                             
PRDRDHI  MVI   KEY+3,X'06'         PRODUCT                                      
         GOTO1 HIGH                                                             
PRDCOMM  CLC   KEY(7),KEYSAVE                                                   
         BE    PRDREAD                                                          
         CLC   KEY(2),KEYSAVE      HAS AGENCY CHANGED                           
         BNE   NEQXIT              PROCESS ESTIMATES                            
         CLC   KEY+2(1),KEYSAVE+2  MEDIA CHANGED                                
         BE    PRDMEDEQ                                                         
PRDMEDCH L     RF,THISMEDA                                                      
         LA    RF,1(RF)                                                         
         CLI   0(RF),255           END OF MEDIA TABLE                           
         BE    NEQXIT              RESET MEDIA TABLE                            
         MVC   KEY+2(1),0(RF)                                                   
         ST    RF,THISMEDA                                                      
         L     RE,THISOPEN                                                      
         MVC   KEY+4(3),AGCLCLT    AGENCY'S CLIENT                              
         XC    KEY+7(17),KEY+7                                                  
         B     PRDRDHI                                                          
*                                                                               
*  AGENCY / MEDIA SAME  M/B RECORD TYPE CHANGED                                 
*                                                                               
PRDMEDEQ CLC   KEY+3(1),KEYSAVE+3                                               
         BNE   NEQXIT                                                           
* MUST BE A CLIENT CHANGE WITHIN THE SAME AG/M ID-- BUMP TO NEXT MEDIA          
         B     PRDMEDCH                                                         
*                                                                               
PRDREAD  MVC   AREC,=A(IOAREA)                                                  
         GOTO1 GETPRT                                                           
         B     EQXIT                                                            
*                                                                               
THISMEDA DS    F                                                                
         EJECT                                                                  
*================================================================*              
* PRINT TOTALS FOR AN AGENCY                                     *              
*================================================================*              
         SPACE 1                                                                
XKAGYTOT NTR1                                                                   
         CLI   XFRAGY,0            TEST FIRST TIME                              
         BE    AGYTOTX                                                          
*                                                                               
         MVC   P(3),SVAGYA         AGENCY ALPHA                                 
         MVI   P+3,C'='                                                         
         ZIC   R0,XFRAGY                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(2),DUB                                                       
         OI    AGYCNT+3,X'0F'                                                   
         UNPK  P+10(7),AGYCNT                                                   
         MVC   P+18(7),=C'RECORDS'                                              
         GOTO1 REPORT                                                           
*                                                                               
AGYTOTX  DS    0H                                                               
         ZAP   AGYCNT,=P'0'        RESET AGENCY RECORD COUNTER                  
         B     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
* SUBROUTINE TO CLEAR FROM R1 TO R0. RF IS DESTROYED.               *           
*===================================================================*           
         SPACE 1                                                                
CLEAR    NTR1                                                                   
         LA    RF,256                                                           
*                                                                               
CLEAR2   SR    R0,R1               GIVES LENGTH TO CLEAR                        
         CR    R0,RF                                                            
         BNH   CLEAR4                                                           
         XC    0(256,R1),0(R1)                                                  
         AR    R1,RF                                                            
         SR    R0,RF                                                            
         BZ    EQXIT                                                            
         B     CLEAR2                                                           
CLEAR4   LR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,CLEARXC                                                       
         B     EQXIT                                                            
CLEARXC  XC    0(0,R1),0(R1)  ** EXECUTED **                                    
         EJECT                                                                  
*=============================================================*                 
* ACCUMULATE ORDERED/PAID TOTALS FOR BUY RECORDS              *                 
* DUB POINTS TO BUY RECORD                                     *                
* TOTIND = C'-' TO POST TO REMOVED                            *                 
* TOTIND = C'+' TO POST TO ADDED                              *                 
*=============================================================*                 
         SPACE 1                                                                
XKTOT    NTR1                                                                   
*                                                                               
         L     R2,DUB                                                           
         USING BUYRECD,R2                                                       
*                                                                               
         L     R5,THISOPEN                                                      
         USING AGCLDSCT,R5                                                      
*                                                                               
         CLI   0(R2),X'FF'         TEST EOF                                     
         BE    EXIT                                                             
*                                                                               
         TM    27(R2),X'80'        TEST DELETED                                 
         BNZ   EXIT                                                             
*                                                                               
         L     R3,AGCLATL          TOTALS FOR THIS AGENCY                       
         LA    RF,5                FIVE MEDIA                                   
WHICHM   CLC   0(1,R3),PBUYKMED    GET CORRECT MEDIA                            
         BE    THISMED                                                          
*                                                                               
         LA    R3,ATABLEN(R3)                                                   
         BCT   RF,WHICHM                                                        
         DC    H'0'                                                             
*                                                                               
THISMED  LA    R3,1(R3)            BUMP PAST MEDIA                              
         CLI   TOTIND,C'-'         DELETES                                      
         BNE   *+8                                                              
         LA    R3,32(R3)          POINT TO MINUS TOTS                           
*                                                                               
         BAS  RE,XKTOT2                                                         
         B     EXIT                                                             
*                                                                               
XKTOT2   DS    0H                                                               
*                                                                               
         GOTO1 =V(GETINS),DMCB,PBUYKEY,GROSS,PBUYKPRD                           
*                                                                               
         L     R4,GROSS                                                         
         L     R5,PGROSS                                                        
         L     R6,PAID                                                          
*                                                                               
         CVD   R4,DUB                                                           
         AP    0(8,R3),DUB        GROSS ORDERED                                 
         CVD   R5,DUB                                                           
         AP    8(8,R3),DUB      GROSS PAID                                      
         CVD   R6,DUB                                                           
         AP    16(8,R3),DUB     NET PAID                                        
         AP    24(8,R3),=P'1'      DELETED COUNT                                
         B     EXIT                                                             
         DROP R5                                                                
         EJECT                                                                  
*=================================================================*             
* PRINT DOLLAR TOTALS FROM BUFFER                                 *             
*=================================================================*             
         SPACE 1                                                                
         DROP  R7                                                               
XKTOTPRT NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         L     R7,THISOPEN                                                      
         USING AGCLDSCT,R7                                                      
XPRNTAA  CLI   AGCLINDI,255        HAS THIS BEEN PRINTED ALREADY                
         BNE   XPRNTA                                                           
         LA    R7,AGCLLEN(R7)                                                   
         CLI   0(R5),255                                                        
         BE    EXIT                                                             
         B     XPRNTAA                                                          
XPRNTA   MVC   THISAGY,AGCLAPC     ADVERTISER POWER CODE                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         L     R3,AGCLATL          ADDRESS OF TOTALS                            
*                                                                               
         MVC   P(3),AGCLACLT                                                    
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,P+4                                                           
         MVC   0(1,R2),0(R3)                                                    
         LA    R3,1(R3)                                                         
         ZAP   CLTTOT,=P'0'        MEDIA                                        
         MVC   CLTTOT+8(88),CLTTOT  CLEAR ACCUMULATORS                          
NEXTMED  AP    00(08,R4),00(08,R3)                                              
         AP    08(08,R4),08(08,R3)                                              
         AP    16(08,R4),16(08,R3)                                              
         AP    24(08,R4),24(08,R3)                                              
         AP    32(08,R4),32(08,R3)                                              
         AP    40(08,R4),40(08,R3)                                              
         AP    48(08,R4),48(08,R3)                                              
         AP    56(08,R4),56(08,R3)                                              
         AP    00(08,R5),00(08,R3)           NET GROSS ORDERED                  
         AP    08(08,R5),08(08,R3)           NET PAID ORD                       
         AP    16(08,R5),16(08,R3)           NET PAID NET                       
         AP    24(08,R5),24(08,R3)           NET COUNT                          
         SP    00(08,R5),32(08,R3)           NET GROSS ORDERED                  
         SP    08(08,R5),40(08,R3)           NET PAID ORD                       
         SP    16(08,R5),48(08,R3)           NET PAID NET                       
         SP    24(08,R5),56(08,R3)           NET COUNT                          
XKTP4    DS    0H                                                               
         LA    R6,8                                                             
         LR    RE,R3                                                            
         BCTR  R3,0                POINT TO BEGINING                            
*                                                                               
XKTP6    DS    0H                                                               
         MVC   3(14,R2),=X'402020206B2020206B2020202020'                        
         ED    3(14,R2),2(RE)                                                   
         XC    15(2,R2),15(R2)     CLEAR THE PENNIES                            
         LA    RE,8(RE)                                                         
         LA    R2,14(R2)                                                        
         CH    R6,=H'5'                                                         
         BNE   XXXT                                                             
         MVI   0(R2),C'-'                                                       
         LA    R2,2(R2)                                                         
XXXT     BCT   R6,XKTP6                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLI   R3,C'T'             PROCESSED LAST MEDIA FOR CLIENT              
         BE    PRINTNET                                                         
         LA    R3,ATABLEN(R3)                                                   
         LA    R2,P+4                                                           
         MVC   0(1,R2),0(R3)                                                    
         LA    R2,2(R2)                                                         
*        LA    R3,1(R3)                                                         
         B     NEXTMED                                                          
         SPACE 1                                                                
* PRINT THE - TOTALS *                                                          
         SPACE 1                                                                
XKTP8    MVC   P+1(5),=C'TOTAL'                                                 
         LA    R2,P+6                                                           
         LA    R4,3                                                             
         LA    R5,AGYTOTS                                                       
         LA    R6,FILTOTS                                                       
*                                                                               
XKTP10   SP    0(8,R6),0(8,R5)     SUBTRACT FROM ACCUMS                         
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XKTP10                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R1,AGYTOTS                                                       
         BAS   RE,XKCLR                                                         
         SPACE 2                                                                
* PRINT THE TOTALS OF INSERTED RECORDS *                                        
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         L     R5,NEXTBUF          RESTORE BUFFER POINTER                       
*                                                                               
         MVC   P(21),=C'** INSERTED RECORDS **'                                 
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),0             TEST END OF BUFFER                           
         BE    XKTP20                                                           
*                                                                               
XKTP12   DS    0H                                                               
         GOTO1 CLUNPK,DMCB,1(R5),P+27                                           
*                                                                               
XKTP14   LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,4(R5)                                                         
         LA    R6,AGYTOTS                                                       
*                                                                               
XKTP16   AP    0(8,R6),0(8,R5)     BUMP AGY TOTALS                              
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XKTP16                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLI   2(R5),0             ALLOW FOR THIS AGENCY                        
         BNE   XKTP12                                                           
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         MVC   P+27(5),=C'TOTAL'                                                
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,AGYTOTS                                                       
         LA    R6,FILTOTS                                                       
*                                                                               
XKTP18   AP    0(8,R6),0(8,R5)     BUMP FILE TOTALS                             
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XKTP18                                                        
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         LA    R1,AGYTOTS                                                       
         BAS   RE,XKCLR                                                         
         SPACE 1                                                                
XKTP20   DS    0H                                                               
         MVC   P(22),=C'* GRAND TOTALS (NET) *'                                 
*                                                                               
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,FILTOTS                                                       
*                                                                               
XKTP22   MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR PENNIES                                
         MVI   15(R3),C'+'         SET SIGN                                     
         CP    0(8,R5),=P'0'                                                    
         BNM   *+8                                                              
         MVI   15(R3),C'-'                                                      
         LA    R5,8(R5)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XKTP22                                                        
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
XKCLR    LA    R0,3                                                             
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
THISOPEN DS    F                                                                
NEXTBUF  DC    A(TOTBUFF)                                                       
APRDTAB  DS    A                                                                
TOTIND   DC    X'00'                                                            
OLDCLT   DC    XL3'00'                                                          
OLDAGY   DC    XL1'00'                                                          
OLDMED   DC    XL1'00'                                                          
CTOTS    DS    0CL24                                                            
         DC    3PL8'0'             GROSS ORD/GROSS PAID/NET PAID                
AGYTOTS  DC    3PL8'0'                                                          
FILTOTS  DC    3PL8'0'                                                          
POLESDTS DS    F                                                                
SVCCAGY  DC    A(0)                                                             
BUYSTART DS    XL2                                                              
BUYEND   DS    XL2                                                              
LASTMKT  DS    XL2                                                              
LASTAFF  DS    XL1                                                              
SVAGYA   DS    CL3                                                              
SVAGYCLT DS    XL2                                                              
SVAGYB   DS    XL1                                                              
XFRAGY   DC    X'00'                                                            
SVCMML   DS    CL10                                                             
CNDATA   DS    XL14                                                             
MKTCD    DS    CL3                 NSI MARKET CODE                              
SVBUYERR DS    CL13                                                             
SVCMLERR DS    CL8                                                              
NOSTAEQU DS    XL1                                                              
ELCODE   DS    XL1                                                              
PRDERRSW DC    X'00'                                                            
*                                                                               
PROF1    DS    CL16                                                             
PROF2    DS    CL16                                                             
PROF3    DS    CL16                                                             
PROF4    DS    CL16                                                             
PROF5    DS    CL16                                                             
PROF6    DS    CL16                                                             
*                                                                               
STATBERR DC    H'0'                                                             
CMLTBERR DC    H'0'                                                             
*                                                                               
AGYCNT   DC    PL4'0'                                                           
*                                                                               
CCSTACNT DC    A(0)                                                             
CCCMLCNT DC    A(0)                                                             
SVCMLCNT DC    A(0)                                                             
CCGOLCNT DC    A(0)                                                             
*                                                                               
THISAGY  DS    CL2                                                              
CTRS     DS    0CL24                                                            
INCNT    DC    PL4'0',CL20'RECORDS IN'                                          
OUTCNT   DC    PL4'0',CL20'RECORDS OUT'                                         
DELCNT   DC    PL4'0',CL20'RECORDS DELETED'                                     
ADDCNT   DC    PL4'0',CL20'RECORDS INSERTED'                                    
CTRX     EQU   *-1                                                              
         LTORG                                                                  
CLTTOT   DS    8PL8                                                             
NETTOT   DS    4PL8                                                             
         DS    CL20                                                             
         EJECT                                                                  
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,MACRF=GM,               X        
               EODAD=XK50                                                       
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,MACRF=PM                        
*                                                                               
TOTBWORK DS    CL32                                                             
BINTABL  DS    CL100                                                            
BINTABLX DS    0H                                                               
         DS    0D                                                               
         DC    C'*TAPEIO*'                                                      
TAPEIO   DS    4000C                                                            
*                                                                               
         DS    0D                                                               
         DC    C'*PRDTAB '                                                      
PRDTAB   DS    250CL8              PRODUCT TABLE FOR ALL ADV PRODUCTS           
*                                   FOR ALL ADV CLIENTS                         
PRDTABX  EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'CCESTTAB'                                                    
CCESTTAB DS    210XL256                                                         
CCESTTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'SVCLTHDR'                                                    
SVCLTHDR DS    1000C               THIS AREA FOR AGY/CC CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'***CCBJM'                                                    
CCBJM   DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'***CCAPA'                                                    
CCAPA   DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'***CCLAI'                                                    
CCLAI   DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**CLT1**'                                                    
CLI1    DS     1000C               THIS AREA FOR AD/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**CLT2**'                                                    
CLI2    DS     1000C               THIS AREA FOR AD/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**CCRR**'                                                    
CCRR    DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**CCMC**'                                                    
CCMC    DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**CCWV**'                                                    
CCWV    DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**CCXXX*'                                                    
CCXXX   DS     1000C               THIS AREA FOR XFRAGY CODES                   
*                                                                               
         DS    0D                                                               
         DC    CL8'PUBCONVT'                                                    
AGPUBTAB DS    52000C              4000 13 BYTE ENTRIES                         
AGPUBTAX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'TOT ADDP'               ADD                DEL               
ATAB1    DC    C'M',4PL8'0',4PL8'0' GROSS,PAID,NET,COUNT GR,PD,N,CNT            
ATABLEN  EQU   *-ATAB1                                                          
         DC    C'N',4PL8'0',4PL8'0'                                             
         DC    C'O',4PL8'0',4PL8'0'                                             
         DC    C'S',4PL8'0',4PL8'0'                                             
         DC    C'T',4PL8'0',4PL8'0'                                             
**                                                                              
ATAB2    DC    C'M',4PL8'0',4PL8'0'                                             
         DC    C'N',4PL8'0',4PL8'0'                                             
         DC    C'O',4PL8'0',4PL8'0'                                             
         DC    C'S',4PL8'0',4PL8'0'                                             
         DC    C'T',4PL8'0',4PL8'0'                                             
**                                                                              
ATAB3    DC    C'M',4PL8'0',4PL8'0'                                             
         DC    C'N',4PL8'0',4PL8'0'                                             
         DC    C'O',4PL8'0',4PL8'0'                                             
         DC    C'S',4PL8'0',4PL8'0'                                             
         DC    C'T',4PL8'0',4PL8'0'                                             
**                                                                              
ATAB4    DC    C'M',4PL8'0',4PL8'0'                                             
         DC    C'N',4PL8'0',4PL8'0'                                             
         DC    C'O',4PL8'0',4PL8'0'                                             
         DC    C'S',4PL8'0',4PL8'0'                                             
         DC    C'T',4PL8'0',4PL8'0'                                             
**                                                                              
ATAB5    DC    C'M',4PL8'0',4PL8'0'                                             
         DC    C'N',4PL8'0',4PL8'0'                                             
         DC    C'O',4PL8'0',4PL8'0'                                             
         DC    C'S',4PL8'0',4PL8'0'                                             
         DC    C'T',4PL8'0',4PL8'0'                                             
**                                                                              
*                                                                               
         SPACE 1                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CCSTEQTB'                                                    
CCSTEQTB DS    200XL10                                                          
CCSTEQBX EQU   *-1                                                              
CCSTEQNB EQU   (*-CCSTEQTB)/10                                                  
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'TOTBUFF'                                                     
TOTBUFF  DS    800D                                                             
TOTBUFFX DS    0C                                                               
         DS    D                   SPARE                                        
IOAREA   DS    CL4000                                                           
         EJECT                                                                  
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDCNTRL                                                        
         PRINT ON                                                               
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
ESTHDRD  DSECT                                                                  
       ++INCLUDE PESTREC                                                        
PUBMASTD DSECT                                                                  
       ++INCLUDE PUBREC                                                         
BUYRECD  DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
         PRINT ON                                                               
PRDTABD  DSECT **PRODUCT TABLE FOR ADVERTISER ***                               
PRDTAGY  DS    CL2                 AGENCY                                       
PRDTMED  DS    CL1                 MEDIA                                        
PRDTCLT  DS    CL3                 ADV CLIENT                                   
PRDTSPR  DS    CL2                 ADV PROD                                     
*                                                                               
         SPACE 3                                                                
AGCLDSCT DSECT                                                                  
AGCLAGY  DS    CL2                 BUYING AGENCY POWER CODE                     
AGCLCLT  DS    CL3                 AGENCY CLIENT CODE                           
AGCLAPC  DS    CL2                 ADVERTISER POWER CODE                        
AGCLACLT DS    CL3                 ADVERTISER CLIENT CODE                       
         DS    0F                  ALIGN                                        
AGCLADV DS     A                   POINTER TO CLIENT HDR                        
AGCLINDI DS    XL4                 INDICATORS                                   
AGCLMEDP DS    A                   MEDIA POINTERS                               
AGCLPROF DS    A                   PROFILE POINTER                              
AGCLATL  DS    A                   TOTAL ADD POINTERS (GROSS,PAID,NET)          
*                                  COUNT) DELETED (GR,PD,NT,COUNT)              
*                                                                               
AGCLPROC EQU   4                   PROCESSED                                    
AGPCLIP  EQU   1                   CLIENT RECORD READ                           
AGPRODP  EQU   2                   PROCESSED PRODUCTS                           
AGCLLEN  EQU   *-AGCLAGY                                                        
************                                                                    
         SPACE 3                                                                
************                                                                    
TOTBUFD  DSECT                                                                  
TOTFUNC  DS    CL1                 +/- INDICATOR                                
TOTAGY   DS    CL2                 AGENCY                                       
TOTMED   DS    CL1                 MEDIA                                        
TOTCLT   DS    CL3                                                              
TOTAMTS  DS    3PL8                                                             
TOTLEN   EQU   *-TOTFUNC                                                        
**                                                                              
         SPACE 3                                                                
PRWORKD  DSECT                                                                  
ERRKEY   DS    CL30                                                             
ERRMSG   DS    CL102                                                            
ERRSTEQ  DS    CL132                                                            
         ORG                                                                    
       ++INCLUDE DDBSPARA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034PPREPXK02N05/01/02'                                      
         END                                                                    
