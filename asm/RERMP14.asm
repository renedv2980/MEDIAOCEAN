*          DATA SET RERMP14    AT LEVEL 047 AS OF 05/01/02                      
*PHASE T8040BA,*                                                                
         TITLE 'T8040B - RELFM0B - REP FILE ESTIMATE OVERLAY'                   
*                                                                               
*******************************************************************             
*                                                                 *             
*        RELFM0B --- REP INVENTORY DEMO RECORD ESTIMATE SCREEN    *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* JUN16/89 (MRR) --- DOCUMENTATION SAYS 'RECALC' DEFAULT TO NO,   *             
*                     PROGRAM WAS SAYING YES, CHANGE PROGRAM      *             
*                                                                 *             
* JUN20/90 (MRR) --- BY-PASS DEMOMATH CALL OF RECLAC IS NO, REMOVE*             
*                     INDEX ELEMENT AFTER CALL IF RECALC IS YES   *             
*                                                                 *             
* OCT08/90 (MRR) --->DEMO COLUMN DATA IS NOW 7                    *             
*                   >'OFOMRAT' HAS MOVED TO NOV/83                *             
*                                                                 *             
* JUL26/91 (BU ) --- IMPLEMENT NEW FORMAT DEMO OVERRIDE ELEMENTS  *             
*                                                                 *             
* APR05/94 (SKU) --- FIX BUG IN ADDING OVERRIDE ELEMENTS          *             
*                                                                 *             
* AUG09/94 (RZ ) --- SUPPORT MULTIPLE IUN DISPLACEMENTS           *             
*                                                                 *             
* JUN30/95 (SKU) --- FIX 'UT' DEMUP PROBLEM                       *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8040B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8040B,R9                                                      
         L     RC,0(R1)            GLOBAL WORKING STORAGE                       
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
*                                                                               
         LA    R8,WORK3            R8 POINTS TO LOCAL WORKING STORAGE           
         LA    R8,2000(R8)                                                      
         USING ESTD,R8                                                          
         LR    RE,R8               CLEAR LOCAL WORKING STORAGE                  
         LA    RF,ESTDX-ESTD                                                    
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R7,ACOMFACS         R7 COVERS COMFACS                            
         USING COMFACSD,R7                                                      
         EJECT                                                                  
* EXTRACT THE STATION CALL LETTERS AND GET THE MARKET NAME                      
*                                                                               
EST      DS    0H                                                               
         LA    R2,LFMKEYH                                                       
         FOUT  ESTMRKTH,SPACES,25                                               
         SPACE 1                                                                
         GOTO1 VPAVSTA,DMCB,8(R2),WORK   GET STATION AND MEDIA                  
         MVC   INVSTA,WORK                                                      
         MVC   INVMED,WORK+5                                                    
         SPACE 1                                                                
         GOTO1 VMKTNME,DMCB,WORK,ESTMRKTH                                       
         MVC   DEMEDIA(8),INVMED   MEDIA/STATION/MARKET FOR DEMOS               
         SPACE 2                                                                
* EDIT THE KEY FIELD                                                            
*                                                                               
EST2     DS    0H                                                               
         L     R4,AIOAREA                                                       
         USING RINVD,R4                                                         
         TM    LFMKEYH+4,X'20'     TEST IF PREVIOUSLY VALIDATED                 
         BO    *+8                                                              
         MVI   BFMTSW,0            FORCE DISPLAY                                
*                                                                               
         XC    RINVKEY,RINVKEY                                                  
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,REPALPHA                                                
         MVC   RINVKSTA,INVSTA                                                  
         GOTO1 CSCANNER,DMCB,(R2),(4,WORK3),0                                   
         LA    R3,INVERR                                                        
         MVC   BYTE,DMCB+4         NUMBER OF FIELDS                             
         CLI   BYTE,0              TEST FOR ERROR                               
         BE    ERROR                                                            
         CLI   BYTE,2              INVENTORY NUMBER MUST BE THERE               
         BL    ERROR                                                            
         CLI   BYTE,4              NO MORE THAN 4 FIELDS                        
         BH    ERROR                                                            
*                                                                               
         LA    R6,WORK3+32         FIRST BLOCK TO EDIT                          
         BAS   R5,INVEDT                                                        
         MVI   RATIMP,C'T'         DEFAULT IS IMPRESSIONS                       
         CLI   BYTE,3              TEST FOR ANY MORE DATA                       
         BL    EST6                                                             
         LA    R6,32(R6)           NEXT BLOCK                                   
         CLI   0(R6),1                                                          
         BE    *+12                                                             
         BAS   R5,DATEDT                                                        
         B     EST4                                                             
*                                                                               
         MVC   RATIMP,12(R6)                                                    
         CLI   RATIMP,C'R'                                                      
         BE    EST4                                                             
         CLI   RATIMP,C'T'                                                      
         BE    EST4                                                             
         CLI   RATIMP,C'I'                                                      
         BNE   ERROR                                                            
         MVI   RATIMP,C'T'                                                      
         SPACE 1                                                                
EST4     DS    0H                                                               
         CLI   BYTE,4                                                           
         BL    EST6                                                             
         LA    R6,32(R6)                                                        
         CLI   0(R6),1                                                          
         BNE   ERROR                                                            
         MVC   RATIMP,12(R6)                                                    
         CLI   RATIMP,C'R'                                                      
         BE    EST6                                                             
         CLI   RATIMP,C'T'                                                      
         BE    EST6                                                             
         CLI   RATIMP,C'I'                                                      
         BNE   ERROR                                                            
         MVI   RATIMP,C'T'                                                      
         SPACE 2                                                                
EST6     DS    0H                                                               
         BAS   RE,HEADER                                                        
         CLI   ERRAREA,0           TEST FOR ERROR                               
         BNE   EXXMOD                                                           
         MVC   TRSVKEY,RINVKEY     SAVE HEADER'S KEY                            
         OI    LFMKEYH+4,X'20'     TURN ON VALID KEY BIT                        
         OI    LFMKEYH+6,X'80'     XMIT BACK                                    
*                                                                               
         CLI   BFMTSW,0            TEST FOR DISPLAY MODE                        
         BE    *+12                                                             
         CLI   BACT,C'X'           TEST FOR DELETE                              
         BE    EST15                                                            
*                                                                               
         BAS   RE,VALTR            EDIT TRANSFER DETAILS                        
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         BAS   RE,DATAREC          GENERATE DATA RECORD                         
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                                                               
         BAS   RE,GETAD            GET TABLE ADDRESSES                          
         BAS   RE,GETDEM           EXTRACT DEMOS FROM DATA RECORD               
         SPACE 2                                                                
* DISPLAY LOGIC                                                                 
*                                                                               
EST8     DS    0H                                                               
         CLI   BFMTSW,1            TEST FOR CHANGE                              
         BE    EST10                                                            
         BAS   RE,DCAT             DISPLAY CELL NAMES                           
         BAS   RE,DISVAL           DISPLAY RECORD VALUES                        
         CLI   BACT,C'D'           TEST FOR ACTION DISPLAY                      
         BE    *+12                                                             
         OI    ESTFBKH+1,X'01'     MAKE FR BOOK MODIFIED SO DEL OR CHA          
         OI    ESTFBKH+6,X'80'     CAN BE RE-INPUT AFTER FORCED DISPLAY         
         B     ESTX                                                             
         SPACE 2                                                                
* CHANGE LOGIC                                                                  
*                                                                               
EST10    DS    0H                                                               
         BAS   RE,GETVAL           GET INPUT VALUES                             
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         BAS   RE,GETOVER          GENERATE OVERRIDE ELS AND DO RECALC          
*                                                                               
         CLI   RECALC,NO                                                        
         BE    EST12                                                            
         XC    DBLOCK,DBLOCK       RECALCULATE IUN RECORD VALUES TO             
         MVC   DBFILE,=C'INV'      PICK UP THE OVERRIDE VALUES                  
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBAREC,AIOAREA                                                   
         ST    R7,DBCOMFCS                                                      
         L     R1,DBAREC                                                        
         LA    R1,34(R1)           POINT AT FIRST ELEMENT                       
         ST    R1,DBAQUART                                                      
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
*                                  BUILD DEMOMATH BLOCK                         
         XC    MATHFACS(MATHFACL),MATHFACS                                      
         LA    RE,DBLOCK                                                        
         ST    RE,MATHDBLK                                                      
         MVC   MATHIFIL,DBFILE                                                  
         MVC   MATHOFIL,DBFILE                                                  
         MVC   MATHOSRC(1),DBSELSRC                                             
*                                                                               
         GOTO1 CDEMOMTH,DMCB,=C'REC',DBAREC,DBAREC,MATHFACS                     
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETEL,DMCB,(X'DE',AIOAREA),DMCB+8                               
         CLI   DMCB,X'FF'                                                       
         BE    EST12                                                            
         L     R1,DMCB+8                                                        
EST11A   EQU   *                                                                
         CLC   0(2,R1),=X'DE07'    INDEX ELEMENT?                               
         BE    EST11D                                                           
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         CLI   0(R1),X'DE'         STILL OVERRIDE?                              
         BE    EST11A                                                           
         B     EST12                                                            
EST11D   EQU   *                                                                
         MVI   0(R1),X'FF'                                                      
         GOTO1 CHELLO,DMCB,(C'D',=C'REPFILE '),(X'FF',AIOAREA),0,0              
*                                                                               
EST12    EQU   *                                                                
         SR    R1,R1               ADJUST RECORD LENGTH                         
         ICM   R1,3,RINVLEN                                                     
         BCTR  R1,0                                                             
         STCM  R1,3,RINVLEN                                                     
         BAS   RE,FLADD            NOW WRITE TO REPFILE                         
         B     ESTX                                                             
         SPACE 2                                                                
* DELETE BOOK LOGIC                                                             
*                                                                               
EST15    DS    0H                                                               
         LA    R2,ESTFBKH                                                       
         BAS   RE,ANY                                                           
         GOTO1 VBOOKVAL,DMCB,(R2),(1,INVSRC),CSCANNER                           
         CLI   DMCB+4,1            TEST FOR ERROR                               
         BNE   ERROR                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RINVKEY     BORROW THE HEADER'S KEY                      
         LA    R4,KEY                                                           
         MVC   RINVKSRC(3),INVSRC  SOURCE/BOOK TO DELETE                        
*                                                                               
         LA    R6,SVCLST           CONVERT FROM BOOKVAL TO KSRC FMT             
EST17    CLC   INVSRC(1),3(R6)                                                  
         BE    EST18                                                            
         LA    R6,L'SVCLST(R6)                                                  
         CLI   0(R6),X'FF'                                                      
         BNE   EST17                                                            
         DC    H'0'                                                             
EST18    MVC   RINVKSRC,2(R6)                                                   
*                                                                               
EST20    LA    R3,ERRNF                                                         
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   ERROR                                                            
         OI    KEY+27,X'80'                                                     
         BAS   RE,WRITE                                                         
         XC    SVDATA,SVDATA       CLEAR SAVE AREA TO FORCE DIS NEXT            
         B     ESTX                                                             
         SPACE 2                                                                
ESTX     DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
* ROUTINE TO EDIT TRANSFER DETAIL FIELDS                                        
*                                                                               
VALTR    NTR1                                                                   
         LA    R5,SVDATA           POINT TO INPUT SAVE AREA                     
         USING SAVED,R5                                                         
         LA    R2,ESTFBKH          EDIT FROM BOOK                               
         BAS   RE,ANY                                                           
         LA    R3,INVERR                                                        
         GOTO1 VBOOKVAL,DMCB,(R2),(1,INVSRC),(C'B',CSCANNER),INVBTYPE           
         CLI   DMCB+4,1                                                         
         BNE   ERROR                                                            
         CLC   INVSRC(3),=C'SAM'   TEST FOR SAME                                
         BE    ERROR                                                            
         CLC   SVFBT,INVBTYPE      LAST INPUT VS BOOK TYPE                      
         BNE   VALTR0                                                           
         CLC   SVFBK,INVSRC        LAST INPUT VS FROM BOOK                      
         BE    *+8                                                              
VALTR0   MVI   BFMTSW,0            FORCE DISPLAY FOR DIFFERENCE                 
         MVC   SVFBK,INVSRC        SAVE THIS TIME INPUT                         
         MVC   SVFBT,INVBTYPE                                                   
         MVC   INVTOBK(3),INVSRC   SET TO BOOK EQUAL TO FROM BOOK               
         SPACE                                                                  
VALTR1   DS    0H                                                               
         LA    R2,ESTTOBKH                                                      
         CLI   5(R2),0             TEST FOR ANY INPUT                           
         BE    VALTR1A                                                          
*                                                                               
         GOTO1 VBOOKVAL,DMCB,(R2),(1,INVTOBK),CSCANNER                          
         CLI   DMCB+4,1                                                         
         BNE   ERROR                                                            
         CLC   INVTOBK(3),=C'SAM'  TEST FOR SAME                                
         BNE   *+10                                                             
         MVC   INVTOBK(3),INVSRC   MAKE TO BK EQ TO FROM BK                     
*                                                                               
VALTR1A  CLC   SVTOBK,INVTOBK      TEST FOR CHANGE IN INPUT                     
         BE    *+8                                                              
         MVI   BFMTSW,0            FORCE DISPLAY                                
         MVC   SVTOBK,INVTOBK                                                   
         SPACE 1                                                                
VALTR2   DS    0H                                                               
         LA    R2,ESTCODEH         VALIDATE PROGRAM CODE                        
         BAS   RE,MOVE                                                          
         LA    RE,CODETAB                                                       
         LA    R1,CODES                                                         
         CLC   WORK(2),0(RE)                                                    
         BE    VALTR3                                                           
         LA    RE,L'CODETAB(RE)                                                 
         BCT   R1,*-14                                                          
         LA    RE,MONTAB           NOT A CODE, LOOK FOR A MONTH/YEAR            
         LA    R1,MONTHS                                                        
         CLC   WORK(1),0(RE)                                                    
         BE    *+16                VALID MONTH CODE                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-14                                                          
         B     ERROR                                                            
         SPACE                                                                  
         CLI   WORK+1,C'0'         NOW LOOK FOR A NUMBER                        
         BL    ERROR                                                            
         CLI   WORK+1,C'9'                                                      
         BH    ERROR                                                            
         MVC   INVCODE,WORK                                                     
         MVI   INVCDCTL,PRO+INV                                                 
         B     VALTR4                                                           
         SPACE                                                                  
VALTR3   DS    0H                                                               
         MVC   INVCODE,WORK                                                     
         MVC   INVCDCTL,2(RE)      CONTROL BITS                                 
         SPACE                                                                  
VALTR4   DS    0H                                                               
         CLC   INVCODE,=C'TP'      DO NOT PERMIT CODE 'TP' IF                   
         BNE   VALTR5              FROM BOOK IS PRIOR TO OCT82                  
         CLC   INVFBK,=X'520A'                                                  
         BNL   VALTR5                                                           
         LA    R2,ESTCODEH                                                      
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 1                                                                
VALTR5   DS    0H                                                               
         CLC   SVCODE,INVCODE                                                   
         BE    *+8                                                              
         MVI   BFMTSW,0            FORCE DISPLAY                                
         MVC   SVCODE,INVCODE                                                   
*                                                                               
         MVI   INVTYP,C'P'         DEFAULT-FROM DEMO FILES                      
         CLI   BACT,C'X'           TEST FOR FORCED DIS ON DELETE                
         BNE   *+8                                                              
         MVI   INVTYP,C'I'         SET DEFAULT TO INVENTORY HERE                
         LA    R2,ESTTYPEH                                                      
         CLI   5(R2),0                                                          
         BE    *+18                                                             
         CLI   5(R2),1             VALIDATE TYPE                                
         BNE   ERROR                                                            
         MVC   INVTYP,8(R2)                                                     
*                                                                               
         CLI   INVTYP,C'I'         TEST FOR INVENTORY TRANSFER                  
         BE    VALTR6                                                           
         CLI   INVTYP,C'P'                                                      
         BNE   ERROR                                                            
         TM    INVCDCTL,PRO        CODE CONSISTENCY WITH 'P'                    
         BZ    ERROR                                                            
         SPACE                                                                  
VALTR6   DS    0H                                                               
         CLC   SVTYPE,INVTYP                                                    
         BE    *+8                                                              
         MVI   BFMTSW,0                                                         
         MVC   SVTYPE,INVTYP                                                    
         LA    R2,ESTFROMH                                                      
         SPACE                                                                  
         CLI   5(R2),0             TEST IF FROM DETAILS INPUT                   
         BE    VALTR8              NO                                           
         SPACE 1                                                                
         XC    TRFIELD,TRFIELD     BUILD DUMMY HEADER AND FIELD                 
         MVI   TRFIELD,L'TRFIELD   FOR FROM DETAILS-THEN GO TO                  
         ZIC   RF,5(R2)            INVENTORY LIST MODULE                        
         LR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRFIELD+8(0),8(R2)                                               
         STC   RF,TRFIELD+5        INPUT LENGTH                                 
         SPACE                                                                  
VALTR7   DS    0H                                                               
         LA    R6,REC2+500         REC2+500 CONTAINS INVLIST                    
         ST    R6,INVLIST                                                       
         LA    R6,WORK3            WORK3 AVAILABLE FOR SCANNER BLOCK            
         GOTO1 VINVLST,DMCB,ESTFROMH,(R6),(RC)                                  
         CLI   INVNO,0             TEST FOR ERROR IN INVLST                     
         BNE   VALTR15                                                          
         ZIC   R3,FERN             INSERT ERROR NUMBER                          
         B     ERROR                                                            
*                                                                               
         SPACE 2                                                                
* FOR NO FROM DETAILS-GENERATE THE DEFAULT DATA IN CHARACTER FORMAT             
* AND BUILD A DUMMY INVENTORY LIST ENTRY WITH THE DEFAULT DETAILS               
*                                                                               
VALTR8   DS    0H                                                               
         CLI   INVTYP,C'I'                                                      
         BE    VALTR9              FOR BOOK TRANSFER                            
         CLC   INVCODE,=C'PR'      CODE 'PR' REQUIRES PURE NUMBER               
         BNE   VALTR9                                                           
         LA    R3,MISINP                                                        
         B     ERROR                                                            
*                                                                               
VALTR9   DS    0H                                                               
         MVI   INVNO,1             ONE DUMMY LIST ENTRY                         
         LA    R3,REC2+500         A(INVLIST)                                   
         ST    R3,INVLIST                                                       
         USING INVLD,R3                                                         
*                                                                               
         XC    TRFIELD,TRFIELD     ALSO BUILD A DUMMY HEADER AND                
         MVI   TRFIELD,L'TRFIELD   DATA W DEFAULT DETAILS                       
         LA    R6,TRFIELD+8        POINT R6 AT DATA START                       
         XC    INVLREC,INVLREC                                                  
         MVI   INVLWT,1                                                         
         MVC   INVLFLE,INVTYP      TAKE FILE FROM TYPE                          
         CLI   INVLFLE,C'I'        TEST FOR INVENTORY                           
         BE    VALTR12                                                          
*                                                                               
         OI    INVLTYP,X'60'       SET DAY/TIME BITS                            
         MVC   INVLDAY,INVDAY      USE HEADER'S DAY/TIME IN                     
         MVC   INVLSTIM(4),INVTIME DUMMY ENTRY FOR BOOK TRANSFER                
         CLC   INVTIME+2(2),=C'CC' TEST FOR TO CONCLUSION                       
         BNE   VALTR10                                                          
*                                                                               
         SR    R1,R1               ADD 2 HOURS TO START                         
         ICM   R1,3,INVLSTIM                                                    
         AH    R1,=H'200'                                                       
         CH    R1,=H'2400'         TEST FOR RUN PAST MIDNIGHT                   
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STCM  R1,3,INVLSTIM+2     SET END TIME                                 
         B     VALTR10                                                          
         SPACE 1                                                                
* GENERATE EBCDIC DAY/TIME DETAILS AT TRFIELD                                   
*                                                                               
VALTR10  DS    0H                                                               
         GOTO1 VUNDAY,DMCB,INVDAY,(R6)                                          
         CLI   0(R6),C' '          TEST FOR END OF DAY EXPRESSION               
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         MVI   0(R6),COMMA         INSERT COMMA AFTER IT                        
         LA    R6,1(R6)                                                         
*                                                                               
         GOTO1 VUNTIME,DMCB,INVTIME,(R6)                                        
         CLI   0(R6),C' '          FIND END OF TIME EXPRESSION                  
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         OC    INVTIME+2(2),INVTIME+2 TEST FOR BREAK CODE                       
         BNZ   *+14                                                             
         MVC   0(2,R6),=C',B'                                                   
         LA    R6,2(R6)            BUMP OUTPUT POINTER                          
         LA    R1,TRFIELD+8                                                     
         SR    R6,R1               FIND LENGTH OF DATA                          
         STC   R6,TRFIELD+5                                                     
         B     VALTR15                                                          
         SPACE 1                                                                
* GENERATE EBCDIC INVENTORY NUMBER/DATE DETAILS                                 
*                                                                               
VALTR12  DS    0H                                                               
         MVI   INVLTYP,X'80'                                                    
         LA    RE,TRSVKEY                                                       
         MVC   INVLNUMB,RINVKINV-RINVKEY(RE)                                    
         MVC   INVLDATE,RINVKSTD-RINVKEY(RE)                                    
         ZIC   R1,INVLNUMB         QUARTER HOUR                                 
         CVD   R1,DUB                                                           
         UNPK  0(2,R6),DUB+6(2)                                                 
         OI    1(R6),X'F0'                                                      
         MVC   2(1,R6),INVLNUMB+1  DAY CODE                                     
         LA    R6,3(R6)                                                         
         CLI   INVLNUMB+2,C'0'     TEST FOR LENGTH OR SPECIAL CODE              
         BE    *+14                                                             
         MVC   0(1,R6),INVLNUMB+2                                               
         LA    R6,1(R6)                                                         
*                                                                               
         MVI   0(R6),COMMA         COMMA AFTER INVENTORY NUMBER                 
         LA    R6,1(R6)                                                         
         GOTO1 VDATCON,DMCB,(3,INVLDATE),(5,(R6))                               
         LA    R6,8(R6)                                                         
         LA    R1,TRFIELD+8                                                     
         SR    R6,R1               FIND DATA LENGTH                             
         STC   R6,TRFIELD+5                                                     
         B     VALTR15                                                          
         DROP  R3                                                               
         SPACE 2                                                                
VALTR15  DS    0H                                                               
         LA    R2,TRFIELD                                                       
         BAS   RE,MOVE                                                          
         CLC   SVFROM,WORK                                                      
         BE    *+8                                                              
         MVI   BFMTSW,0                                                         
         MVC   SVFROM,WORK                                                      
*                                                                               
         MVI   RECALC,NO           DEFAULT IS NOT TO RECALCULATE                
         LA    R2,ESTRECAH                                                      
         CLI   5(R2),0                                                          
         BE    VALTRX                                                           
         LA    R3,INVERR           EDIT RECALC FIELD                            
         MVC   RECALC,8(R2)                                                     
         CLI   RECALC,YES                                                       
         BE    VALTRX                                                           
         CLI   RECALC,NO                                                        
         BNE   ERROR                                                            
         SPACE 1                                                                
VALTRX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R5                                                               
         SPACE 2                                                                
CODETAB  DS    0CL3                                                             
         DC    C'TP',AL1(PRO+TP)                                                
         DC    C'TT',AL1(PRO+TP)                                                
         DC    C'ES',AL1(PRO+INV)                                               
         DC    C'PJ',AL1(PRO+INV)                                               
         DC    C'PR',AL1(PRO)                                                   
         DC    C'PA',AL1(PRO+INV)                                               
         DC    C'PT',AL1(PRO+INV+MIX)                                           
         DC    C'TE',AL1(PRO+INV)                                               
         DC    C'PE',AL1(PRO+INV)                                               
         DC    C'NT',AL1(PRO+INV)                                               
         DC    C'FT',AL1(PRO+INV)                                               
         DC    C'MT',AL1(PRO+INV)                                               
         DC    C'YT',AL1(PRO+INV)                                               
         DC    C'JT',AL1(PRO+INV)                                               
         DC    C'OT',AL1(PRO+INV)                                               
         DC    C'RT',AL1(PRO+INV)                                               
         DC    C'NP',AL1(PRO+INV)                                               
         DC    C'FP',AL1(PRO+INV)                                               
         DC    C'MP',AL1(PRO+INV)                                               
         DC    C'YP',AL1(PRO+INV)                                               
         DC    C'OP',AL1(PRO+INV)                                               
         DC    C'RP',AL1(PRO+INV)                                               
         DC    C'JP',AL1(PRO+INV)                                               
         DC    C'  ',AL1(PRO+INV)                                               
CODES    EQU   (*-CODETAB)/L'CODETAB                                            
         SPACE                                                                  
MONTAB   DC    C'NFMAYJO'                                                       
MONTHS   EQU   (*-MONTAB)                                                       
         DS    0H                                                               
         EJECT                                                                  
* ROUTINE TO CREATE DATA RECORD BY A TRANSFER AND UPGRADE                       
*                                                                               
DATAREC  NTR1                                                                   
         LA    RE,REC              CLEAR AND BUILD DATA RECORD AT REC           
         LA    RF,1000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         SPACE                                                                  
         GOTO1 VCALLOV,DMCB,(X'17',0),(RA)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VT80417,DMCB        A(DEMO INTERFACE MODULE)                     
         SPACE 1                                                                
* BUILD A DUMMY KEY FOR DATA RECORD - THEN CALL INTERFACE MODULE                
*                                                                               
DATAR2   DS    0H                                                               
         MVC   RINVKEY,TRSVKEY     MOVE IN HEADER'S KEY                         
         MVC   RINVKSRC(3),INVTOBK                                              
*                                                                               
         LA    R6,SVCLST           CONVERT FROM BOOKVAL TO KSRC FMT             
DATAR2G  CLC   INVTOBK(1),3(R6)                                                 
         BE    DATAR2J                                                          
         LA    R6,L'SVCLST(R6)                                                  
         CLI   0(R6),X'FF'                                                      
         BNE   DATAR2G                                                          
         DC    H'0'                                                             
DATAR2J  MVC   RINVKSRC,2(R6)                                                   
*                                                                               
         SPACE 1                                                                
DATAR3   MVC   RINVLEN,=H'35'      SET LENGTH FOR VIRGIN RECORD                 
         SPACE 1                                                                
         GOTO1 VT80417,DMCB,(RC)                                                
         SPACE 1                                                                
         CLI   INVBAD,0                                                         
         BE    DATAR4                                                           
         ZIC   R3,INVBAD           ERROR MESSAGE FROM DEMO MODULE               
         LA    R2,ESTFROMH         POSITION CURSOR AT FROM FIELD                
         B     ERROR                                                            
         SPACE                                                                  
DATAR4   DS    0H                                                               
         MVC   HALF,27(R4)         REPAIR RECORD LENGTH AFTER                   
         LH    RE,HALF             DEMO MODULES                                 
         BCTR  RE,0                                                             
         STCM  RE,3,27(R4)                                                      
         SPACE 1                                                                
         XC    WORK,WORK           PUT IN 'CE' EL BEFORE DEMUP CALL.            
         MVC   WORK(2),=X'CE0A'                                                 
         MVC   WORK+2(5),INVDAY    HEADER DAY TIME                              
         MVC   WORK+7(3),INVSRC    FROM BOOK                                    
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         SPACE                                                                  
* TRANSFER FROM ELEMENT HAS FROM DETAILS INPUT                                  
*                                                                               
DATAR5   DS    0H                                                               
         XC    REC2(200),REC2      DEMUP GETS FROM SOURCE/BOOK                  
         LA    RE,REC2             FROM TRANSFER FROM ELEMENT                   
         USING RINVFREL,RE                                                      
         MVI   RINVFRCD,X'03'                                                   
         MVC   RINVFRST,DEMSTA                                                  
         MVC   RINVFRBK,INVSRC                                                  
         MVC   RINVFRTY,INVTYP                                                  
         MVC   RINVFRBT,INVBTYPE                                                
         CLI   INVTYP,C'I'         TEST FOR INV. TO INV. TRANSFER               
         BNE   *+10                NO                                           
         MVC   RINVFRBT,INVFRBT    USE BOOK TYPE PASSED BY T80417               
         MVI   RINVFRPR,C'E'       ESTIMATE OVERLAY                             
         ZIC   R1,TRFIELD+5        VARIABLE DATA LENGTH                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RINVFRDT(0),TRFIELD+8 FROM DETAILS                               
         LA    R1,RINVFRDT-RINVFREL+1(R1) FIND EL LEN                           
         STC   R1,RINVFRLN                                                      
         GOTO1 VADDELEM,DMCB,(R4),REC2                                          
         DROP  RE                                                               
         SPACE 1                                                                
DATAR6   DS    0H                                                               
         LA    RE,WORK             BUILD CODE ELEMENT                           
         USING RINVCEL,RE                                                       
         XC    WORK,WORK                                                        
         MVI   RINVCCOD,X'CD'                                                   
         MVI   RINVCLEN,10                                                      
         MVC   RINVCODE,INVCODE                                                 
*                                                                               
         TM    INVCDCTL,TP         FOR TIME PERIOD TRANSFERS WHERE              
         BZ    *+18                AUTOMATIC FOOTNOTING IS SUPPRESSED,          
         CLI   TRFNOVER,YES        CLEAR THE CODE ON RECORD                     
         BNE   *+10                                                             
         MVC   RINVCODE,SPACES                                                  
*                                                                               
         TM    INVTOBK,X'20'       ESTIMATED TO BOOK TEST                       
         BZ    *+8                                                              
         MVI   RINVCSET,C'E'                                                    
*                                                                               
         TM    INVTOBK,X'04'       PROJECTED TO BOOK TEST                       
         BZ    *+8                                                              
         MVI   RINVCSET,C'P'                                                    
*                                                                               
         TM    INVTOBK,X'02'       SPECIAL SURVEY BOOK TEST                     
         BZ    *+8                                                              
         MVI   RINVCSET,C'S'                                                    
*                                                                               
         CLI   ESTFROM,C'+'        TEST FOR COMBO                               
         BNE   *+8                                                              
         OI    RINVCTYP,X'80'      ADDED DEMOS                                  
         OC    RINVCTYP,INVIND     CUMULATIVE INDICATORS                        
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         DROP  RE                                                               
         SPACE 1                                                                
* EITHER PERFORM USER UPGRADE OR FORCE AN INDEX 100 UPGRADE                     
* TO MAKE SURE RECORD HAS HPT'S                                                 
*                                                                               
DATAR8   DS    0H                                                               
         LA    R2,ESTUPGH                                                       
         LA    R5,SVDATA                                                        
         USING SAVED,R5                                                         
         BAS   RE,MOVE             EXTRACT UPGRADE INPUT                        
         CLC   SVUP,WORK           TEST FOR CHANGE IN UPG INPUT                 
         BE    *+8                                                              
         MVI   BFMTSW,0            FORCE DISPLAY                                
         MVC   SVUP,WORK                                                        
         DROP  R5                                                               
*                                                                               
         CLI   5(R2),0             TEST FOR USER INPUT                          
         BE    DATAR12                                                          
*                                                                               
* CODE FOR USER INPUT UPGRADE                                                   
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 VUPVAL,DMCB,(1,(R2)),WORK,(R7)                                   
         CLI   DMCB,1                                                           
         BE    *+12                                                             
         LA    R3,235              INVALID UPGRADE                              
         B     ERROR                                                            
         SPACE                                                                  
         CLI   ESTFROM,C'+'        UPGRADES ARE NO GOOD FOR COMBOS              
         BNE   *+12                                                             
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 1                                                                
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         CLI   RAVLNTYP,3          ONLY PERMIT ONE BOOK OPERAND                 
         BNE   DATAR9              FOR PUTS                                     
         CLI   RAVLNCAT,C'P'                                                    
         BNE   DATAR9                                                           
         OC    RAVLNOP2,RAVLNOP2                                                
         BZ    DATAR9                                                           
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 1                                                                
DATAR9   CLI   RAVLNTYP,0          MANIPULATION OF UPGRADE DATA                 
         BE    DATAR10                                                          
         OC    RAVLNOP1,RAVLNOP1                                                
         BZ    DATAR10                                                          
         OC    RAVLNOP2,RAVLNOP2                                                
         BNZ   DATAR10                                                          
         MVC   RAVLNOP2,INVFBK                                                  
         DROP  RE                                                               
         SPACE                                                                  
DATAR10  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         B     DATAR15                                                          
         SPACE 2                                                                
* CODE FOR MISSING UPGRADE INPUT.  FORCE UPGRADE TO GET HPT DATA                
* FOR TO DAY/TIME AND SOURCE/BOOK.                                              
*                                                                               
DATAR12  DS    0H                                                               
         CLC   INVCODE,=C'PJ'      CODE PJ REQUIRES UPGRADE                     
         BNE   *+12                                                             
         LA    R3,MISINP                                                        
         B     ERROR                                                            
*                                                                               
         XC    WORK,WORK           BUILD FORCED UPGRADE ELEMENT                 
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVC   RAVLNOP1,=H'100'                                                 
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         B     DATAR15                                                          
         DROP  RE                                                               
         SPACE                                                                  
* CALL DEMUP AND EXIT ROUTINE                                                   
*                                                                               
DATAR15  DS    0H                                                               
         MVI   BYTE2,0                                                          
         CLI   INVTYP,C'I'         TEST FOR INV TO INV TRANSFER                 
         BNE   *+8                                                              
         MVI   BYTE2,C'I'                                                       
*                                  BECAUSE OF UT/TP PROBLEM                     
         NI    11(R4),X'FF'-X'40'                                               
         GOTO1 VDEMUP,DMCB,(BYTE2,34(R4)),WORK,(R7)                             
         OI    11(R4),X'40'                                                     
         B     EXXMOD                                                           
         EJECT                                                                  
* ROUTINE TO GET ADDRESS OF MASTER DISPLACEMENT TABLE AND TO                    
* SET POINTERS TO RATINGS AND IMPS IN THE IUN RECORD SECTION                    
* OF TABLE                                                                      
*                                                                               
GETAD    NTR1                                                                   
         MVC   TABLIST,TABLISTL                                                 
         GOTO1 CDEMADDR,DMCB,(X'FF',TABLIST),ACOMFACS                           
         L     R1,ADISPTAB                                                      
         USING DSPHDRD,R1                                                       
         SPACE 1                                                                
GETAD2   DS    0H                                                               
         OC    DSPFILE(2),DSPFILE  TEST FOR END OF TABLE                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   DSPFILE,C'I'        TEST FOR IUN RECORD                          
         BE    GETAD3                                                           
         ICM   RF,7,DSPAET                                                      
         LA    R1,1(RF,R1)         POINT TO NEXT HEADER                         
         B     GETAD2                                                           
*                                                                               
GETAD3   CLC   DSPSBOOK,=X'B0F4'   BOOK BASE DISP TABLE                         
         BE    GETAD4                                                           
         ICM   RF,7,DSPAET                                                      
         LA    R1,1(RF,R1)         POINT TO NEXT HEADER                         
         B     GETAD2                                                           
         SPACE 1                                                                
GETAD4   DS    0H                                                               
         LR    RE,R1               SAVE DISP TABLE START                        
         SR    RF,RF                                                            
         ICM   RF,7,DSPAET         LENGTH OF HEADER PLUS ENTRIES                
         LA    R1,DSPHDRLN(R1)     POINT TO FIRST ENTRY                         
         ST    R1,ASDISP           SAVE ITS ADDRESS                             
         AR    RF,RE                                                            
         ST    RF,AEDISP           END OF TABLE                                 
         SR    RF,R1               TOTAL LENGTH OF ENTRIES                      
         SR    RE,RE                                                            
         D     RE,=F'5'            DIVIDE BY LENGTH OF EACH ENTRY               
         STH   RF,IUNCELLS         NUMVALS FOR IUN RECORD                       
         SPACE 1                                                                
         LR    R0,RF               SET R0 AS FIELD COUNTER                      
         L     R1,ASDISP                                                        
         USING DSPDTAD,R1                                                       
         SPACE 1                                                                
GETAD5   DS    0H                                                               
         CLI   DSPMOD,C'R'         TEST FOR RATINGS                             
         BNE   GETAD6              NEXT ENTRY                                   
         OC    ARSTART,ARSTART     TEST FOR FIRST RATING                        
         BNZ   *+8                                                              
         ST    R1,ARSTART                                                       
         ST    R1,AREND            UPDATE END POINTER                           
         B     GETAD7                                                           
         SPACE 1                                                                
GETAD6   DS    0H                                                               
         CLI   DSPMOD,C'T'         TEST FOR IMPS                                
         BNE   GETAD7              NEXT ENTRY                                   
         OC    ATSTART,ATSTART     TEST FOR FIRST IMP                           
         BNZ   *+8                                                              
         ST    R1,ATSTART                                                       
         ST    R1,ATEND                                                         
         SPACE 1                                                                
GETAD7   DS    0H                                                               
         LA    R1,5(R1)            NEXT ENTRY                                   
         BCT   R0,GETAD5                                                        
         SPACE 1                                                                
GETAD8   DS    0H                                                               
         L     R1,AREND            SET POINTER PAST LAST RATING                 
         LA    R1,5(R1)                                                         
         ST    R1,AREND                                                         
*                                                                               
         L     R1,ATEND            SET POINTER PAST LAST IMP                    
         LA    R1,5(R1)                                                         
         ST    R1,ATEND                                                         
*                                                                               
         LA    R1,ARSTART                                                       
         CLI   RATIMP,C'R'         TEST FOR RATINGS                             
         BE    *+8                                                              
         LA    R1,ATSTART          POINTER TO START OF IMPS                     
         LM    RE,RF,0(R1)         START AND END POINTERS                       
*                                                                               
         SR    RF,RE               LENGTH OF TABLE SECTION                      
         SR    RE,RE                                                            
         D     RE,=F'5'            FIND NUMBER OF RATINGS OR IMPS               
         STC   RF,BASCELLS         SAVE COUNT OF BASIC CELLS                    
         B     EXXMOD                                                           
         DROP  R1                                                               
         EJECT                                                                  
* ROUTINE TO GET RECORD VALUES - OUTPUT TO OLDRAT, OLDIMP,                      
* OLDSHR, AND OLDHUT                                                            
*                                                                               
GETDEM   NTR1                                                                   
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         ST    R7,DBCOMFCS                                                      
         MVC   DBAREC,AIOAREA                                                   
         L     R1,DBAREC                                                        
         LA    R1,34(R1)           POINT TO FIRST ELEMENT POSITION              
         ST    R1,DBAQUART                                                      
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
         SPACE 1                                                                
         GOTO1 CDEMOUT,DMCB,(C'M',ARSTART),DBLOCK,OLDRAT,AREND                  
         SPACE 1                                                                
         GOTO1 (RF),(R1),(C'M',ATSTART),DBLOCK,OLDIMP,ATEND                     
         SPACE 1                                                                
         GOTO1 (RF),(R1),(C'M',SHARE),DBLOCK,OLDSHR,SHRHUTX                     
         B     EXXMOD                                                           
         EJECT                                                                  
* ROUTINE TO DISPLAY CELL NAMES ON SCREEN                                       
*                                                                               
DCAT     NTR1                                                                   
         MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         MVC   VDEMOCON,DMCB                                                    
         SPACE 1                                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         ST    R7,DBCOMFCS                                                      
         MVI   DBSELMED,C'U'                                                    
         SPACE 1                                                                
         LA    R2,ESTNFSTH         R2 TO FIRST FIELD                            
         LA    R3,ESTNLSTH         R3 POINTS TO LAST FIELD                      
         L     R5,ARSTART                                                       
         CLI   RATIMP,C'R'         TEST FOR RATINGS                             
         BE    *+8                                                              
         L     R5,ATSTART          POINTER TO START OF IMPS                     
         ZIC   R6,BASCELLS         COUNT OF BASIC CELLS                         
         XC    THREE,THREE                                                      
         SPACE 1                                                                
DCAT2    DS    0H                                                               
         MVC   THREE+1(2),0(R5)                                                 
         GOTO1 VDEMOCON,DMCB,THREE,(6,8(R2)),DBLOCK                             
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT FIELD                          
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BZ    *-12                                                             
         CR    R2,R3               TEST FOR END OF SCREEN                       
         BH    DCAT4                                                            
         LA    R5,5(R5)            NEXT CELL IN TABLE                           
         BCT   R6,DCAT2                                                         
         SPACE 1                                                                
DCAT4    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
* ROUTINE TO DISPLAY DEMO VALUES ON SCREEN                                      
*                                                                               
DISVAL   NTR1                                                                   
         GOTO1 VFOUTBLK,DMCB,ESTDFSTH,ESTLAST                                   
         LA    R3,OLDRAT                                                        
         L     R5,ARSTART                                                       
         CLI   RATIMP,C'R'         TEST FOR RATINGS                             
         BE    *+12                                                             
         LA    R3,OLDIMP           NO-PUT IMPS OUT INSTEAD                      
         L     R5,ATSTART                                                       
         ZIC   R6,BASCELLS         COUNTER                                      
         LA    R2,ESTDFSTH         POINT TO FIRST UNPROTECTED FIELD             
         MVI   BYTE,NO             SHARE/HUT SWITCH OFF                         
         SPACE 1                                                                
DISV2    DS    0H                                                               
         MVC   FULL,0(R3)                                                       
*                                                                               
DISV4    DS    0H                                                               
         EDIT  (B4,FULL),(7,8(R2)),1,ALIGN=LEFT,ZERO=BLANK                      
         OI    4(R2),X'20'         TURN ON VALID BIT                            
         SPACE 1                                                                
         GOTO1 DEELEM,DMCB,(R5)    LOOK FOR 'DE' ELEM                           
         BZ    DISV6               NOT FOUND                                    
         MVC   DUB(6),8(R2)        INSERT A STAR BEFORE VALUE                   
         MVI   8(R2),C'*'          TO INDICATE OVERRIDE                         
         MVC   9(5,R2),DUB                                                      
         SPACE 1                                                                
DISV6    DS    0H                                                               
         LA    R3,4(R3)            NEXT VALUE                                   
         LA    R5,5(R5)            NEXT DISP TABLE ENTRY                        
         ZIC   R0,0(R2)            FIND NEXT UNPROTECTED FIELD                  
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *-10                                                             
         BCT   R6,DISV2                                                         
         SPACE 1                                                                
DISV8    DS    0H                                                               
         CLI   BYTE,YES            EXIT AFTER SHARE/HUT DONE                    
         BE    EXXMOD                                                           
         LA    R6,2                RESET COUNTER                                
         LA    R3,OLDSHR           DISPLAY SHARE AND HUT                        
         LA    R5,SHARE                                                         
         LA    R2,ESTSHRH                                                       
         MVI   BYTE,YES                                                         
         B     DISV2                                                            
         EJECT                                                                  
* ROUTINE TO EDIT INPUT VALUES ON SCREEN AND PLACE IN WORK AREA                 
*                                                                               
GETVAL   NTR1                                                                   
         LA    R2,ESTDFSTH         POINT TO FIRST VALUE FIELD                   
         ZIC   R6,BASCELLS         COUNTER                                      
         LA    R4,NEWRAT           R4 POINTS TO OUTPUT AREA                     
         L     R5,ARSTART          R5 POINTS TO MASTER DISP TABLE               
         CLI   RATIMP,C'R'         TEST FOR RATINGS OR IMPS                     
         BE    *+12                                                             
         LA    R4,NEWIMP                                                        
         L     R5,ATSTART                                                       
         MVI   BYTE,NO             SHARE/HT EDIT SW IS OFF                      
         SPACE 1                                                                
GETV2    DS    0H                                                               
         CLI   5(R2),0             TEST FOR NO INPUT                            
         BE    GETV4                                                            
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         LA    R3,8(R2)            R3 POINTS AT DATA                            
         SPACE 1                                                                
         CLI   0(R3),C'*'                                                       
         BNE   GETV3                                                            
         SH    R1,=H'1'            STRIP OUT LEADING STAR                       
         BZ    GETV4               IGNORE FIELD W ONLY A STAR                   
         STC   R1,5(R2)            ADJUST FIELD LENGTH                          
         MVC   0(7,R3),1(R3)       SHIFT DATA OVER                              
         MVI   7(R3),C' '                                                       
*                                                                               
GETV3    DS    0H                                                               
         ZIC   RF,5(R2)                                                         
         ST    RF,DMCB+4                                                        
         GOTO1 VCASHVAL,DMCB,(1,(R3))                                           
         CLI   DMCB,X'FF'                                                       
         BE    GETVERR                                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GETV4    DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *-10                                                             
         LA    R4,4(R4)            NEXT OUTPUT VALUE                            
         LA    R5,5(R5)            BUMP TO NEXT MAST DISP TAB ENTRY             
         BCT   R6,GETV2                                                         
         SPACE 1                                                                
GETV5    DS    0H                                                               
         CLI   BYTE,YES            SHARE/HUT DONE YET                           
         BE    EXXMOD                                                           
         LA    R2,ESTSHRH                                                       
         LA    R6,2                                                             
         LA    R4,NEWSHR                                                        
         LA    R5,SHARE                                                         
         MVI   BYTE,YES            GO DO SHARE AND HUT                          
         B     GETV2                                                            
         SPACE 1                                                                
GETVERR  DS    0H                                                               
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         EJECT                                                                  
* ROUTINE TO COMPARE SCREEN INPUT AGAINST RECORD VALUES AND ADD                 
* OVERRIDE ELEMENTS FOR DIFFERENCES.  FOR THE RECALCULATION OPTION,             
* THE CORRESPONING RATING OR IMP VALUE WILL BE ADJUSTED BY THE                  
* RATIO OF THE INPUT OVER THE RECORD VALUE AND AN APPROPRAITE                   
* OVERRIDE ELEMENT ADDED.                                                       
*                                                                               
GETOVER  NTR1                                                                   
         ZIC   R2,BASCELLS         COUNTER OF CELLS                             
         LA    R3,OLDIMP           R3 POINTS TO RECORD VALUES                   
         LA    R4,NEWIMP           R4 POINTS TO INPUT                           
         L     R5,ATSTART          R5 POINTS TO DEMO EXPRESSIONS                
         CLI   RATIMP,C'T'         TEST FOR IMPS                                
         BE    *+16                                                             
         LA    R3,OLDRAT                                                        
         LA    R4,NEWRAT                                                        
         L     R5,ARSTART                                                       
         MVI   BYTE,NO             SHARE/HUT SWITCH OFF                         
         SPACE 1                                                                
* DELETE AN OLD ELEMENT BEFORE ADDING OVERRIDE EL FOR INPUT VALUE               
*                                                                               
GETOV2   DS    0H                                                               
         CLC   0(4,R4),0(R3)       INPUT VALUE AGAINST RECORD VALUE             
         BE    GETOV4                                                           
         OC    0(4,R4),0(R4)                                                    
         BZ    GETOV4                                                           
         SPACE 1                                                                
         GOTO1 DEELEM,DMCB,(R5)    LOOK FOR 'DE' ELEM                           
         BZ    GETOV2A                                                          
*                                                                               
*  IF ELEMENT IS FOUND, THE ADDITIONAL SEARCH ARGUMENT (P3) IS                  
*    SET IN THE 'DEELEM' ROUTINE FOR THE 'HELLO' CALL                           
*                                                                               
         GOTO1 CHELLO,DMCB,(C'D',=C'REPFILE '),(X'DE',AIOAREA)                  
         LR    R0,R4               SAVE R4                                      
         L     R4,AIOAREA          POINT AT RECORD                              
         SR    R1,R1               ADJUST RECORD LENGTH                         
         ICM   R1,3,RINVLEN                                                     
         BCTR  R1,0                                                             
         STCM  R1,3,RINVLEN                                                     
         LR    R4,R0               RESTORE R4                                   
         SPACE 1                                                                
         USING RINVOEL,R6                                                       
GETOV2A  EQU   *                                                                
         XC    SCRATCH,SCRATCH     NOW ADD OVERRIDE ELEMENT                     
         LA    R6,SCRATCH                                                       
         XC    0(12,R6),0(R6)                                                   
         MVC   0(2,R6),=X'DE0C'    INSERT ELEMENT CODE/LENGTH                   
         MVC   4(2,R6),0(R5)       TYPE AND CATEGORY                            
         MVI   7(R6),X'81'         FOR RATINGS: DECIMAL PRECISION               
         CLI   4(R6),C'R'          IS DEMO A RATING?                            
         BE    GETOV2B             YES                                          
         CLI   4(R6),C'P'          IS DEMO A RATING?                            
         BE    GETOV2B             YES                                          
         CLI   4(R6),C'S'          IS DEMO A RATING?                            
         BE    GETOV2B             YES                                          
         MVI   7(R6),X'42'         FOR IMPS: HUNDREDS                           
GETOV2B  EQU   *                                                                
         MVC   10(2,R6),2(R4)      SCREEN VALUE                                 
         GOTO1 VADDELEM,DMCB,AIOAREA,(R6)                                       
         SPACE 1                                                                
* RECALCULATE CORRESPONDING RATING/IMP CELL AND ADD OVERRIDE FOR IT             
*                                                                               
GETOV3   DS    0H                                                               
         CLI   RECALC,NO           TEST FOR RECALC                              
         BE    GETOV4                                                           
         CLI   BYTE,YES            TEST FOR SHARE/HUT                           
         BE    GETOV4                                                           
         BAS   RE,GETDEMO                                                       
         BAS   RE,GETINDEX         RATIO OF INPUT TO RECORD VALUE               
         BAS   RE,CALC             DERIVE CORRESPONDING RAT/IMP                 
         SPACE 1                                                                
         PRINT GEN                                                              
         GOTO1 DEELEM,DMCB,THREE+1        LOOK FOR 'DE' ELEM                    
         PRINT NOGEN                                                            
         BZ    GETOV3A                                                          
*                                                                               
*  IF ELEMENT IS FOUND, THE ADDITIONAL SEARCH ARGUMENT (P3) IS                  
*    SET IN THE 'DEELEM' ROUTINE FOR THE 'HELLO' CALL                           
*                                                                               
         GOTO1 CHELLO,DMCB,(C'D',=C'REPFILE '),(X'DE',AIOAREA)                  
         LR    R0,R4               SAVE R4                                      
         L     R4,AIOAREA          POINT AT RECORD                              
         SR    R1,R1               ADJUST RECORD LENGTH                         
         ICM   R1,3,RINVLEN                                                     
         BCTR  R1,0                                                             
         STCM  R1,3,RINVLEN                                                     
         LR    R4,R0               RESTORE R4                                   
         SPACE 1                                                                
GETOV3A  EQU   *                                                                
         MVC   SCRATCH+4(2),THREE+1 MODIFIER AND DEMO                           
         XC    SCRATCH+8(4),SCRATCH+8                                           
         MVC   SCRATCH+10(2),HALF  CALCULATED VALUE                             
         GOTO1 VADDELEM,DMCB,AIOAREA,SCRATCH                                    
         DROP  R6                                                               
         SPACE 1                                                                
GETOV4   DS    0H                                                               
         LA    R3,4(R3)            NEXT RECORD VALUE                            
         LA    R4,4(R4)            NEXT INPUT VALUE                             
         LA    R5,5(R5)            NEXT MAST DISP ENTRY                         
         BCT   R2,GETOV2                                                        
         SPACE 1                                                                
         CLI   BYTE,YES            SHARE/HUT DONE YET                           
         BE    EXXMOD                                                           
         LA    R2,2                                                             
         LA    R3,OLDSHR                                                        
         LA    R4,NEWSHR                                                        
         LA    R5,SHARE                                                         
         MVI   BYTE,YES                                                         
         B     GETOV2                                                           
         EJECT                                                                  
* ROUTINE TO FIND CORRESPONDING DEMO AND VALUE                                  
* AT ENTRY R5 POINTS TO MASTER DISPLACEMENT TABLE - DEMO CODE PLACED            
* IN THREE AND VALUE AT FULL ON EXIT.                                           
*                                                                               
GETDEMO  DS    0H                                                               
         XC    THREE,THREE                                                      
         MVC   THREE+1(2),0(R5)    EXTRACT DEMO CODE                            
         MVI   THREE+1,C'R'        AND SUBSTITUTE CORRESPONDING                 
         L     R0,ATSTART                                                       
         LA    RF,OLDRAT                                                        
         CLI   RATIMP,C'T'         MODIFIER                                     
         BE    *+16                                                             
         MVI   THREE+1,C'T'                                                     
         L     R0,ARSTART                                                       
         LA    RF,OLDIMP                                                        
*                                                                               
         LR    R1,R5                                                            
         SR    R1,R0               DISPLACEMENT INTO MAST DISP TAB              
         SR    R0,R0                                                            
         D     R0,=F'5'            INTEGER INDEX INTO TABLE                     
         SLL   R1,2                INDEX INTO VALUES                            
         AR    RF,R1               POINT TO VALUE                               
         MVC   FULL,0(RF)          EXTRACT VALUE                                
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO COMPUTE INDEX OF INPUT/RECORD VALUE                                
* AT ENTRY R3 POINTS TO RECORD VALUE AND R4 TO INPUT                            
*                                                                               
GETINDEX DS    0H                                                               
         L     R1,0(R4)                                                         
         L     R0,0(R3)                                                         
         XC    INDEX,INDEX                                                      
         MH    R1,=H'100'         <<<<<<<<<<                                    
         LTR   RF,R0                                                            
         BZR   RE                                                               
         SRL   R0,1                HALVE DIVISOR                                
         AR    R0,R1                                                            
         SRDA  R0,32                                                            
         DR    R0,RF                                                            
         ST    R1,INDEX                                                         
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO MULTIPLY INDEX BY CORRESPONDING VALUE (AT FULL)                    
*                                                                               
CALC     DS    0H                                                               
         L     R1,FULL                                                          
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         M     R0,INDEX                                                         
         A     R1,=F'50000'                                                     
         D     R0,=F'100000'                                                    
         M     R0,=F'10'                                                        
         STH   R1,HALF                                                          
         BR    RE                                                               
         EJECT                                                                  
*              GET A HEADER RECORD                                              
         SPACE 1                                                                
HEADER   NTR1                                                                   
         MVC   KEY,RINVREC                                                      
         XC    KEY+24(3),KEY+24    RINVKSRC/RINVKTXT                            
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   *+12                NOT FOUND                                    
         BAS   RE,GETREC           FOUND A MATCH                                
         B     HEADREC2                                                         
         SPACE 1                                                                
         LA    R3,ERRNF                                                         
         OC    RINVKSTD,RINVKSTD                                                
         BNZ   ERROR               INPUT A DATE - RECORD NOT FOUND              
         SPACE 1                                                                
         CLC   KEYSAVE(21),KEY     DID I FIND INVENTORY NUMBER                  
         BNE   ERROR                                                            
         SPACE 1                                                                
         OC    KEY+24(3),KEY+24    MAKE SURE IT'S HEADER                        
         BZ    *+6                                                              
         DC    H'0'                PROBLEM WITH FILE                            
         SPACE 1                                                                
HEADREC1 BAS   RE,GETREC                                                        
         L     R4,AIOAREA                                                       
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    HEADREC2            TAKE ONE WITH NO END                         
         SPACE 1                                                                
         BAS   RE,SEQ              IF ALL HAVE END TAKE HIGHEST                 
         CLC   KEYSAVE(21),KEY                                                  
         BNE   HEADREC2                                                         
         OC    KEY+24(3),KEY+24                                                 
         BZ    HEADREC1                                                         
         B     *-24                NOT A HEADER                                 
         SPACE 1                                                                
HEADREC2 L     R4,AIOAREA                                                       
         MVC   INVEFF,RINVPEFF     SAVE START DATE                              
         MVC   INVDAY(5),RINVPDAY  SAVE DAY AND TIME                            
         B     EXXMOD                                                           
         EJECT                                                                  
*              EDIT INVENTORY NUMBER                                            
         SPACE 1                                                                
INVEDT   DS    0H                                                               
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
         BR    R5                                                               
         SPACE 2                                                                
*              EDIT DATE                                                        
         SPACE 1                                                                
DATEDT   DS    0H                                                               
         GOTO1 VDATVAL,DMCB,(0,12(R6)),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(3,RINVKSTD)                               
         BR    R5                                                               
         EJECT                                                                  
*              ADD THE RECORD TO FILE                                           
         SPACE 1                                                                
FLADD    NTR1                                                                   
         USING RINVAEL,R5                                                       
         MVC   KEY,RINVREC                                                      
         SPACE 1                                                                
         LA    R5,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   RINVACOD(2),=X'EF0C'                                             
         MVC   RINVAFST,TODAY                                                   
         MVC   RINVALST,TODAY                                                   
         MVI   RINVAWHY,C'A'                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    FLPUT                                                            
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,AIOAREA,(R5)                                       
         BAS   RE,ADDREC           ADD THE RECORD                               
         MVC   BSVDA,KEY                                                        
         NI    DMINBTS,X'F7'       TURN OFF PASS DELETES                        
         B     EXXMOD                                                           
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
         L     R5,DMCB+8                                                        
         CLI   DMCB,X'FF'                                                       
         BNE   *+8                                                              
         LA    R5,WORK                                                          
         MVC   RINVALST,TODAY                                                   
         MVI   RINVAWHY,C'C'                                                    
         SPACE 1                                                                
         LA    RE,REC                                                           
         ST    RE,AIOAREA                                                       
         SPACE 1                                                                
         GOTO1 VDELELEM,DMCB,(X'EF',AIOAREA)                                    
         GOTO1 VADDELEM,DMCB,AIOAREA,(R5)                                       
         BAS   RE,PUTREC           WRITE BACK THE NEW                           
         NI    DMINBTS,X'F7'                                                    
         MVC   BSVDA,KEY+28                                                     
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
         L     R4,AIOAREA          A(RECORD)                                    
*                                                                               
*    R4 IS COVERED BY RINVD FROM AN EARLIER USING                               
*                                                                               
         LR    R3,R4               CALCULATE END OF RECORD                      
         ZICM  RF,RINVLEN,2        RECORD LENGTH                                
         AR    R3,RF               A(END OF RECORD)                             
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
         CLC   0(2,R8),2(R2)       YES - CHECK DEMO CODE                        
         BNE   DEEL0004            NOT CORRECT DEMO                             
         XC    DUB,DUB             SET UP FOR POSSIBLE DELETE                   
         MVC   DUB(2),0(R8)        DEMO MOD/CODE                                
         LA    RF,DUB              A(ADDITIONAL SEARCH ARGUMENT)                
         ST    RF,DMCB+8           A(DUB) IN P3                                 
         MVI   DMCB+8,2            L(ADDITIONAL SEARCH ARGUMENT)                
         B     DEEL0098            EXIT WITH 'FOUND'                            
DEEL0010 EQU   *                   NEW STYLE ELEMENT                            
         CLI   1(R2),12            LENGTH OF ELEMENT = 12 (NEW STYLE?)          
         BNE   DEEL0004            NO - UNKNOWN LENGTH - SKIP IT                
         CLC   0(2,R8),4(R2)       CHECK DEMO CODE                              
         BNE   DEEL0004            NOT CORRECT DEMO                             
         XC    DUB,DUB             SET UP FOR POSSIBLE DELETE                   
         MVC   DUB+2(2),0(R8)      DEMO MOD/CODE                                
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
*                                                                               
*  SCRATCH WORK SPACE FOR NEW ELEMENTS                                          
*                                                                               
SCRATCH  DS    CL32                                                             
         EJECT                                                                  
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
SHARE    DC    C'S',X'01',3X'00'                                                
HUT      DC    C'P',X'01',3X'00'                                                
SHRHUTX  DC    X'FF'                                                            
         SPACE 1                                                                
TABLISTL DC    X'D0',3X'00',X'FF'                                               
OFORMAT  DC    C'INVUIUN',X'530B00'                                             
         EJECT                                                                  
       ++INCLUDE RELFMGEN                                                       
         EJECT                                                                  
       ++INCLUDE RELFMTWA                                                       
       ++INCLUDE RELFME8D                                                       
         EJECT                                                                  
* LOCAL WORKING STORAGE                                                         
*                                                                               
ESTD     DSECT                                                                  
         DS    0F                                                               
TABLIST  DS    0XL5                                                             
ADISPTAB DS    A                                                                
         DS    XL1                                                              
ASDISP   DS    A                                                                
AEDISP   DS    A                                                                
ARSTART  DS    A                                                                
AREND    DS    A                                                                
ATSTART  DS    A                                                                
ATEND    DS    A                                                                
INDEX    DS    F                                                                
THREE    DS    CL3                                                              
RATIMP   DS    C                                                                
RECALC   DS    C                                                                
BASCELLS DS    X                                                                
IUNCELLS DS    H                                                                
         SPACE 2                                                                
* DEMOMATH BLOCK                                                                
*                                                                               
MATHFACS DS    0F                                                               
MATHDBLK DS    A                   A(DBLOCK)                                    
MATHFCTR DS    F                   FACTOR                                       
MATHIFIL DS    CL3                 INPUT RECORD FILE                            
MATHOFIL DS    CL3                 OUTPUT RECORD FILE                           
MATHOSRC DS    CL3                 OUTPUT RECORD SOURCE                         
MATHFACL EQU   *-MATHFACS                                                       
         SPACE 2                                                                
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
OLDRAT   DS    32F                                                              
OLDIMP   DS    32F                                                              
OLDSHR   DS    F                                                                
OLDHUT   DS    F                                                                
NEWRAT   DS    32F                                                              
NEWIMP   DS    32F                                                              
NEWSHR   DS    F                                                                
NEWHUT   DS    F                                                                
ESTDX    EQU   *                                                                
         SPACE 2                                                                
* DSECT TO COVER SAVED INPUT DATA                                               
*                                                                               
SAVED    DSECT                                                                  
SVFBK    DS    XL3                 FROM BOOK                                    
SVTOBK   DS    XL3                 TO BOOK                                      
SVTYPE   DS    C                                                                
SVCODE   DS    CL2                                                              
SVFROM   DS    CL30                FROM DETAILS IN CHARACTER FORM               
SVUP     DS    CL17                UPGRADE INPUT                                
SVFBT    DS    CL1                 BOOK TYPE                                    
         SPACE 2                                                                
* DSECT TO COVER INVENTORY LIST ENTRY                                           
*                                                                               
INVLD    DSECT                                                                  
INVLREC  DS    0CL10                                                            
INVLFLE  DS    C                   P=PAV, I=INVENTORY                           
INVLTYP  DS    C                   X'80'  INVENTORY NUMBER                      
*                                  X'40'  FIRST IN DAY/TIME EXP.                
*                                  X'20'  LAST IN DAY/TIME EXP.                 
*                                  X'08'  ADD EXPRESSION                        
INVLWT   DS    CL1                 WEIGHT (BINARY)                              
INVLDATA DS    0CL6                                                             
INVLSTIM DS    CL2                 START TIME                                   
INVLETIM DS    CL2                 END TIME                                     
INVLDAY  DS    CL1                 DAY                                          
         DS    CL1                 SPARE                                        
         ORG   INVLDATA                                                         
INVLNUMB DS    CL3                 NUMBER                                       
INVLDATE DS    CL3                 START DATE (Y/M/D BINARY)                    
         DS    CL1                 SPARE                                        
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MISINP   EQU   1                                                                
PRO      EQU   X'01'                                                            
INV      EQU   X'02'                                                            
TP       EQU   X'04'               READ FROM TIME PERIOD FILE                   
MIX      EQU   X'08'               READ FROM PAV AND TIME PERIOD                
COMMA    EQU   C','                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
         EJECT                                                                  
* REGENINV                                                                      
         PRINT OFF                                                              
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* REGENAVL                                                                      
         PRINT OFF                                                              
RAVLD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBEXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DEDBEXTRAD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047RERMP14   05/01/02'                                      
         END                                                                    
