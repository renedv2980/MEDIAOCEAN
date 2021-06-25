*          DATA SET RELFM07    AT LEVEL 084 AS OF 05/01/02                      
*PHASE T80407A,+0                                                               
         TITLE 'T80407 - REPPAK FILE MAINT - INVENTORY TRANSFER'                
*                                                                               
*- RELFM07 - PHASE T80407                                                       
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  08/24/89  PJS  CHANGED PHASE CARD TO 'A' LEVEL                               
*                                                                               
T80407   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80407                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T80407+4096,R9                                                   
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         EJECT                                                                  
*              GET THE MARKET RECORD                                            
         SPACE 1                                                                
         LA    R2,LFMKEYH                                                       
         FOUT  TRNMRKTH,SPACES,25                                               
         SPACE 1                                                                
         GOTO1 VPAVSTA,DMCB,8(R2),WORK   GET STATION AND MEDIA                  
         MVC   INVSTA,WORK                                                      
         MVC   INVMED,WORK+5                                                    
         SPACE 1                                                                
         GOTO1 VMKTNME,DMCB,WORK,TRNMRKTH                                       
         MVC   DEMEDIA(8),INVMED   MEDIA/STATION/MARKET  FOR DEMOS              
         EJECT                                                                  
*              BUILD INVENTORY RECORD KEY                                       
         SPACE 1                                                                
         USING RINVD,R4                                                         
         LA    R4,INVKLAST                                                      
         XC    RINVREC(34),RINVREC                                              
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,REPALPHA                                                
         MVC   RINVKSTA,INVSTA                                                  
         SPACE 1                                                                
         LA    R2,TRNFRBKH         FROM BOOK                                    
         BAS   RE,ANY                                                           
         LA    R3,INVERR                                                        
         GOTO1 VBOOKVAL,DMCB,(R2),(1,INVSRC),(C'B',CSCANNER),INVBTYPE           
         CLI   DMCB+4,1                                                         
         BNE   ERROR                                                            
         SPACE 1                                                                
         SPACE 1                                                                
         MVC   INVTOBK(3),INVSRC   TO BOOK                                      
         LA    R2,TRNTOBKH                                                      
         BAS   RE,ANY                                                           
         LA    R3,INVERR                                                        
         GOTO1 VBOOKVAL,DMCB,(R2),(1,WORK),CSCANNER                             
         CLI   DMCB+4,1                                                         
         BNE   ERROR                                                            
         SPACE 1                                                                
         CLC   WORK(3),=C'SAM'                                                  
         BE    *+10                                                             
         MVC   INVTOBK(3),WORK                                                  
         SPACE 1                                                                
         TM    LFMKEYH+4,X'20'                                                  
         BNO   INV150                                                           
         TM    TRNFRBKH+4,X'20'                                                 
         BNO   INV150                                                           
         TM    TRNTOBKH+4,X'20'                                                 
         BO    INV200                                                           
         SPACE 1                                                                
* FOR CHANGE IN KEY, FROM BOOK, OR TO BOOK, TURN OFF ALL THE                    
* THE VALID BITS ON SCREEN.                                                     
*                                                                               
INV150   LA    R2,TRNINV1H                                                      
         LA    RF,TRNLAST                                                       
INV151   CR    R2,RF                                                            
         BNL   INV200                                                           
         NI    4(R2),X'DF'                                                      
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     INV151                                                           
         EJECT                                                                  
INV200   GOTO1 VCALLOV,DMCB,(X'17',0),(RA)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VT80417,DMCB        A(T80417)                                    
         OI    LFMKEYH+4,X'20'                                                  
         SPACE 1                                                                
         LA    R2,TRNINV1H                                                      
         BAS   RE,ANY              SOME INPUT MUST BE FIRST FIELD               
         SPACE 1                                                                
         LA    R3,INVERR                                                        
         OI    TRNFRBKH+4,X'20'                                                 
         OI    TRNTOBKH+4,X'20'                                                 
INV203   XR    R1,R1               IF TO INV./CODE/UPGRADE/TYPE/                
         LR    R5,R2               CHANGED RE-DO TRANSFER                       
         LA    R8,4                                                             
         SPACE 1                                                                
INV205   IC    R1,0(R5)                                                         
         AR    R5,R1                                                            
         TM    4(R5),X'20'                                                      
         BO    *+8                                                              
         NI    4(R2),X'DF'         DATA CHANGED REVALID LINE                    
         BCT   R8,INV205                                                        
         SPACE 1                                                                
         TM    4(R2),X'20'                                                      
         BO    *+16                                                             
         BAS   RE,DATAREC                                                       
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         SPACE                                                                  
         LA    RF,TRNLAST          A(END OF SCREEN)                             
         LA    R2,LINELEN(R2)      POINT TO NEXT LINE                           
         CR    R2,RF                                                            
         BNL   EXXMOD              ALL DONE WITH EDIT                           
         CLI   5(R2),0                                                          
         BNE   INV203                                                           
         B     EXXMOD                                                           
         EJECT                                                                  
*              EDIT SCREEN LINE AND BUILD DATA RECORD                           
*                                                                               
*              AT ENTRY R2 POINTS TO LINE START AND R4 POINTS TO REC            
*                                                                               
DATAREC  NTR1                                                                   
         MVI   TRFNOVER,0   RESET TIME PERIOD FOOTNOTE SUPPRESS SWITCH          
         ST    R2,THISLINE         SAVE LINE START                              
         LR    RE,R2                                                            
         LA    RE,LINELEN(RE)                                                   
         SR    R1,R1                                                            
         SPACE                                                                  
DATAR1   NI    4(R2),X'DF'         TURN OFF ALL PREVIOUSLY VALID                
         IC    R1,0(R2)            BITS ON LINE                                 
         AR    R2,R1                                                            
         CR    R2,RE                                                            
         BL    DATAR1                                                           
         SPACE                                                                  
DATAR2   EQU   *                   EDIT CODE FIELD                              
         L     R2,THISLINE                                                      
         LA    R2,CODEH(R2)                                                     
         LA    R3,INVERR                                                        
         BAS   RE,MOVE                                                          
         LA    RE,CODETAB                                                       
         LA    R1,CODES                                                         
         CLC   WORK(2),0(RE)                                                    
         BE    DATAR3                                                           
         LA    RE,L'CODETAB(RE)                                                 
         BCT   R1,*-14                                                          
         LA    RE,MONTAB           NOT A CODE-LOOK FOR A MONTH/YR               
         LA    R1,MONTHS                                                        
         CLC   WORK(1),0(RE)       LOOK FOR A MONTH CODE                        
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-14                                                          
         B     ERROR                                                            
         SPACE 1                                                                
         CLI   WORK+1,C'0'         NOW LOOK FOR NUMBER                          
         BL    ERROR                                                            
         CLI   WORK+1,C'9'                                                      
         BH    ERROR                                                            
         MVC   INVCODE,WORK                                                     
         MVI   INVCDCTL,PRO+INV                                                 
         B     DATAR5                                                           
         SPACE                                                                  
DATAR3   EQU   *                                                                
         MVC   INVCODE,WORK                                                     
         MVC   INVCDCTL,2(RE)      CONTROL BITS                                 
         CLC   INVCODE,=C'TP'      FOR CODE 'TP' DO NOT ALLOW                   
         BNE   DATAR5              CODE FOR FROM BOOKS PRIOR TO OCT82           
         CLC   INVFBK,=X'520A'                                                  
         BL    ERROR                                                            
         SPACE                                                                  
DATAR5   EQU   *                                                                
         MVI   INVTYP,C'P'         DEFAULT-FROM DEMO FILES                      
         L     R2,THISLINE                                                      
         LA    R2,TYPEH(R2)                                                     
         CLI   5(R2),0                                                          
         BE    *+18                                                             
         CLI   5(R2),1             VALIDATE TYPE                                
         BNE   ERROR                                                            
         MVC   INVTYP,8(R2)                                                     
         CLI   INVTYP,C'I'         TEST FOR INVENTORY TRANSFER                  
         BE    DATAR6                                                           
         CLI   INVTYP,C'P'                                                      
         BNE   ERROR                                                            
         TM    INVCDCTL,PRO        CODE CONSISTENCY WITH 'P'                    
         BZ    ERROR                                                            
         SPACE 2                                                                
DATAR6   EQU   *                                                                
         L     R2,THISLINE                                                      
         LA    R2,FROMH(R2)                                                     
         MVI   INVNO,0                                                          
         CLI   5(R2),0                                                          
         BNE   DATAR8              DATA IN FROM FIELD                           
         CLI   INVTYP,C'I'                                                      
         BE    DATAR10             FOR BOOK TRANSFER                            
         CLC   INVCODE,=C'PR'      CODE 'PR' REQUIRES INPUT                     
         BNE   DATAR10             NO FROM DETAILS INPUT CODE                   
         LA    R3,MISINP                                                        
         B     ERROR                                                            
         SPACE                                                                  
DATAR8   DS    0H                  EDIT FROM FIELD                              
         XC    TRFIELD,TRFIELD     EXTRACT SCREEN INPUT TO DUMMY                
         MVI   TRFIELD,L'TRFIELD   HEADER AND FIELD                             
         ZIC   R1,5(R2)                                                         
         LR    R0,R1               SAVE DATA LENGTH                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRFIELD+8(0),8(R2)                                               
         STC   R0,TRFIELD+5        INPUT LENGTH                                 
*                                                                               
         LA    RE,WORK3                                                         
         LA    RE,2000(RE)         WORK3+2000 CONTAINS INV LIST                 
         ST    RE,INVLIST                                                       
         LA    R8,1000(RE)         WORK3+3000 IS STORAGE FOR VINVLST            
         SPACE                                                                  
         GOTO1 VINVLST,DMCB,(R2),(R8),(RC)                                      
         CLI   INVNO,0             TEST FOR ERROR IN INVLST                     
         BNE   DATAR10                                                          
         ZIC   R3,FERN             INSERT ERROR MESSAGE NUMBER                  
         B     ERROR                                                            
*                                                                               
         SPACE                                                                  
DATAR10  EQU   *                   EDIT TO FIELD                                
         L     R2,THISLINE                                                      
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         LA    R3,MISINP                                                        
         B     ERROR                                                            
         SPACE                                                                  
         LA    R6,WORK3                                                         
         LA    R6,3000(R6)         POINT R6 AT SPARE STORAGE                    
         LA    R4,INVKLAST                                                      
         XC    RINVKINV(10),RINVKINV    CLEAR INV NO/DATE/SRC.                  
         SPACE                                                                  
         GOTO1 CSCANNER,DMCB,(R2),(2,(R6))                                      
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         SPACE                                                                  
         BAS   R8,INVEDT           EDIT INVENTORY NUMBER                        
         CH    R5,=H'1'                                                         
         BE    *+12                NO DATE                                      
         LA    R6,32(R6)                                                        
         BAS   R8,DATEDT                                                        
         SPACE                                                                  
         BAS   RE,HEADER                                                        
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         MVC   INVKLAST,REC        KEY FROM HEADER                              
         SPACE 1                                                                
         CLI   INVNO,0             BUILD DUMMY LIST ENTRY IF NO FROM            
         BNE   DATAR12             DETAILS INPUT USING 'TO' HEADER              
         MVI   INVNO,1                                                          
         LA    RE,WORK3                                                         
         LA    RE,2000(RE)         WORK3+2000 CONTAINS INVLIST                  
         ST    RE,INVLIST          BUILD INVENTORY LIST ENTRY                   
         XC    TRFIELD,TRFIELD     BUILD DUMMY HEADER USING                     
         MVI   TRFIELD,L'TRFIELD   DEFAULT FROM DETAILS                         
         LA    R6,TRFIELD+8                                                     
         USING INVLD,RE                                                         
         XC    INVLREC,INVLREC                                                  
         MVI   INVLWT,1                                                         
         MVC   INVLFLE,INVTYP                                                   
         CLI   INVLFLE,C'I'                                                     
         BE    DATAR11                                                          
         OI    INVLTYP,X'60'       SET DAY/TIME BITS                            
         MVC   INVLDAY,INVDAY                                                   
         MVC   INVLSTIM(4),INVTIME                                              
         CLC   INVTIME+2(2),=C'CC' TEST FOR TO CONCLUSION                       
         BNE   DATAR10A                                                         
         SR    R1,R1                                                            
         ICM   R1,3,INVLSTIM       SET END TIME BY ADDING 2 HOURS               
         AH    R1,=H'200'          TO START TIME                                
         CH    R1,=H'2400'         TEST FOR RUN PAST MIDNIGHT                   
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STCM  R1,3,INVLSTIM+2                                                  
         SPACE 1                                                                
DATAR10A DS    0H                                                               
         GOTO1 VUNDAY,DMCB,INVDAY,(R6)                                          
         CLI   0(R6),C' '          TEST FOR END OF DAY EXPRESSION               
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         MVI   0(R6),C','          INSERT COMMA AFTER IT                        
         LA    R6,1(R6)                                                         
*                                                                               
         GOTO1 VUNTIME,DMCB,INVTIME,(R6)                                        
         CLI   0(R6),C' '          FIND END OF TIME EXPRESSION                  
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         OC    INVTIME+2(2),INVTIME+2 TEST FOR BREAK CODE                       
         BNZ   *+14                                                             
         MVC   0(2,R6),=C',B'      BREAK CODE                                   
         LA    R6,2(R6)            BUMP OUTPUT POINTER ACCORDINGLY              
         LA    R1,TRFIELD+8                                                     
         SR    R6,R1               FIND LENGTH OF DATA                          
         STC   R6,TRFIELD+5                                                     
         B     DATAR12                                                          
         SPACE 1                                                                
DATAR11  MVI   INVLTYP,X'80'                                                    
         LA    R8,REC                                                           
         MVC   INVLNUMB,RINVKINV-RINVKEY(R8)                                    
         MVC   INVLDATE,RINVKSTD-RINVKEY(R8)                                    
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
         MVI   0(R6),C','          COMMA AFTER INVENTORY NUMBER                 
         LA    R6,1(R6)                                                         
         GOTO1 VDATCON,DMCB,(3,INVLDATE),(5,(R6))                               
         LA    R6,8(R6)            POINT PAST DATE                              
         LA    R1,TRFIELD+8                                                     
         SR    R6,R1               FIND DATA LENGTH                             
         STC   R6,TRFIELD+5                                                     
         DROP  RE                                                               
         SPACE 2                                                                
DATAR12  EQU   *                                                                
         LA    RE,REC2             BUILD DATA RECORD IN REC2                    
         LR    R4,RE                                                            
         LA    RF,1000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ST    R4,AIOAREA                                                       
         SPACE                                                                  
         MVC   RINVKEY(24),INVKLAST                                             
         MVC   RINVKSRC(3),INVTOBK                                              
         SPACE 1                                                                
         LA    R6,SVCLST           CONVERT FROM BOOKVAL TO KSRC                 
DATAR12L CLC   INVTOBK(1),3(R6)                                                 
         BE    DATAR12P                                                         
         LA    R6,L'SVCLST(R6)                                                  
         CLI   0(R6),X'FF'                                                      
         BNE   DATAR12L                                                         
         DC    H'0'                                                             
DATAR12P MVC   RINVKSRC,2(R6)                                                   
         SPACE 1                                                                
DATAR13  MVC   RINVLEN,=H'35'      SET LENGTH FOR VIRGIN RECORD                 
         SPACE                                                                  
         GOTO1 VT80417,DMCB,(RC)                                                
         SPACE                                                                  
         CLI   INVBAD,0                                                         
         BE    DATAR15                                                          
         ZIC   R3,INVBAD           ERROR MESSAGE FROM DEMO MODULE               
         L     R2,THISLINE         POSITION CURSOR AT FROM FIELD                
         LA    R2,FROMH(R2)                                                     
         B     ERROR                                                            
         SPACE                                                                  
DATAR15  EQU   *                                                                
         MVC   HALF,REC2+27        RESTORE RECORD LENGTH AFTER                  
         LH    RE,HALF             DEMO MODULES                                 
         BCTR  RE,0                                                             
         STCM  RE,3,REC2+27                                                     
         SPACE 1                                                                
         XC    WORK,WORK           ADD DAY/TIME ELEMENT BEFORE                  
         MVC   WORK(2),=X'CE0A'    A POSSIBLE DEMUP CALL                        
         MVC   WORK+2(5),INVDAY    HEADER DAY TIME                              
         MVC   WORK+7(3),INVSRC    FROM BOOK                                    
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         SPACE                                                                  
         LA    R6,WORK3                                                         
         LA    R6,1000(R6)         BUILD TRANSFER FROM ELEMENT                  
         XC    0(200,R6),0(R6)     SO DEMUP WILL HAVE FROM SRC/BOOK             
         USING RINVFREL,R6                                                      
         MVI   RINVFRCD,X'03'                                                   
         MVC   RINVFRST,DEMSTA                                                  
         MVC   RINVFRBK,INVSRC                                                  
         MVC   RINVFRTY,INVTYP                                                  
         MVI   RINVFRPR,C'T'       FROM TRANSFER                                
         MVC   RINVFRBT,INVBTYPE   BOOK TYPE                                    
         CLI   INVTYP,C'I'         TEST FOR INVENTORY TRANSFER                  
         BNE   *+10                                                             
         MVC   RINVFRBT,INVFRBT    USE BK TYPE PASSED FROM T80417               
         ZIC   R1,TRFIELD+5        EXTRACT FROM DETAILS                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RINVFRDT(0),TRFIELD+8                                            
         LA    R1,RINVFRDT-RINVFREL+1(R1) FIND EL LEN                           
         STC   R1,RINVFRLN                                                      
         GOTO1 VADDELEM,DMCB,(R4),(R6)                                          
         DROP  R6                                                               
         SPACE 1                                                                
         LA    RE,WORK             BUILD CODE ELEMENT                           
         USING RINVCEL,RE                                                       
         XC    WORK,WORK                                                        
         MVI   RINVCCOD,X'CD'                                                   
         MVI   RINVCLEN,10                                                      
         MVC   RINVCODE,INVCODE                                                 
         TM    INVCDCTL,TP         CLEAR CODE FIELD FOR TIME                    
         BZ    *+18                PERIOD TRANSFERS WHEN AUTOMATIC              
         CLI   TRFNOVER,YES        FOOTNOTING IS SUPPRESSED                     
         BNE   *+10                                                             
         MVC   RINVCODE,SPACES                                                  
         TM    INVTOBK,X'20'       ESTIMATED TO BOOK TEST                       
         BZ    *+8                                                              
         MVI   RINVCSET,C'E'                                                    
         SPACE 1                                                                
         TM    INVTOBK,X'04'       PROJECTED TO BOOK TEST                       
         BZ    *+8                                                              
         MVI   RINVCSET,C'P'                                                    
         SPACE 1                                                                
         TM    INVTOBK,X'02'       SPECIAL SURVEY BOOK TEST                     
         BZ    *+8                                                              
         MVI   RINVCSET,C'S'                                                    
         SPACE 1                                                                
         L     R1,THISLINE         POINT R1 AT FROM FIELD                       
         LA    R1,FROM(R1)                                                      
         CLI   0(R1),C'+'                                                       
         BNE   *+8                                                              
         OI    RINVCTYP,X'80'      ADDED DEMOS                                  
         OC    RINVCTYP,INVIND     CUMULATIVE INDICATORS                        
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         DROP  RE                                                               
         SPACE 2                                                                
         L     R2,THISLINE                                                      
         LA    R2,UPGH(R2)                                                      
         CLI   5(R2),0                                                          
         BE    DATAR20                                                          
         XC    WORK,WORK                                                        
         GOTO1 VUPVAL,DMCB,(1,(R2)),WORK,(R7)                                   
         CLI   DMCB,1                                                           
         BE    *+12                                                             
         LA    R3,235              INVALID UPGRADE EXPRESSION                   
         B     ERROR                                                            
         SPACE                                                                  
         L     RE,THISLINE         DO NOT ALLOW UPGRADES FOR                    
         LA    RE,FROM(RE)         A COMBO                                      
         CLI   0(RE),C'+'                                                       
         BNE   *+12                                                             
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 1                                                                
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         CLI   RAVLNTYP,3          ONLY ALLOW ONE BOOK OPERAND                  
         BNE   DATAR16             FOR PUTS                                     
         CLI   RAVLNCAT,C'P'                                                    
         BNE   DATAR16                                                          
         OC    RAVLNOP2,RAVLNOP2                                                
         BZ    DATAR16                                                          
         LA    R3,INVERR                                                        
         B     ERROR                                                            
         SPACE 1                                                                
DATAR16  CLI   RAVLNTYP,0          MANIPULATION OF UPGRADE DATA                 
         BE    DATAR17                                                          
         OC    RAVLNOP1,RAVLNOP1                                                
         BZ    DATAR17                                                          
         OC    RAVLNOP2,RAVLNOP2                                                
         BNZ   DATAR17                                                          
         MVC   RAVLNOP2,INVFBK                                                  
         DROP  RE                                                               
         SPACE                                                                  
DATAR17  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         SPACE                                                                  
         MVI   BYTE2,0                                                          
         CLI   INVTYP,C'I'         TEST FOR INV TO INV TRANSFER                 
         BNE   *+8                                                              
         MVI   BYTE2,C'I'                                                       
*                                  BECAUSE OF UT/TP PROBLEM                     
         NI    11(R4),X'FF'-X'40'                                               
         GOTO1 VDEMUP,(R1),(BYTE2,REC2+34),WORK,(R7)                            
         OI    11(R4),X'40'                                                     
         B     DATAR30                                                          
         SPACE 2                                                                
* DATAR20 DEALS WITH THE CASE OF NO UPGRADE INPUT BY FORCING AN INDEX           
* 100 UPGRADE TO GET HPT DATA FOR TO DAY/TIME BOOK.                             
*                                                                               
DATAR20  EQU   *                                                                
         CLC   INVCODE,=C'PJ'      CODE PJ REQUIRES UPGRADE                     
         BNE   *+12                                                             
         LA    R3,MISINP                                                        
         B     ERROR                                                            
*                                                                               
DATAR24  DS    0H                                                               
         XC    WORK,WORK           BUILD FORCED UPGRADE ELEMENT                 
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVC   RAVLNOP1,=H'100'                                                 
         GOTO1 VADDELEM,DMCB,(R4),WORK                                          
         MVI   BYTE2,0                                                          
         CLI   INVTYP,C'I'         TEST FOR INV TO INV TRANSFER                 
         BNE   *+8                                                              
         MVI   BYTE2,C'I'                                                       
*                                  BECAUSE OF UT/TP PROBLEM                     
         NI    11(R4),X'FF'-X'40'                                               
         GOTO1 VDEMUP,(R1),(BYTE2,REC2+34),WORK,(R7)                            
         OI    11(R4),X'40'                                                     
         B     DATAR30                                                          
         DROP  RE                                                               
         SPACE                                                                  
DATAR30  EQU   *                                                                
         BAS   R8,FLADD                                                         
         LA    R4,REC                                                           
         XC    RINVKSRC(3),RINVKSRC                                             
         BAS   R8,FLCHA            CHANGE HEADER AND RESET POINTER              
         SPACE                                                                  
         L     R2,THISLINE         TURN ON ALL THE VALID BITS ON LINE           
         LA    RE,LINELEN(R2)                                                   
         SR    R1,R1                                                            
         OI    4(R2),X'20'                                                      
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,RE                                                            
         BL    *-12                                                             
         MVC   DEMEDIA(8),INVMED   RESTORE HEADER'S STATION/MEDIA               
         B     EXXMOD                                                           
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
         SPACE 1                                                                
         DS    0H                                                               
         EJECT                                                                  
*              ADD THE RECORD TO FILE                                           
         SPACE 1                                                                
         USING RINVAEL,R5                                                       
FLADD    MVC   KEY,RINVREC                                                      
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
         BR    R8                                                               
         SPACE 1                                                                
FLPUT    TM    KEY+27,X'80'                                                     
         BNO   *+12                                                             
         MVI   KEY+27,0                                                         
         BAS   RE,WRITE            UNDELETE THE POINTER                         
         LA    RE,WORK3                                                         
         ST    RE,AIOAREA                                                       
         BAS   RE,GETREC           GET OLD RECORD IN WORK3                      
         SPACE 1                                                                
         GOTO1 VGETEL,DMCB,(X'EF',AIOAREA),DMCB+8                               
         L     R5,DMCB+8                                                        
         CLI   DMCB,X'FF'                                                       
         BNE   *+8                                                              
         LA    R5,WORK                                                          
         MVC   RINVALST,TODAY                                                   
         MVI   RINVAWHY,C'C'                                                    
         SPACE 1                                                                
         LA    RE,REC2                                                          
         ST    RE,AIOAREA                                                       
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,AIOAREA,(R5)                                       
         BAS   RE,PUTREC           WRITE BACK THE NEW                           
         NI    DMINBTS,X'F7'                                                    
         MVC   BSVDA,KEY+28                                                     
         BR    R8                                                               
         SPACE 1                                                                
FLCHA    MVC   KEY,RINVREC                                                      
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         LA    RE,REC                                                           
         ST    RE,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         GOTO1 VGETEL,DMCB,(X'EF',AIOAREA),DMCB+8                               
         L     R5,DMCB+8                                                        
         CLI   DMCB,X'FF'                                                       
         BER   R8                                                               
         MVC   RINVALST,TODAY                                                   
         MVI   RINVAWHY,C'C'                                                    
         SPACE 1                                                                
         BAS   RE,PUTREC           WRITE BACK THE NEW                           
         MVC   BSVDA,KEY+28                                                     
         BR    R8                                                               
         EJECT                                                                  
*              EDIT INVENTORY NUMBER                                            
         SPACE 1                                                                
INVEDT   LA    R3,INVERR                                                        
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
         BR    R8                                                               
         SPACE 2                                                                
*              EDIT DATE                                                        
         SPACE 1                                                                
DATEDT   LA    R3,INVERR                                                        
         GOTO1 VDATVAL,DMCB,(0,12(R6)),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(3,RINVKSTD)                               
         BR    R8                                                               
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
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RELFMGEN                                                       
         EJECT                                                                  
       ++INCLUDE RELFMTWA                                                       
         EJECT                                                                  
       ++INCLUDE RELFMEDD                                                       
         PRINT OFF                                                              
         EJECT                                                                  
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE REGENAVL                                                       
         SPACE 2                                                                
       ++INCLUDE FATWA                                                          
         EJECT                                                                  
         PRINT ON                                                               
* INVENTORY LIST ENTRY DSECT                                                    
*                                                                               
INVLD    DSECT                                                                  
INVLREC  DS    0CL10                                                            
INVLFLE  DS    CL1                 P=PAV, I=INVENTORY                           
INVLTYP  DS    CL1                 X'80'  INVENTORY NUMBER                      
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
TP       EQU   X'04'               READ TIME PERIOD FILE                        
MIX      EQU   X'08'               READ PAV AND TIME PERIOD FILES               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
TYPEH    EQU   TRNTYPEH-TRNINV1H                                                
CODEH    EQU   TRNCODEH-TRNINV1H                                                
FROMH    EQU   TRNFROMH-TRNINV1H                                                
FROM     EQU   TRNFROM-TRNINV1H                                                 
UPGH     EQU   TRNUPGH-TRNINV1H                                                 
LINELEN  EQU   TRNINV2H-TRNINV1H                                                
         PRINT OFF                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084RELFM07   05/01/02'                                      
         END                                                                    
