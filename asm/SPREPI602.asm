*          DATA SET SPREPI602  AT LEVEL 128 AS OF 01/16/09                      
*PHASE SPI602A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPI602 - SPOT INVOICE RECORD LISTING'                           
***********************************************************************         
*        QOPT1- Y=INCLUDE EASI, N=NO EASI, O=ONLY EASI                          
*        QOPT2- A=DATE ADDED FILTERING                                          
*               M=MONTH OF SERVICE DATE FILTERING                               
*        QOPT3- S=ZERO SPOTS,$=ZERO$M,Y=BOTH                                    
*        QOPT4- N=NO DETAILS, PERSON RECAPS ONLY                                
*        QOPT5- Y=RUN ACROSS ALL AGY/MEDIA ON FILE                              
***********************************************************************         
SPI602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPI6,CLEAR=YES,R8                                              
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING WKD,RC                                                           
*                                                                               
         RELOC RELO                                                             
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
         CLI   MODE,REQLAST                                                     
         BNE   *+12                                                             
         BAS   RE,REQL                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BNE   *+12                                                             
         BAS   RE,CLTF                                                          
         J     EXIT                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*        RUN FIRST                                                              
*                                                                               
RUNF     DS    0H                                                               
         LA    RF,PERSREC                                                       
         ST    RF,ADPERS                                                        
         MVC   SVSECAGY,AGY                                                     
         XC    CTKEY,CTKEY                                                      
         LA    R1,CTKEY                                                         
         USING CT5REC,R1           ACCESS RECORD                                
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGY                                                     
         DROP  R1                                                               
*                                                                               
         MVC   KEYSAVE,CTKEY                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),=CL8'CTFILE',CTKEY,ADPERS          
         CLI   DMCB+8,0                                                         
         BNE   RUNFX               NO ACCESS RECORD = NO SEC AGY                
*                                                                               
         CLC   CTKEY(L'CT5KEY),KEYSAVE                                          
         BNE   RUNFX               NO ACCESS RECORD = NO SEC AGY                
*                                                                               
         L     R6,ADPERS                                                        
         MVI   ELCODE,CTSEAELQ                                                  
         MVC   DATADISP,=X'001C'   28 FOR CTFILE                                
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
RUNF10   BRAS  RE,NEXTEL                                                        
         BNE   RUNFX                                                            
         CLI   0(R6),CTSEAELQ                                                   
         BNE   RUNFX                                                            
         CLI   2(R6),C' '                                                       
         BNH   RUNFX                                                            
         MVC   SVSECAGY,2(R6)                                                   
*                                                                               
RUNFX    B     EXIT                                                             
*                                                                               
*                                                                               
*        REQUEST FIRST                                                          
*                                                                               
REQF     DS    0H                                                               
         LA    RF,HDPROC                                                        
         ST    RF,HEADHOOK                                                      
*                                                                               
         L     RF,=A(IPTRLST)      INPUTTER LIST                                
         A     RF,RELO                                                          
         ST    RF,ANXTIPTR                                                      
         MVI   0(RF),X'FF'         SET EOL                                      
*                                                                               
         LA    R0,SRKEYL           SET SORT KEY LENGTH                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(2),DUB                                               
*                                                                               
         LA    R0,SRECL            SET SORT RECORD LENGTH                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(2),DUB                                                
*                                                                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0  SET SORT CONTROLS               
         XC    SORTREC,SORTREC     PASS DUMMY EOF RECORD                        
         MVI   SORTREC,X'FF'                                                    
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
         B     REQF4                                                            
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,NN,BI,A),WORK=1'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=NN'                                    
*                                                                               
REQF4    DS    0H                                                               
         LA    RF,INVREC                                                        
         ST    RF,ADINV                                                         
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QEND,C' '                                                        
         BH    *+10                                                             
         MVC   QEND,QSTART                                                      
*                                                                               
         CLI   QOPT2,C'M'          IF FILTERING ON MONTH                        
         BNE   REQF5                                                            
         MVC   QSTART+4(2),=C'01'  NEED 1ST OF MONTHS                           
         MVC   QEND+4(2),=C'01'                                                 
*                                                                               
         MVC   BQSTARTP,=X'FFFF'                                                
         CLI   QSTART,C' '                                                      
         BNH   REQF4D                                                           
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)  AND TWO BYTE DATES              
         XC    BQSTARTP,=X'FFFF'   AND COMPLEMENT                               
*                                                                               
REQF4D   DS    0H                                                               
         XC    BQENDP,BQENDP                                                    
         CLI   QEND,C' '                                                        
         BNH   REQF20                                                           
         GOTO1 (RF),(R1),QEND,(2,BQENDP)                                        
         XC    BQENDP,=X'FFFF'                                                  
         B     REQF20                                                           
*                                                                               
REQF5    DS    0H                                                               
         CLI   QOPT2,C' '                                                       
         BH    *+8                                                              
         MVI   QOPT2,C'A'          DEFAULT DATE TYPE                            
         CLI   QOPT2,C'A'          IF FILTERING ON ADD DATE                     
         BNE   REQF20                                                           
         XC    BQSTART,BQSTART                                                  
         CLI   QSTART,C' '                                                      
         BNH   REQF5B                                                           
         GOTO1 DATCON,DMCB,QSTART,(3,BQSTART)                                   
*                                                                               
REQF5B   DS    0H                                                               
         MVC   BQEND,=X'FFFFFF'                                                 
         CLI   QEND,C' '                                                        
         BNH   REQF20                                                           
         GOTO1 (RF),(R1),QEND,(3,BQEND)                                         
*                                                                               
REQF20   DS    0H                                                               
         CLI   QOPT5,C'Y'          ALL AGY/MED?                                 
         BNE   REQFX                NO - USE NORMAL SPONSOR BREAKS              
*                                                                               
         BAS   RE,BLDAGYL                                                       
         MVI   BAGYMD,0                                                         
         MVC   QAGY,=C'@@'         FORCE ALPHA AGY NOT EQUAL                    
*                                                                               
* READ CLTHDR                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY+1,1                                                          
*                                                                               
REQF30   GOTO1 HIGH                                                             
*                                                                               
REQF32   CLI   KEY,0               TEST HEADER REC                              
         BNE   REQF100                                                          
         OC    KEY+4(9),KEY+4      TEST CLIENT REC                              
         BZ    REQF40                                                           
         MVI   KEY+4,X'FF'         FORCE NEXT CLT                               
         B     REQF30                                                           
*                                                                               
REQF40   GOTO1 GETCLT                                                           
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         CLC   CKEYAM,BAGYMD       SAME AGYMD?                                  
         BE    REQF50                                                           
         MVC   BAGYMD,CKEYAM                                                    
*                                                                               
         XR    RE,RE                                                            
         IC    RE,BAGYMD                                                        
         SRL   RE,4                DROP MEDIA                                   
         MHI   RE,4                AGY X 4                                      
         LA    RE,AGYTAB(RE)                                                    
         MVC   QAGY,0(RE)          SAVE AGY ALPHA                               
         MVC   COUNTRY,3(RE)                                                    
*                                                                               
REQF50   MVC   BCLT,CKEYCLT                                                     
         MVI   MODE,CLTFRST                                                     
         BAS   RE,CLTF                                                          
         XC    KEY,KEY                                                          
         MVC   KEY,CKEY                                                         
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         B     REQF32                                                           
         DROP  R6                                                               
*                                                                               
REQF100  BAS   RE,REQL                                                          
         GOTO1 AENDREQ                                                          
*                                                                               
REQFX    B     EXIT                                                             
*                                                                               
REQFERR  DS    0H                                                               
         MVC   P(80),QAREA                                                      
         MVC   P+85(14),=C'INVALID REQUEST'                                     
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
         SPACE 2                                                                
*        CLIENT FIRST                                                           
*                                                                               
CLTF     NTR1                                                                   
         BAS   RE,RDINV            READ INVOICE AND PASS TO SORT                
         MVI   MODE,CLTLAST        DONE WITH CLIENT                             
         B     EXIT                                                             
         SPACE 2                                                                
*        REQUEST LAST                                                           
*                                                                               
REQL     NTR1                                                                   
         CLC   QCLT,=C'ALL'        IF DOING SINGLE CLIENT                       
         BE    REQL2                                                            
         CLI   QCLT,C'A'           *, ETC.                                      
         BL    REQL2                                                            
         MVI   MODE,CLTLAST        RESET MODE SO IT WILL SHOW                   
*                                                                               
REQL2    DS    0H                                                               
         BAS   RE,RPORT            PRINT REPORT                                 
         MVI   MODE,REQLAST                                                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        RDINV - READ INVOICES AND PASS TO SORT                                 
*                                                                               
***********************************************************************         
         SPACE 2                                                                
RDINV    NTR1                                                                   
         XC    LASTKEY,LASTKEY                                                  
         XC    ITOTS,ITOTS                                                      
         ZAP   IDLRS,=P'0'                                                      
         XC    SORTREC,SORTREC                                                  
         LA    R5,SORTREC          BUILD SORT RECORD                            
         USING SRECD,R5                                                         
         LA    R7,XKEY                                                          
         USING SNVKEYD,R7                                                       
         XC    XKEY,XKEY                                                        
         MVC   SNVKTYPE(2),=X'0E03'                                             
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,BCLT                                                     
*                                                                               
RD2      DS    0H                                                               
         BAS   RE,XFHIGH              EXPANDED KEY FILE                         
         B     RD4B                                                             
RD4      DS    0H                                                               
         BAS   RE,XFSEQ                                                         
RD4B     DS    0H                                                               
         CLC   XKEY(05),XKEYSAVE     THRU CLIENT                                
         BNE   RD90                  END OF INPUT                               
                                                                                
*                                  PROCESS FILE INVOICE RECORDS                 
*                                  ----------------------------                 
*                                                                               
         LA    R7,XKEY                                                          
         CLI   INVFILT,0            INVOICE NUMBER FILTER FOR TESTING           
         BE    RD9                                                              
         CLC   SNVKINV,INVFILT                                                  
         BNE   RD4                                                              
         B     RD9                                                              
*                                                                               
INVFILT  DC    10X'00'                                                          
TRCOUNT  DC    PL4'10000'                                                       
*                                                                               
RD9      DS    0H                                                               
         CLI   QOPT2,C'M'          FILTERING ON MOS?                            
         BNE   RD9B                                                             
         CLC   SNVKMOS,BQENDP                                                   
         BL    RD4                                                              
         CLC   SNVKMOS,BQSTARTP                                                 
         BH    RD4                                                              
*                                                                               
         CLC   XKEY(SNVKMINK-SNVKEY),LASTKEY  ON NEW INV (MINIO SET)            
         BE    RD9B                                                             
         XC    SORTREC,SORTREC               CLEAR SORT RECORD                  
         MVC   LASTKEY,XKEY                                                     
*                                                                               
RD9B     DS    0H                                                               
         MVC   AREC,ADINV                                                       
         BAS   RE,XFGET                                                         
*                                                                               
         CLI   QOPT5,C'T'          TRACE?                                       
         BNE   RD10                                                             
         SP    TRCOUNT,=P'1'                                                    
         BM    RD90                                                             
         L     R7,ADINV                                                         
         GOTO1 HEXOUT,DMCB,(R7),P+2,65,=C'N'                                    
         GOTO1 REPORT                                                           
*                                                                               
RD10     DS    0H                                                               
         L     R7,ADINV                                                         
         CLC   BSTA,SNVKSTA        TEST SAME STATION                            
         BE    RD10C                                                            
*                                  NEW STATION                                  
         MVC   BSTA,SNVKSTA                                                     
         MVC   WORK,KEY                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
*                                                                               
*                                  READ STATION FOR MKT                         
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         GOTO1 MSUNPK,DMCB,SNVKSTA-2,FULL,W                                     
         CLI   W+4,C' '                                                         
         BNE   *+8                                                              
         MVI   W+4,C'T'                                                         
         MVC   KEY+2(5),W                                                       
*                                                                               
         MVC   KEY+7(2),AGY                                                     
         MVC   KEY+9(3),CLT                                                     
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         MVC   MKT,=C'9999'        NO STATION RECORD MEANS NO MKT               
*                                                                               
         GOTO1 HIGHSTA                                                          
         L     RF,ADSTAT                                                        
         CLC   KEYSAVE(9),0(RF)                                                 
         BNE   *+10                                                             
         MVC   MKT,SMKT-STAREC(RF)                                              
*                                                                               
RD10C    DS    0H                                                               
         LA    R6,SNVELS           **GO THRU ELEMS**                            
*                                                                               
RD10D    DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    RD14                                                             
         CLI   0(R6),X'40'         INVOICE ITEM ELEM                            
         BE    RD12                                                             
         CLI   0(R6),X'10'         INVOICE HEADER                               
         BE    RD11F                                                            
         CLI   0(R6),X'F1'         ACTIVITY ELEM                                
         BE    RD11H                                                            
*                                                                               
RD11     DS    0H                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RD10D                                                            
*                                                                               
RD11F    DS    0H                  INVOICE HEADER ELEM                          
         USING SNVHDELD,R6                                                      
         CLI   SNVHDEZS,C' '                                                    
         BNH   *+8                                                              
         MVI   SREASI,C'Y'                                                      
         MVC   SREST,SNVHDEST      ESTIMATE                                     
*                                                                               
         MVC   SRPERS(4),SNVHDEZS  EASI SOURCE                                  
         CLI   SNVHDPRD,0          PRODUCT                                      
         BE    RD11                                                             
         CLI   SNVHDPRD,X'FF'                                                   
         BE    RD11                                                             
         L     RF,ADCLT            FIND 3 CHAR CODE                             
         LA    RF,CLIST-CLTHDR(RF)                                              
         LA    R0,220                                                           
*                                                                               
         MVC   SRPRD,=C'???'                                                    
RD11G    DS    0H                                                               
         CLC   3(1,RF),SNVHDPRD                                                 
         BE    *+16                                                             
         LA    RF,4(RF)                                                         
         BCT   R0,RD11G                                                         
         B     RD11                                                             
*                                                                               
         MVC   SRPRD,0(RF)                                                      
         B     RD11                                                             
*                                                                               
RD11H    DS    0H                  ACTIVITY ELEM                                
         USING ACTVD,R6                                                         
         MVC   SRACTD,ACTVADDT     DATE ADDED  ONLY                             
*                                                                               
         CLI   1(R6),ACTVLENQ      NEW ELEM LENGTH?                             
         BL    RD11H01                                                          
         OC    ACTVSCID,ACTVSCID   ANYTHING THERE?                              
         BZ    RD11H01                                                          
         MVC   SRPERS,ACTVSCID     PERSON                                       
         B     RD11                                                             
*                                                                               
RD11H01  DS    0H                  ACTIVITY ELEM                                
         TM    13(R6),X'80'        NEW SECURITY ON?                             
         BZ    RD11                                                             
*                                                                               
         XC    CTKEY,CTKEY                                                      
         MVI   CTKEY,C'0'                                                       
         MVC   CTKEY+1(2),SVSECAGY                                              
         MVC   CTKEY+23(2),ACTVCHID  PID                                        
         MVC   KEYSAVE,CTKEY                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',CTKEY,ADPERS              
         CLI   DMCB+8,0                                                         
         BNE   RD11                                                             
         CLC   CTKEY(L'CT0KEY),KEYSAVE  HAVE AUTH RECORD?                       
         BNE   RD11                     NO                                      
         LR    R4,R6               SAVE R6                                      
         L     R6,ADPERS                                                        
*                                                                               
         MVI   ELCODE,X'C3'                                                     
         MVC   DATADISP,=X'001C'   28 FOR CTFILE                                
         BRAS  RE,GETEL                                                         
         BNE   RD11H02                                                          
         MVC   SRPERS,2(R6)                                                     
*                                                                               
RD11H02  DS    0H                  ACTIVITY ELEM                                
         LR    R6,R4                                                            
         B     RD11                                                             
*                                                                               
RD12     DS    0H                  DETAIL ELEMENT                               
         USING SNVIDELD,R6                                                      
         L     RF,ISPTS            JUST BUMP ITEM COUNT                         
         LA    RF,1(RF)                                                         
         ST    RF,ISPTS                                                         
*                                                                               
         ICM   RF,15,SNVIDCST      AND COST                                     
         CVD   RF,DUB                                                           
         AP    IDLRS,DUB                                                        
         B     RD11                                                             
*                                                                               
RD14     DS    0H                                                               
         CLI   QOPT5,C'T'          TRACE?                                       
         BNE   RD14B                                                            
         GOTO1 HEXOUT,DMCB,SRECD,P+4,SRECL,=C'N'                                
         GOTO1 REPORT                                                           
*                                                                               
RD14B    DS    0H                                                               
         CLI   SNVKMINK,X'FF'    FF REC IS END OF INVOICE (MINIO SET)           
         BNE   RD4                                                              
*                                                                               
         CLI   QOPT2,C'A'          TEST FILTERING ON ACTIVITY DATES             
         BNE   RD15                                                             
         CLC   SRACTD,BQSTART                                                   
         BL    RD18                                                             
         CLC   SRACTD,BQEND                                                     
         BH    RD18                                                             
*                                                                               
RD15     DS    0H                                                               
         CLI   QOPT1,C'Y'          EASI OK                                      
         BE    RD15D                                                            
         CLI   QOPT1,C'N'          NO EASI                                      
         BNE   RD15B                                                            
         CLI   SREASI,C'Y'                                                      
         BE    RD18                                                             
         B     RD15D                                                            
RD15B    DS    0H                                                               
         CLI   QOPT1,C'O'          EASI ONLY                                    
         BNE   RD15D                                                            
         CLI   SREASI,C'Y'                                                      
         BE    RD15D                                                            
         B     RD18                                                             
*                                                                               
RD15D    DS    0H                                                               
         CLI   QOPT3,C'N'          ZERO INV OPTION                              
         BE    RD15Z                                                            
         CLI   QOPT3,C'S'          TEST TO LIST ONLY ZERO SPOT INVS             
         BNE   RD15F                                                            
         OC    ISPTS,ISPTS                                                      
         BNZ   RD18                                                             
         B     RD15Z                                                            
*                                                                               
RD15F    DS    0H                                                               
         CLI   QOPT3,C'$'          TEST TO LIST ONLY ZERO DOLLAR INVS           
         BNE   RD15H                                                            
         CP    IDLRS,=P'0'         ZERO DOLLARS?                                
         BNE   RD18                NO                                           
         B     RD15Z                                                            
*                                                                               
RD15H    DS    0H                                                               
         CLI   QOPT3,C'Y'          ONLY ZERO DOLLAR AND SPOT INVS               
         BNE   RD15J                                                            
         OC    ISPTS,ISPTS         ZERO SPOTS?                                  
         BNZ   RD18                                                             
         CP    IDLRS,=P'0'         ZERO DOLLARS?                                
         BNE   RD18                NO                                           
         B     RD15Z                                                            
*                                                                               
RD15J    DS    0H                                                               
*                                                                               
RD15Z    DS    0H                                                               
         CLI   QPERSON,C' '        PERSON FILTER                                
         BNH   RD16                                                             
         CLC   QPERSON,SRPERS      IF PERSON NOT =                              
         BNE   RD18                SKIP INVOICE                                 
*                                                                               
RD16     DS    0H                                                               
         PACK  DUB,MKT             MARKET                                       
         CVB   R0,DUB                                                           
         STCM  R0,3,SRMKT                                                       
         MVC   SRSTA,SNVKSTA       STATION                                      
         MVC   SRCLT,SNVKCLT       CLIENT                                       
         MVC   SRINV,SNVKINV       INVOICE NUMBER                               
         MVC   SRMOS,SNVKMOS       MONTH                                        
         MVC   SRSPTS,ISPTS                                                     
         MVC   SRDLRS,IDLRS                                                     
         MVC   SRDISK,XKEY+36                                                   
*                                                                               
         CLI   QOPT5,C'T'          TRACE?                                       
         BNE   RD17                                                             
         GOTO1 HEXOUT,DMCB,SRECD,P+4,SRECL,=C'N'                                
         MVI   P+2,C'S'                                                         
         GOTO1 REPORT                                                           
*                                                                               
RD17     DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SRECD                                       
*                                                                               
RD18     DS    0H                                                               
         XC    ITOTS,ITOTS                                                      
         ZAP   IDLRS,=P'0'                                                      
         XC    SORTREC,SORTREC                                                  
         CLI   INVFILT,0           IF INV# FILTER (FOR TESTING)                 
         BNE   RD90                STOP NOW                                     
         B     RD4                 NEXT RECORD                                  
*                                                                               
RD90     DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         DROP  R6                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        RPORT - REPORT RECORDS BACK FROM SORT                                  
*                                                                               
***********************************************************************         
         SPACE 2                                                                
RPORT    NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   RPTYP,C'N'          NORMAL REPORT                                
         XC    LASTREC,LASTREC                                                  
         MVC   LASTMKT,=X'FFFF'                                                 
         XC    ITOTS,ITOTS                                                      
         ZAP   IDLRS,=P'0'                                                      
         XC    MTOTS,MTOTS                                                      
         ZAP   MDLRS,=P'0'                                                      
         XC    PTOTS,PTOTS                                                      
         ZAP   PDLRS,=P'0'                                                      
         XC    RTOTS,RTOTS                                                      
         ZAP   RDLRS,=P'0'                                                      
*                                                                               
RP4      DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'GET',0                                           
*                                                                               
         OC    DMCB+4(4),DMCB+4    TEST AT END                                  
         BZ    RP90                DONE, (FF REC TAKES CARE OF LASTS)           
*                                                                               
         L     R5,DMCB+4                                                        
         USING SRECD,R5                                                         
         LA    R6,P                                                             
         USING PLIND,R6                                                         
*                                                                               
         CLC   SRECD(10),LASTREC           PERSON/MKT CHANGE                    
         BE    RP6                                                              
*                                  FINISH PREVIOUS MARKET                       
         CLC   LASTMKT,=X'FFFF'    (IF ANY)                                     
         BZ    RP5                                                              
         CLI   QOPT4,C'N'          IF DOING RECAPS ONLY                         
         BE    RP4D                SKIP MARKET TOTALS                           
*                                                                               
         MVC   PLSTA(17),=C'**MARKET TOTALS**'                                  
         MVC   PLINVN(8),=C'INVOICES'                                           
         EDIT  (B4,MINVS),(5,PLINVN+9),ZERO=NOBLANK,COMMAS=YES                  
         EDIT  (B4,MSPTS),(8,PLSPTS),ZERO=NOBLANK,COMMAS=YES                    
         EDIT  (P8,MDLRS),(14,PLDLRS),2,COMMAS=YES                              
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
RP4D     DS    0H                                                               
         LA    R2,MTOTS            ROLL UP TOTALS                               
         LA    R3,PTOTS                                                         
         BAS   RE,ROLLUP                                                        
         XC    MTOTS,MTOTS                                                      
         ZAP   MDLRS,=P'0'                                                      
*                                                                               
         CLC   SRPERS,LASTREC+SRPERS-SRECD   PERSON CHANGE                      
         BE    RP5                                                              
*                                  FINISH PREVIOUS PERSON                       
         OC    LASTREC,LASTREC     (IF ANY)                                     
         BZ    RP5                                                              
         CLI   QOPT4,C'N'          IF DOING RECAPS ONLY                         
         BE    RP4F                SKIP INPUTTER TOTALS HERE                    
         MVC   PLSTA(19),=C'**INPUTTER TOTALS**'                                
         MVC   PLINVN(8),=C'INVOICES'                                           
         EDIT  (B4,PINVS),(5,PLINVN+9),ZERO=NOBLANK,COMMAS=YES                  
         EDIT  (B4,PSPTS),(8,PLSPTS),ZERO=NOBLANK,COMMAS=YES                    
         EDIT  (P8,PDLRS),(14,PLDLRS),2,COMMAS=YES                              
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
RP4F     DS    0H                                                               
         L     RF,ANXTIPTR         SAVE INPUTTER INFO                           
         MVC   0(8,RF),LASTREC+SRPERS-SRECD                                     
         MVC   8(16,RF),PINVS      INVOICES,SPOTS,$                             
         LA    RF,24(RF)                                                        
         ST    RF,ANXTIPTR                                                      
         MVI   0(RF),X'FF'        SET EOL                                       
*                                                                               
         LA    R2,PTOTS            ROLL UP TOTALS                               
         LA    R3,RTOTS                                                         
         BAS   RE,ROLLUP                                                        
         XC    PTOTS,PTOTS                                                      
         ZAP   PDLRS,=P'0'                                                      
*                                  START OF NEW MARKET                          
RP5      DS    0H                                                               
         CLI   SRECD,X'FF'         EOF RECORD                                   
         BE    RP4                 SKIP IT                                      
         CLI   QOPT4,C'N'          IF DOING RECAPS ONLY                         
         BE    RP6                 SKIP MARKET                                  
*                                                                               
         MVI   FORCEMID,C'Y'                                                    
         MVC   MID1(6),=C'MARKET'                                               
         SR    R0,R0               MARKET                                       
         ICM   R0,3,SRMKT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKT,DUB                                                          
         MVC   MID1+7(4),MKT                                                    
*                                  READ MARKET                                  
         CLC   MKT,=C'9999'                                                     
         BNE   *+14                                                             
         MVC   MID1+12(11),=C'**UNKNOWN**'                                      
         B     RP5D                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),MKT                                                     
         MVC   KEY+6(2),AGY                                                     
         GOTO1 READMKT                                                          
         L     RF,ADMARKET                                                      
         MVC   MID1+12(24),MKTNAME-MKTREC(RF)                                   
*                                                                               
RP5D     DS    0H                                                               
         OC    MID1,SPACES                                                      
         GOTO1 UNDERLIN,DMCB,(40,MID1),MID2                                     
*                                                                               
         SR    RF,RF               LEAVE AT LEAST 6 LINES FOR NEW MKT           
         IC    RF,LINE             (NOTE- ALLOWLIN DOESN'T WORK IF              
         LA    RF,6(RF)            FORCEMID IS SET)                             
         SR    R0,R0                                                            
         IC    R0,MAXLINES                                                      
         CR    RF,R0                                                            
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVPERS,SRPERS                                                    
*                                                                               
RP6      DS    0H                                                               
         CLI   QOPT4,C'N'          IF DOING RECAPS ONLY                         
         BE    RP8B                SKIP ALL THIS                                
         GOTO1 MSUNPK,DMCB,(X'80',SRSTA-2),FULL,PLSTA                           
         CLI   PLSTA,C'0'          CABLE HEAD?                                  
         BL    *+8                                                              
         MVI   PLSTA+4,C'/'                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SRCLT                                                   
         GOTO1 READ                                                             
         GOTO1 GETCLT                                                           
*                                                                               
         L     R3,ADCLT                                                         
         USING CLTHDR,R3                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,(CPROF+6,SRCLT),PLCLT                                
         DROP  R3                                                               
*                                                                               
         MVC   PLPRD,SRPRD                                                      
         EDIT  (B1,SREST),(3,PLEST)                                             
*                                                                               
         XC    SRMOS,=X'FFFF'                                                   
         GOTO1 DATCON,DMCB,(2,SRMOS),(18,PLMOS)                                 
*                                                                               
         MVC   PLINVN,SRINV         INVOICE NUMBER                              
*                                                                               
         OC    SRACTD,SRACTD                                                    
         BZ    RP7                                                              
         GOTO1 DATCON,DMCB,(3,SRACTD),(5,PLACTD)                                
RP7      DS    0H                                                               
*                                                                               
         EDIT  (B4,SRSPTS),(8,PLSPTS),COMMAS=YES,ZERO=NOBLANK                   
         EDIT  (P8,SRDLRS),(14,PLDLRS),2,COMMAS=YES                             
*                                                                               
         B     RP8                                                              
         GOTO1 HEXOUT,DMCB,SRDISK,PLDISK,4,=C'N'                                
*                                                                               
RP8      DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
RP8B     DS    0H                                                               
         MVC   LASTREC,SRECD       SAVE CURRENT RECORD                          
         MVC   LASTMKT,SRMKT       SAVE MARKET                                  
         MVC   IINVS,=F'1'                                                      
         MVC   ISPTS,SRSPTS                                                     
         MVC   IDLRS,SRDLRS                                                     
         LA    R2,ITOTS            ROLLUP TOTALS                                
         LA    R3,MTOTS                                                         
         BAS   RE,ROLLUP                                                        
         XC    ITOTS,ITOTS                                                      
         ZAP   IDLRS,=P'0'                                                      
         B     RP4                                                              
*                                                                               
RP90     DS    0H                                                               
*                                  FINISH REQUEST                               
         OC    LASTREC,LASTREC     (IF ANY DATA)                                
         BZ    RP96                                                             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RPTYP,C'R'          RECAP                                        
         L     R4,=A(IPTRLST)      A(INPUTTER LIST)                             
         A     R4,RELO                                                          
*                                                                               
RP94     DS    0H                                                               
         CLI   0(R4),X'FF'         EOL                                          
         BE    RP95                                                             
*                                                                               
         MVC   P(8),0(R4)                                                       
         EDIT  (B4,08(R4)),(08,P+14),COMMAS=YES,ZERO=NOBLANK                    
         EDIT  (B4,12(R4)),(09,P+26),COMMAS=YES,ZERO=NOBLANK                    
         EDIT  (P8,16(R4)),(15,P+37),2,COMMAS=YES,MINUS=YES                     
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R4,24(R4)                                                        
         B     RP94                                                             
*                                                                               
RP95     DS    0H                                                               
         GOTO1 REPORT              SKIP A LINE                                  
         MVC   P(08),=C'*TOTAL* '                                               
         EDIT  (B4,RINVS),(08,P+14),ZERO=NOBLANK,COMMAS=YES                     
         EDIT  (B4,RSPTS),(09,P+26),ZERO=NOBLANK,COMMAS=YES                     
         EDIT  (P8,RDLRS),(15,P+37),2,COMMAS=YES                                
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
RP96     DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'END'                                             
RPX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE 2                                                                
*        ROLLUP - ROLL UP TOTALS                                                
ROLLUP   DS    0H                                                               
         LA    R0,2                                                             
RU4      DS    0H                                                               
         L     RF,0(R3)                                                         
         A     RF,0(R2)                                                         
         ST    RF,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,RU4                                                           
         AP    0(8,R3),0(8,R2)                                                  
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*        DATAMGR LINKS CALLS FOR EXPANDED KEY FILE                              
*                                                                               
XFHIGH   DS    0H                                                               
         LR    R0,RE                                                            
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),=CL8'XSPDIR',XKEY,XKEY             
         B     DMCK                                                             
         SPACE 2                                                                
XFSEQ    DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),=CL8'XSPDIR',XKEY,XKEY             
         B     DMCK                                                             
         SPACE 2                                                                
XFGET    DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),=CL8'XSPFIL',XKEY+36,     X        
               ADINV,DMWORK                                                     
         B     DMCK                                                             
*                                                                               
DMCK     DS    0H                                                               
         MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,DMCB+8                                                      
         BZ    DMCK4                                                            
         DC    H'0'                                                             
*                                                                               
DMCK4    DS    0H                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        HDPROC - HEADLINES                                                     
*                                                                               
***********************************************************************         
         SPACE 2                                                                
HDPROC   NTR1                                                                   
         LA    R6,HEAD8            USE HEADS 7,8,9                              
         USING PLIND,R6                                                         
*                                                                               
         CLI   RPTYP,C'N'                                                       
         BNE   HDP08                                                            
*                                                                               
         MVC   PLINVN(7),=C'INVOICE'                                            
         MVC   PLSTA+132(7),=C'STATION'                                         
         MVC   PLCLT+132(3),=C'CLT'                                             
         MVC   PLPRD+132(3),=C'PRD'                                             
         MVC   PLEST+132(3),=C'EST'                                             
         MVC   PLMOS+132(5),=C'MONTH'                                           
         MVC   PLINVN+1+132(6),=C'NUMBER'                                       
         MVC   PLACTD-1+132(10),=C'DATE ADDED'                                  
         MVC   PLSPTS+3+132(5),=C'SPOTS'                                        
         MVC   PLDLRS+4+132(10),=C'GROSS COST'                                  
*                                                                               
         MVC   PLSTA+264(7),DASHES                                              
         MVC   PLCLT+264(3),DASHES                                              
         MVC   PLMOS+264(5),DASHES                                              
         MVC   PLINVN+264(7),DASHES                                             
         MVC   PLACTD-1+264(10),DASHES                                          
         MVC   PLSPTS+3+264(5),DASHES                                           
         MVC   PLDLRS+4+264(10),DASHES                                          
*                                                                               
         MVC   HEAD6(9),=C'INPUTTER-'                                           
         MVC   HEAD6+11(8),SVPERS                                               
         CLI   SVPERS,C'*'                                                      
         BNL   HDP09                                                            
         GOTO1 HEXOUT,DMCB,SVPERS,HEAD6+11,8,=C'N'                              
         MVC   HEAD6+9(2),=C'X'''                                               
         MVI   HEAD6+27,C''''                                                   
         B     HDP09                                                            
*                                                                               
HDP08    DS    0H                  RECAP COLUMN HEADS                           
         MVC   00(08,R6),=C'INPUTTER'                                           
         MVC   14(08,R6),=C'INVOICES'                                           
         MVC   30(05,R6),=C'SPOTS'                                              
         MVC   42(10,R6),=C'GROSS COST'                                         
*                                                                               
         MVC   132+00(08,R6),DASHES                                             
         MVC   132+14(08,R6),DASHES                                             
         MVC   132+30(05,R6),DASHES                                             
         MVC   132+42(10,R6),DASHES                                             
*                                                                               
HDP09    DS    0H                                                               
         CLI   QSTART,C' '                                                      
         BNH   HDP20                                                            
         CLI   QOPT2,C'A'          ACTIVITY DATE FILTERING                      
         BNE   HDP10                                                            
         MVC   HEAD4(11),=C'DATES ADDED'                                        
         GOTO1 DATCON,DMCB,QSTART,(5,HEAD4+13)                                  
         GOTO1 (RF),(R1),QEND,(5,HEAD4+22)                                      
         MVI   HEAD4+21,C'-'                                                    
         B     HDP20                                                            
*                                                                               
HDP10    DS    0H                                                               
         CLI   QOPT2,C'M'          MOS FILTERING                                
         BNE   HDP20                                                            
         MVC   HEAD4(16),=C'MONTH OF SERVICE'                                   
         GOTO1 DATCON,DMCB,QSTART,(18,HEAD4+18)                                 
         GOTO1 (RF),(R1),QEND,(18,HEAD4+25)                                     
         MVI   HEAD4+24,C'-'                                                    
*                                                                               
HDP20    DS    0H                                                               
         CLI   QOPT1,C'N'          NO EASI                                      
         BNE   *+14                                                             
         MVC   HEAD5(22),=C'EASI INVOICES EXCLUDED'                             
         B     HDP30                                                            
         CLI   QOPT1,C'O'          ONLY EASI                                    
         BNE   *+14                                                             
         MVC   HEAD5(18),=C'EASI INVOICES ONLY'                                 
         B     HDP30                                                            
*                                                                               
HDP30    DS    0H                                                               
HDPRX    DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
*          DATA SET SPREPXS02  AT LEVEL 044 AS OF 05/25/04                      
         EJECT                                                                  
*===============================================================                
* BUILD A LIST OF AGENCIES ON THIS SPTFILE                                      
*===============================================================                
                                                                                
BLDAGYL  NTR1                                                                   
         XC    AGYTAB(AGYTABL),AGYTAB                                           
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         GOTO1 HIGH                                                             
         B     BLDAG2X                                                          
*                                                                               
BLDAG2   GOTO1 SEQ                                                              
*                                                                               
BLDAG2X  CLI   KEY,6                                                            
         JNE   EXIT                                                             
*                                                                               
         GOTO1 GETAGY              READ AGYHDR                                  
*                                                                               
         L     R4,ADAGY                                                         
         USING AGYHDR,R4                                                        
*                                                                               
         LA    R6,AGYEL                                                         
         SR    R0,R0                                                            
*                                                                               
BLDAG4   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    BLDAG2                                                           
         CLI   0(R6),2                                                          
         BNE   BLDAG4                                                           
         CLC   QMED,2(R6)          MATCH MEDIA CODE                             
         BNE   BLDAG4                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,3(R6)            GET AGY/MED BYTE                             
         SRL   RE,4                DROP MEDIA                                   
         MHI   RE,4                AGY X 4                                      
         LA    RE,AGYTAB(RE)                                                    
         MVC   0(2,RE),KEY+1       MOVE ALPHA AGENCY TO SLOT                    
         MVC   2(1,RE),3(R6)       MOVE AGY/MED BYTE TOO                        
         MVC   3(1,RE),AGYPROF+7-AGYHDR(RE)  SAVE COUNTRY CODE                  
         NI    2(RE),X'F0'         DROP MEDIA                                   
         B     BLDAG2                                                           
         DROP  R4                                                               
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 3                                                                
         DS    0D                                                               
         DC    CL8'*AGYTAB*'                                                    
AGYTAB   DS    16XL4               AGY ALPHA, BAGY (LEFT ALIGN)                 
AGYTABX  EQU   *                                                                
AGYTABL  EQU   AGYTABX-AGYTAB                                                   
*                                                                               
ELCODE   DS    X                                                                
SVSECAGY DS    CL2                                                              
DASHES   DC    40C'-'                                                           
VSORTER  DC    V(SORTER)                                                        
PERSREC  DS    1000X                                                            
INVREC   DS    4000C                                                            
IPTRLST  DS    1200XL20       1200 INPUTTERS MAX X 20 BYTES PER                 
*                                                                               
*                                                                               
*                                                                               
WKD      DSECT                                                                  
RELO     DS    A                                                                
ADPERS   DS    A                                                                
ADINV    DS    A                                                                
ITOTS    DS    0XL8                                                             
IINVS    DS    F                                                                
ISPTS    DS    F                                                                
IDLRS    DS    PL8                                                              
MTOTS    DS    0XL8                                                             
MINVS    DS    F                                                                
MSPTS    DS    F                                                                
MDLRS    DS    PL8                                                              
PTOTS    DS    0XL8                                                             
PINVS    DS    F                                                                
PSPTS    DS    F                                                                
PDLRS    DS    PL8                                                              
RTOTS    DS    0XL8                                                             
RINVS    DS    F                                                                
RSPTS    DS    F                                                                
RDLRS    DS    PL8                                                              
*                                                                               
W        DS    XL100                                                            
X        DS    XL256                                                            
THISREC  DS    XL(SRECL)                                                        
LASTREC  DS    XL(SRECL)                                                        
SORTREC  DS    XL(SRECL)                                                        
LASTKEY  DS    XL32                                                             
LASTMKT  DS    XL2                                                              
SVPERS   DS    CL8                                                              
RPTYP    DS    CL1                                                              
ANXTIPTR DS    A                                                                
         DS    0F                                                               
*                                                                               
CTKEY    DS    XL25                                                             
XKEY     DS    XL64                                                             
XKEYSAVE DS    XL64                                                             
         DS    0F                                                               
         SPACE 3                                                                
*                   INVOICE RECORD DSECT                                        
       ++INCLUDE SPGENSNV                                                       
         SPACE 2                                                                
PLIND    DSECT                                                                  
PLIN     DS    0CL132                                                           
         DS    CL3                                                              
PLSTA    DS    CL9                                                              
         DS    CL1                                                              
PLCLT    DS    CL3                                                              
         DS    CL2                                                              
PLPRD    DS    CL3                                                              
         DS    CL2                                                              
PLEST    DS    CL3                                                              
         DS    CL2                                                              
PLMOS    DS    CL6                                                              
         DS    CL2                                                              
PLINVN   DS    CL10                                                             
         DS    CL2                                                              
PLACTD   DS    CL8                                                              
         DS    CL5                                                              
PLSPTS   DS    CL8                                                              
         DS    CL2                                                              
PLDLRS   DS    CL14                                                             
         DS    CL2                                                              
PLDISK   DS    CL8                                                              
         SPACE 3                                                                
SRECD    DSECT                                                                  
SRPERS   DS    CL8                                                              
SRMKT    DS    XL2                                                              
SRSTA    DS    XL3                                                              
SRCLT    DS    XL2                                                              
SRPRD    DS    CL3                                                              
SREST    DS    XL1                                                              
SRMOS    DS    XL2                                                              
SRINV    DS    CL10                                                             
SRACTD   DS    XL3                                                              
SRKEYL   EQU   *-SRECD                                                          
SREASI   DS    CL1                                                              
SRDISK   DS    XL4                                                              
SRSPTS   DS    XL4                                                              
SRDLRS   DS    PL8                                                              
SRECL    EQU   *-SRECD                                                          
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
SPWORKD  DSECT                                                                  
         ORG   Q2USER                                                           
QPERSON  DS    CL8                 INPUTTER                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'128SPREPI602 01/16/09'                                      
         END                                                                    
