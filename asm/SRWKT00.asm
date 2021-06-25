*          DATA SET SRWKT00    AT LEVEL 008 AS OF 03/29/11                      
*PHASE T13A00A                                                                  
         TITLE '$WKTEST - GENERATE WRKF/WRKZ TEST REPORTS'                      
         PRINT NOGEN                                                            
WKTEST   CSECT                                                                  
         NMOD1 WRKX-WRKD,**$WKT**,CLEAR=YES                                     
         USING WRKD,RC                                                          
         L     R2,20(R1)           R2=A(TWA)                                    
         USING SRWKTFFD,R2                                                      
         L     RA,0(R1)            RA=A(SYSFACS)                                
         USING SYSFACD,RA                                                       
         MVC   ATIA,4(R1)                                                       
         L     RF,8(R1)            RF=AUTL                                      
         USING UTLD,RF                                                          
         MVC   USERID,TUSER        SET USERID                                   
         OC    TUSER,TUSER                                                      
         BNZ   *+10                                                             
*&&UK*&& MVC   USERID,=X'0026'     IF NOT CONNECTED USE DDS1                    
*&&US*&& MVC   USERID,=X'002B'     IF NOT CONNECTED USE TCH1                    
         DROP  RF                                                               
*                                                                               
         LA    R8,RECBUFF          INITIALISE OPEN PRINT LINE                   
         XC    0(255,R8),0(R8)                                                  
*                                                                               
         CLC   =C'=WKTEST,Z',SRVSRV                                             
         BE    WKTESTZ                                                          
         CLC   =C'$WKTEST,Z',SRVSRV                                             
         BE    WKTESTZ                                                          
         CLC   =C'=WKZEST',SRVSRV                                               
         BE    WKTESTZ                                                          
         CLC   =C'$WKZEST',SRVSRV                                               
         BE    WKTESTZ                                                          
*                                                                               
***********************************************************************         
* WRKF TEST                                                                     
***********************************************************************         
         USING FWLHDRD,R8                                                       
         MVC   FWLSOFLAB,=C'*SOFSOF*'                                           
         MVC   FWLUSRID,USERID                                                  
*                                                                               
         LA    R4,SRVFILH          FILE ID                                      
         CLI   5(R4),1                                                          
         BL    MISS                                                             
         CLI   5(R4),9             MUST BE 9 CHRS                               
         BNE   INV                                                              
*                                  VALIDATE ABCD,12,C                           
         MVC   FWLSYSPRG,8(R4)                                                  
         MVC   FWLSUBPRG,11(R4)                                                 
         CLI   12(R4),C','                                                      
         BNE   INV                                                              
         TM    13(R4),X'F0'                                                     
         BNO   INV                                                              
         TM    14(R4),X'F0'                                                     
         BNO   INV                                                              
         PACK  DUB,13(2,R4)                                                     
         L     RF,DUB+4                                                         
         SRL   RF,4                                                             
         STC   RF,FWLDAY                                                        
         CLI   15(R4),C','                                                      
         BNE   INV                                                              
         MVC   FWLCLASS,16(R4)                                                  
FILEX    EQU   *                                                                
*                                                                               
TYPE     LA    R4,SRVTYPEH         FILE TYPE                                    
         CLI   5(R4),0                                                          
         BE    TYPEX                                                            
         CLI   5(R4),1                                                          
         BH    INV                                                              
         MVC   FWLTYPE,8(R4)                                                    
TYPEX    EQU   *                                                                
         SPACE 1                                                                
STAT     LA    R4,SRVSTATH         STATUS H/K                                   
         CLI   5(R4),0                                                          
         BE    STATX                                                            
         CLI   5(R4),2                                                          
         BH    INV                                                              
         LA    R4,8(R4)                                                         
         ZIC   R1,5(R4)                                                         
STAT1    CLI   0(R4),C'H'                                                       
         BNE   *+8                                                              
         OI    FWLSTAT,X'40'                                                    
         CLI   0(R4),C'K'                                                       
         BNE   *+8                                                              
         OI    FWLSTAT,X'08'                                                    
         LA    R4,1(R4)                                                         
         BCT   R1,STAT1                                                         
STATX    EQU   *                                                                
*                                                                               
LRET     LA    R4,SRVLRETH         LIVE RETAIN HOURS                            
         CLI   5(R4),0                                                          
         BE    LRETX                                                            
         CLI   5(R4),4                                                          
         BH    INV                                                              
         BL    LRET1                                                            
         CLC   8(4,R4),=C'PERM'                                                 
         BNE   LRET1                                                            
         MVC   FWLRETNL,=X'FFFF'                                                
         B     LRETX                                                            
LRET1    TM    4(R4),X'08'                                                      
         BZ    INV                                                              
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   RF,DUB                                                           
         STH   RF,FWLRETNL                                                      
LRETX    EQU   *                                                                
         SPACE 1                                                                
DRET     LA    R4,SRVDRETH         DEAD RETAIN HOURS                            
         CLI   5(R4),0                                                          
         BE    DRETX                                                            
         CLI   5(R4),4                                                          
         BH    INV                                                              
         BL    DRET1                                                            
         CLC   8(4,R4),=C'PERM'                                                 
         BNE   DRET1                                                            
         MVC   FWLRETND,=X'FFFF'                                                
         B     DRETX                                                            
DRET1    TM    4(R4),X'08'                                                      
         BZ    INV                                                              
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   RF,DUB                                                           
         STH   RF,FWLRETND                                                      
DRETX    EQU   *                                                                
         SPACE 1                                                                
ATTB     LA    R4,SRVATTBH         REPORT ATTRIBUTES                            
         SR    R1,R1                                                            
         ICM   R1,1,5(R4)                                                       
         BZ    ATTBX                                                            
         LA    RF,8(R4)                                                         
*                                                                               
ATTB01   CLI   0(RF),C'L'          TEST FOR LIBRARY FILE                        
*NOP     BNE   *+8                                                              
*NOP     OI    FWLATTB,FWLATLIB                                                 
*                                                                               
         LA    RF,1(RF)                                                         
         BCT   R1,ATTB01           TRY NEXT BYTE                                
*                                                                               
ATTBX    MVC   SAVATTB,FWLATTB                                                  
*                                                                               
FLEN     LA    R4,SRVFLENH         READ DATA LINES                              
         CLI   5(R4),0             IGNORE NO INPUT                              
         BNZ   FLEN01                                                           
         B     FLENX                                                            
FLEN01   TM    4(R4),X'08'         TEST NUMERIC                                 
         BZ    INV                                                              
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R4)                                                   
         CVB   R7,DUB              R7=FIXED LEN                                 
         STCM  R7,3,FWLMAXRL                                                    
         STCM  R7,3,FIXEDLEN                                                    
FLENX    EQU   *                                                                
*                                                                               
DESC     LA    R4,SRVDESCH         REPORT DESCRIPTION                           
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         SH    R1,=H'1'                                                         
         BM    DESCX                                                            
         MVC   FWLDESC,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FWLDESC(0),8(R4)                                                 
         CLC   FWLDESC(7),=C'$WKFULL'                                           
         BNE   DESCX                                                            
         DC    H'0',C'$WKFULL '                                                 
DESCX    EQU   *                                                                
                                                                                
PSWD     LA    R4,SRVPSWDH         REPORT PASSWORD                              
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         SH    R1,=H'1'                                                         
         BM    PSWDX                                                            
         MVC   FWLPSWD,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FWLPSWD(0),8(R4)                                                 
PSWDX    EQU   *                                                                
                                                                                
PAPPEN   LA    R4,SRVAPPNH         REPORT APPEND                                
         SR    R3,R3                                                            
         ICM   R3,1,5(R4)                                                       
         BZ    PAPPENX                                                          
         LA    R4,8(R4)                                                         
         BAS   RE,VALNUM                                                        
         CLI   DUB,X'FF'                                                        
         BE    PAPPENX                                                          
         CVB   R1,DUB              SET IT IN TIOBAID                            
         STCM  R1,3,FWLFILENO                                                   
         OI    FWLFLAG,FWLFLREFN                                                
         OI    FWLFLAG,FWLFLMOD                                                 
         OI    FWLFLAG,FWLFLRCOP                                                
PAPPENX  EQU   *                                                                
                                                                                
PUDATA   LA    R4,SRVUDATH         REPORT USER INDEX DATA                       
         CLI   5(R4),0                                                          
         BE    PUDATAX                                                          
         CLC   8(2,R4),=C'A='                                                   
         BE    PUDATA1                                                          
         CLC   8(2,R4),=C'B='                                                   
         BE    PUDATA1                                                          
         CLI   5(R4),2             TWO CHRS DEFAULTS TO ADDING BEFORE           
         BNE   INV                                                              
         MVC   UDATA,8(R4)                                                      
         MVI   UDATATY,C'B'                                                     
         B     PUDATA2                                                          
PUDATA1  CLI   5(R4),4             A=XX OR B=XX TO ADD BEFORE OR AFTER          
         BNE   INV                                                              
         MVC   UDATA,10(R4)                                                     
         MVC   UDATATY,8(R4)                                                    
PUDATA2  CLI   UDATATY,C'B'                                                     
         BNE   PUDATAX                                                          
         MVC   FWLUDATA,UDATA                                                   
PUDATAX  EQU   *                                                                
                                                                                
*----------------------------------------------------------------------         
*        OPEN WKRF FILE AND EXTRACT RETURNED VALUES                             
*----------------------------------------------------------------------         
GENFIRST MVI   PASS,1                                                           
         MVC   SAVEFST,RECBUFF     SAVE OPEN DETAIL                             
*                                                                               
GEN000   BAS   RE,PRINT            SET OPEN FILE PARAMS                         
         MVC   OKMSG,SPACES                                                     
         MVC   OKMSG(13),=CL10'FILE ===> '                                      
         MVC   OKMSG+50(4),WRKFILE                                              
         LA    RE,OKMSG+10                                                      
         MVC   0(3,RE),FWLSYSPRG                                                
         MVC   3(1,RE),FWLSUBPRG                                                
         SR    RF,RF                                                            
         IC    RF,FWLDAY                                                        
         SLL   RF,4                                                             
         O     RF,=F'12'                                                        
         ST    RF,FULL                                                          
         EDIT  (P4,FULL),(2,4(RE)),FILL=0                                       
         MVC   6(1,RE),FWLCLASS                                                 
         MVI   7(RE),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,FWLREPRNO      GET ASSIGNED REPORT NUMBER                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,RE),DUB                                                      
                                                                                
*----------------------------------------------------------------------         
*        READ DATA LINES AND WRITE TO WRKF                                      
*----------------------------------------------------------------------         
GEN001   XC    REPS,REPS           CLEAR REPEAT COUNT                           
         LA    RF,1                                                             
         STCM  RF,15,RECNUM        INIT RECNUM                                  
         LA    R4,SRVREPTH                                                      
         TM    4(R4),X'08'         TEST NUMERIC                                 
         BZ    GEN002                                                           
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),SRVREPT(0)                                                
         CVB   R1,DUB                                                           
         ST    R1,REPS             SAVE REPEAT COUNT                            
*                                                                               
GEN002   LA    R4,SRVREP1H         READ DATA LINES                              
         USING DLINED,R4                                                        
         CLI   DLREPSH+5,0         1ST MUST HAVE SOME DATA                      
         BZ    MISS                                                             
GEN003   CLI   DLREPSH+5,0         IGNORE NO INPUT                              
         BZ    GEND070                                                          
         TM    DLREPSH+4,X'08'     TEST NUMERIC                                 
         BZ    INV                                                              
         SR    R1,R1                                                            
         IC    R1,DLREPSH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),DLREPS(0)                                                 
         CVB   R7,DUB              R7=REPETITIONS                               
*                                                                               
         OC    FIXEDLEN,FIXEDLEN                                                
         BZ    GEND030                                                          
         CLI   DLLENH+5,0          MUST BE ZERO IF FIXED LEN                    
         BNE   INV                                                              
         LH    R0,FIXEDLEN         FAKE LEN IF FIXED                            
         CVD   R0,DUB                                                           
         B     GEND040                                                          
*                                                                               
GEND030  TM    DLLENH+4,X'08'      TEST NUMERIC                                 
         BZ    INV                                                              
         SR    R1,R1                                                            
         IC    R1,DLLENH+5                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),DLLEN(0)                                                  
GEND040  CVB   R0,DUB              R0=LENGTH                                    
         AHI   R0,10               OVERHEAD (LENGTH(4) + SEQ#(6))               
         CLI   PASS,1                                                           
         BE    GEND070             DONT DO ALL THIS CRAP ON PASS1               
*                                                                               
         LR    RF,R8                                                            
         AR    RF,R0                                                            
         ST    RF,FULL             SAVE A(EOF) IN FULL                          
*                                                                               
         LA    RF,4(R8)            RF=4(R8) FOR VARIABLE                        
         SLL   R0,16                                                            
         STCM  R0,15,0(R8)         SET LEN INTO 1ST 4 BYTES                     
*                                                                               
         EDIT  (B4,RECNUM),(6,0(RF)),DUB=EDUB                                   
*                                                                               
         ICM   RF,15,RECNUM        BUMP RECNUM                                  
         LA    RF,1(RF)                                                         
         STCM  RF,15,RECNUM                                                     
*                                                                               
         LA    RF,10(R8)           LL00NNNNNNDATA--DATA--ECT                    
*                                                                               
GEND050  SR    R1,R1               GET ACTUAL DATA LEN IN R1                    
         ICM   R1,1,DLDATAH+5                                                   
         BZ    GEND051                                                          
         BCTR  R1,0                                                             
*                                                                               
GEND051  L     RE,FULL             TEST FOR END OF RECORD                       
         SR    RE,RF                                                            
         BZ    GEND060             PERFECT FIT                                  
         CR    RE,R1                                                            
         BNL   GEND055             LOADS OF ROOM SO CONTINUE                    
         LR    R1,RE                                                            
         BCTR  R1,0                FILL IN THE LAST BIT                         
*                                                                               
GEND055  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DLDATA      MOVE DATA TO RECORD                          
         LA    RF,1(RF,R1)                                                      
         B     GEND051                                                          
*                                                                               
GEND060  BAS   RE,PRINT            WRITE THE RECORD                             
*                                                                               
GEND065  BCT   R7,GEND040          REPEAT (R7) TIMES                            
*                                                                               
GEND070  LA    R1,SRVREPXH                                                      
         CR    R4,R1               TEST FOR END OF SCREEN                       
         BE    GEND080                                                          
*                                                                               
         LA    R4,DLINEL(R4)       NEXT DATA LINE                               
         B     GEN003                                                           
*                                                                               
GEND080  OC    REPS,REPS           REPEAT (REPS) TIMES                          
         BZ    GEND090                                                          
         L     R1,REPS                                                          
         BCTR  R1,0                                                             
         ST    R1,REPS                                                          
         B     GEN002                                                           
*                                                                               
GEND090  MVC   FWLSOFLAB,=C'*EOFEOF*'                                           
         CLI   UDATATY,C'A'                                                     
         BE    GEND095                                                          
         BAS   RE,PRINT            WRITE EOF                                    
         B     GEND100                                                          
*                                                                               
GEND095  LA    RF,WRKKEY           DO A "CLO/USX' WITH DATA IN INDEX            
         USING FUKRECD,RF                                                       
         MVC   FUKINFO,UDATA                                                    
         GOTO1 VDATAMGR,DMCB,=C'CLO/USX',WRKFILE,WRKKEY,RECBUFF,ATIA            
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
*                                                                               
GEND100  CLI   PASS,1              IS THIS THE TEST PASS                        
         BNE   EXIT1                                                            
         MVI   PASS,2              OK DO IT FOR REAL                            
         MVC   RECBUFF(256),SAVEFST                                             
         B     GEN000                                                           
*                                                                               
PRINT    CLI   PASS,1              DON'T PRINT ON 1ST PASS                      
         BER   RE                                                               
         ST    RE,SAVERE                                                        
         GOTO1 VDATAMGR,DMCB,DMPRINT,WRKFILE,0,RECBUFF,ATIA                     
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* WRKZ TEST                                                                     
***********************************************************************         
WKTESTZ  DS    0D                                                               
*                                                                               
         USING ZWLHDRD,R8                                                       
         MVC   ZWLSOFLAB,=C'*SOFSOF*'                                           
         MVC   ZWLUSRID,USERID                                                  
                                                                                
*----------------------------------                                             
* FILE ID                                                                       
*----------------------------------                                             
         LA    R4,SRVFILH          FILE ID                                      
         CLI   5(R4),1                                                          
         BL    MISS                                                             
         CLI   5(R4),9             MUST BE 9 CHRS                               
         BNE   INV                                                              
*                                  VALIDATE ABCD,12,C                           
         MVC   ZWLSYSPRG,8(R4)                                                  
         MVC   ZWLSUBPRG,11(R4)                                                 
         CLI   12(R4),C','                                                      
         BNE   INV                                                              
         TM    13(R4),X'F0'                                                     
         BNO   INV                                                              
         TM    14(R4),X'F0'                                                     
         BNO   INV                                                              
         PACK  DUB,13(2,R4)                                                     
         L     RF,DUB+4                                                         
         SRL   RF,4                                                             
         STC   RF,ZWLDAY                                                        
         CLI   15(R4),C','                                                      
         BNE   INV                                                              
         MVC   ZWLCLASS,16(R4)                                                  
                                                                                
*----------------------------------                                             
* FILE TYPE                                                                     
*----------------------------------                                             
         LA    R4,SRVTYPEH         FILE TYPE                                    
         CLI   5(R4),0                                                          
         BE    ZTYPEX                                                           
         CLI   5(R4),1                                                          
         BH    INV                                                              
         MVC   ZWLTYPE,8(R4)                                                    
ZTYPEX   DS    0H                                                               
                                                                                
*----------------------------------                                             
* STATUS H/K                                                                    
*----------------------------------                                             
ZSTAT    LA    R4,SRVSTATH         STATUS H/K                                   
         CLI   5(R4),0                                                          
         BE    ZSTATX                                                           
         CLI   5(R4),2                                                          
         BH    INV                                                              
         LA    R4,8(R4)                                                         
         ZIC   R1,5(R4)                                                         
ZSTAT1   CLI   0(R4),C'H'                                                       
         BNE   *+8                                                              
         OI    ZWLSTAT,X'40'                                                    
         CLI   0(R4),C'K'                                                       
         BNE   *+8                                                              
         OI    ZWLSTAT,X'08'                                                    
         LA    R4,1(R4)                                                         
         BCT   R1,ZSTAT1                                                        
ZSTATX   EQU   *                                                                
                                                                                
*----------------------------------                                             
* LIVE RETAIN HOURS                                                             
*----------------------------------                                             
ZLRET    LA    R4,SRVLRETH         LIVE RETAIN HOURS                            
         CLI   5(R4),0                                                          
         BE    ZLRETX                                                           
         CLI   5(R4),4                                                          
         BH    INV                                                              
         BL    ZLRET1                                                           
         CLC   8(4,R4),=C'PERM'                                                 
         BNE   ZLRET1                                                           
         MVC   ZWLRETNL,=X'FFFF'                                                
         B     ZLRETX                                                           
ZLRET1   TM    4(R4),X'08'                                                      
         BZ    INV                                                              
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   RF,DUB                                                           
         STH   RF,ZWLRETNL                                                      
ZLRETX   EQU   *                                                                
                                                                                
*----------------------------------                                             
* DEAD RETAIN HOURS                                                             
*----------------------------------                                             
ZDRET    LA    R4,SRVDRETH         DEAD RETAIN HOURS                            
         CLI   5(R4),0                                                          
         BE    ZDRETX                                                           
         CLI   5(R4),4                                                          
         BH    INV                                                              
         BL    ZDRET1                                                           
         CLC   8(4,R4),=C'PERM'                                                 
         BNE   ZDRET1                                                           
         MVC   ZWLRETND,=X'FFFF'                                                
         B     ZDRETX                                                           
ZDRET1   TM    4(R4),X'08'                                                      
         BZ    INV                                                              
         ZIC   R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R4)                                                      
         CVB   RF,DUB                                                           
         STH   RF,ZWLRETND                                                      
ZDRETX   EQU   *                                                                
*----------------------------------                                             
* REPORT ATTRIBUTES                                                             
*----------------------------------                                             
ZATTB    LA    R4,SRVATTBH         REPORT ATTRIBUTES                            
         SR    R1,R1                                                            
         ICM   R1,1,5(R4)                                                       
         BZ    ZATTBX                                                           
         LA    RF,8(R4)                                                         
*                                                                               
ZATTB01  CLI   0(RF),C'L'          TEST FOR LIBRARY FILE                        
*NOP     BNE   *+8                                                              
*NOP     OI    ZWLATTB,ZWLATLIB                                                 
*                                                                               
         LA    RF,1(RF)                                                         
         BCT   R1,ZATTB01          TRY NEXT BYTE                                
*                                                                               
ZATTBX   MVC   SAVATTB,ZWLATTB                                                  
                                                                                
*----------------------------------                                             
* LENGTH                                                                        
*----------------------------------                                             
ZFLEN    LA    R4,SRVFLENH         READ DATA LINES                              
         CLI   5(R4),0             IGNORE NO INPUT                              
         BNZ   ZFLEN01                                                          
         B     ZFLENX                                                           
ZFLEN01  TM    4(R4),X'08'         TEST NUMERIC                                 
         BZ    INV                                                              
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R4)                                                   
         CVB   R7,DUB              R7=FIXED LEN                                 
         STCM  R7,3,ZWLMAXRL                                                    
         STCM  R7,3,FIXEDLEN                                                    
ZFLENX   EQU   *                                                                
                                                                                
*----------------------------------                                             
* REPORT DESCRIPTION                                                            
*----------------------------------                                             
ZDESC    LA    R4,SRVDESCH         REPORT DESCRIPTION                           
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         SH    R1,=H'1'                                                         
         BM    ZDESCX                                                           
         MVC   ZWLDESC,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ZWLDESC(0),8(R4)                                                 
         CLC   ZWLDESC(7),=C'$WZFULL'                                           
         BNE   ZDESCX                                                           
         DC    H'0',C'$WZFULL '                                                 
ZDESCX   EQU   *                                                                
                                                                                
*----------------------------------                                             
* REPORT PASSWORD                                                               
*----------------------------------                                             
ZPSWD    LA    R4,SRVPSWDH         REPORT PASSWORD                              
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         SH    R1,=H'1'                                                         
         BM    ZPSWDX                                                           
         MVC   ZWLPSWD,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ZWLPSWD(0),8(R4)                                                 
ZPSWDX   EQU   *                                                                
                                                                                
*----------------------------------                                             
* REPORT APPEND                                                                 
*----------------------------------                                             
ZAPPEN   LA    R4,SRVAPPNH         REPORT APPEND                                
         SR    R3,R3                                                            
         ICM   R3,1,5(R4)                                                       
         BZ    ZAPPENX                                                          
         LA    R4,8(R4)                                                         
         BAS   RE,VALNUM                                                        
         CLI   DUB,X'FF'                                                        
         BE    ZAPPENX                                                          
         CVB   R1,DUB              SET IT IN TIOBAID                            
         STCM  R1,15,ZWLFILENO                                                  
         OI    ZWLFLAG,ZWLFLREFN                                                
         OI    ZWLFLAG,ZWLFLMOD                                                 
         OI    ZWLFLAG,ZWLFLRCOP                                                
ZAPPENX  DS    0H                                                               
                                                                                
*----------------------------------                                             
* REPORT USER INDEX DATA                                                        
*----------------------------------                                             
ZUDATA   LA    R4,SRVUDATH         REPORT USER INDEX DATA                       
         CLI   5(R4),0                                                          
         BE    ZUDATAX                                                          
         CLC   8(2,R4),=C'A='                                                   
         BE    ZUDATA1                                                          
         CLC   8(2,R4),=C'B='                                                   
         BE    ZUDATA1                                                          
         CLI   5(R4),2             TWO CHRS DEFAULTS TO ADDING BEFORE           
         BNE   INV                                                              
         MVC   UDATA,8(R4)                                                      
         MVI   UDATATY,C'B'                                                     
         B     ZUDATA2                                                          
ZUDATA1  CLI   5(R4),4             A=XX OR B=XX TO ADD BEFORE OR AFTER          
         BNE   INV                                                              
         MVC   UDATA,10(R4)                                                     
         MVC   UDATATY,8(R4)                                                    
ZUDATA2  CLI   UDATATY,C'B'                                                     
         BNE   ZUDATAX                                                          
         MVC   ZWLUDATA,UDATA                                                   
ZUDATAX  DS    0H                                                               
                                                                                
*----------------------------------------------------------------------         
*        OPEN WKRF FILE AND EXTRACT RETURNED VALUES         *                   
*----------------------------------------------------------------------         
         MVI   PASS,1                                                           
         MVC   SAVEFST,RECBUFF     SAVE OPEN DETAIL                             
*                                                                               
ZGEN000  BAS   RE,ZPRINT           SET OPEN FILE PARAMS                         
         MVC   OKMSG,SPACES                                                     
         MVC   OKMSG(13),=CL10'FILE ===> '                                      
         MVC   OKMSG+50(4),WRKZILE                                              
         LA    RE,OKMSG+10                                                      
         MVC   0(3,RE),ZWLSYSPRG                                                
         MVC   3(1,RE),ZWLSUBPRG                                                
         SR    RF,RF                                                            
         IC    RF,ZWLDAY                                                        
         SLL   RF,4                                                             
         O     RF,=F'12'                                                        
         ST    RF,FULL                                                          
         EDIT  (P4,FULL),(2,4(RE)),FILL=0                                       
         MVC   6(1,RE),ZWLCLASS                                                 
         MVI   7(RE),C','                                                       
         ICM   R0,15,ZWLREPRNO     GET ASSIGNED REPORT NUMBER                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,RE),DUB                                                      
                                                                                
*----------------------------------------------------------------------         
* READ DATA LINES AND WRITE TO WRKZ                                             
*----------------------------------------------------------------------         
         XC    REPS,REPS           CLEAR REPEAT COUNT                           
         LA    RF,1                                                             
         STCM  RF,15,RECNUM        INIT RECNUM                                  
         LA    R4,SRVREPTH                                                      
         TM    4(R4),X'08'         TEST NUMERIC                                 
         BZ    ZGEN002                                                          
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),SRVREPT(0)                                                
         CVB   R1,DUB                                                           
         ST    R1,REPS             SAVE REPEAT COUNT                            
*                                                                               
ZGEN002  LA    R4,SRVREP1H         READ DATA LINES                              
         USING DLINED,R4                                                        
         CLI   DLREPSH+5,0         1ST MUST HAVE SOME DATA                      
         BZ    MISS                                                             
ZGEN003  CLI   DLREPSH+5,0         IGNORE NO INPUT                              
         BZ    ZGEND070                                                         
         TM    DLREPSH+4,X'08'     TEST NUMERIC                                 
         BZ    INV                                                              
         SR    R1,R1                                                            
         IC    R1,DLREPSH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),DLREPS(0)                                                 
         CVB   R7,DUB              R7=REPETITIONS                               
*                                                                               
         OC    FIXEDLEN,FIXEDLEN                                                
         BZ    ZGEND030                                                         
         CLI   DLLENH+5,0          MUST BE ZERO IF FIXED LEN                    
         BNE   INV                                                              
         LH    R0,FIXEDLEN         FAKE LEN IF FIXED                            
         CVD   R0,DUB                                                           
         B     ZGEND040                                                         
*                                                                               
ZGEND030 TM    DLLENH+4,X'08'      TEST NUMERIC                                 
         BZ    INV                                                              
         SR    R1,R1                                                            
         IC    R1,DLLENH+5                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),DLLEN(0)                                                  
ZGEND040 CVB   R0,DUB              R0=LENGTH                                    
         AHI   R0,10               OVERHEAD (LENGTH(4) + SEQ#(6))               
         CLI   PASS,1                                                           
         BE    ZGEND070            DONT DO ALL THIS CRAP ON PASS1               
*                                                                               
         LR    RF,R8                                                            
         AR    RF,R0                                                            
         ST    RF,FULL             SAVE A(EOF) IN FULL                          
*                                                                               
         LA    RF,4(R8)            RF=4(R8) FOR VARIABLE                        
         SLL   R0,16                                                            
         STCM  R0,15,0(R8)         SET LEN INTO 1ST 4 BYTES                     
*                                                                               
         EDIT  (B4,RECNUM),(6,0(RF)),DUB=EDUB                                   
*                                                                               
         ICM   RF,15,RECNUM        BUMP RECNUM                                  
         LA    RF,1(RF)                                                         
         STCM  RF,15,RECNUM                                                     
*                                                                               
         LA    RF,10(R8)           LL00NNNNNNDATA--DATA--ECT                    
*                                                                               
ZGEND050 SR    R1,R1               GET ACTUAL DATA LEN IN R1                    
         ICM   R1,1,DLDATAH+5                                                   
         BZ    ZGEND051                                                         
         BCTR  R1,0                                                             
*                                                                               
ZGEND051 L     RE,FULL             TEST FOR END OF RECORD                       
         SR    RE,RF                                                            
         BZ    ZGEND060            PERFECT FIT                                  
         CR    RE,R1                                                            
         BNL   ZGEND055            LOADS OF ROOM SO CONTINUE                    
         LR    R1,RE                                                            
         BCTR  R1,0                FILL IN THE LAST BIT                         
*                                                                               
ZGEND055 EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),DLDATA      MOVE DATA TO RECORD                          
         LA    RF,1(RF,R1)                                                      
         B     ZGEND051                                                         
*                                                                               
ZGEND060 BAS   RE,ZPRINT           WRITE THE RECORD                             
*                                                                               
ZGEND065 BCT   R7,ZGEND040         REPEAT (R7) TIMES                            
*                                                                               
ZGEND070 LA    R1,SRVREPXH                                                      
         CR    R4,R1               TEST FOR END OF SCREEN                       
         BE    ZGEND080                                                         
*                                                                               
         LA    R4,DLINEL(R4)       NEXT DATA LINE                               
         B     ZGEN003                                                          
*                                                                               
ZGEND080 OC    REPS,REPS           REPEAT (REPS) TIMES                          
         BZ    ZGEND090                                                         
         L     R1,REPS                                                          
         BCTR  R1,0                                                             
         ST    R1,REPS                                                          
         B     ZGEN002                                                          
*                                                                               
ZGEND090 MVC   ZWLSOFLAB,=C'*EOFEOF*'                                           
         CLI   UDATATY,C'A'                                                     
         BE    ZGEND095                                                         
         BAS   RE,ZPRINT            WRITE EOF                                   
         B     ZGEND100                                                         
*                                                                               
ZGEND095 LA    RF,WRKKEY           DO A "CLO/USX' WITH DATA IN INDEX            
         USING ZUKRECD,RF                                                       
         MVC   ZUKINFO,UDATA                                                    
         GOTO1 VDATAMGR,DMCB,=C'CLO/USX',WRKZILE,WRKKEY,RECBUFF,ATIA            
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
*                                                                               
ZGEND100 CLI   PASS,1              IS THIS THE TEST PASS                        
         BNE   EXIT1                                                            
         MVI   PASS,2              OK DO IT FOR REAL                            
         MVC   RECBUFF(256),SAVEFST                                             
         B     ZGEN000                                                          
*                                                                               
ZPRINT   CLI   PASS,1              DON'T PRINT ON 1ST PASS                      
         BER   RE                                                               
         ST    RE,SAVERE                                                        
         GOTO1 VDATAMGR,DMCB,DMPRINT,WRKZILE,0,RECBUFF,ATIA                     
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* EXITS AND ERRORS                                                              
***********************************************************************         
EXIT1    MVC   SRVMSG,OKMSG        OUTPUT OK MESSAGE                            
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'                                                      
                                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
MISS     LA    R5,=CL60'ERROR 01 MISSING INPUT FIELD'                           
         B     *+8                                                              
INV      LA    R5,=CL60'ERROR 02 INVALID INPUT FIELD'                           
         MVC   SRVMSG(60),0(R5)                                                 
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'                                                      
         B     EXIT                                                             
*                                                                               
ERROR    MVC   SRVMSG,SPACES                                                    
         MVC   SRVMSG(8),=C'ERROR 00'                                           
         IC    R4,8(R1)                                                         
         SRL   R4,4                                                             
         STC   R4,DUB                                                           
         IC    R4,8(R1)                                                         
         SLL   R4,28                                                            
         SRL   R4,28                                                            
         STC   R4,DUB+1                                                         
         OC    SRVMSG+6(2),DUB                                                  
         OI    SRVMSGH+6,X'80'                                                  
         TM    8(R1),X'80'                                                      
         BZ    EXIT                                                             
         DC    H'0',C'$WKFULL '                                                 
         B     EXIT                                                             
*                                                                               
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKF1  '                                                     
WRKZILE  DC    CL8'WRKZ1  '                                                     
SPACES   DC    CL136' '                                                         
                                                                                
***********************************************************************         
*        VALNUM R4=A(FIELD),R3=LEN  EXIT DUB=PACKED OR FF   *                   
***********************************************************************         
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
WRKD     DSECT                                                                  
DUB      DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
ATIA     DS    A                                                                
SAVERE   DS    A                                                                
DMCB     DS    6F                                                               
*                                                                               
REPS     DS    F                                                                
RECNUM   DS    F                                                                
FIXEDLEN DS    H                                                                
USERID   DS    H                                                                
SAVATTB  DS    X                                                                
PASS     DS    X                                                                
         DS    X                                                                
UDATATY  DS    X                                                                
UDATA    DS    XL2                                                              
WORK     DS    XL32                                                             
OKMSG    DS    CL60                                                             
*                                                                               
WRKKEY   DS    CL128                                                            
SAVEFST  DS    CL255                                                            
RECBUFF  DS    4096C                                                            
*                                                                               
WRKX     EQU   *                                                                
         EJECT                                                                  
DLINED   DSECT                                                                  
DLREPSH  DS    XL8                                                              
DLREPS   DS    CL4                 REPETITIONS                                  
DLLENH   DS    XL8                                                              
DLLEN    DS    CL6                 LENGTH                                       
DLDATAH  DS    XL8                                                              
DLDATA   DS    CL60                DATA                                         
*                                                                               
DLINEL   EQU   *-DLINED                                                         
         EJECT                                                                  
*                                                                               
*PREFIX=F                                                                       
* DMWRKFK                                                                       
       ++INCLUDE DMWRKFK                                                        
* DMWRKFL                                                                       
       ++INCLUDE DMWRKFL                                                        
*PREFIX=                                                                        
*                                                                               
*PREFIX=Z                                                                       
* DMWRKFK                                                                       
       ++INCLUDE DMWRKZK                                                        
* DMWRKFL                                                                       
       ++INCLUDE DMWRKZL                                                        
*PREFIX=                                                                        
*                                                                               
SRWKTFFD DSECT                                                                  
         DS    CL64                                                             
* SRWKTFFD                                                                      
       ++INCLUDE SRWKTFFD                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FAD                                                            
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SRWKT00   03/29/11'                                      
         END                                                                    
