*          DATA SET SPREPRN02  AT LEVEL 056 AS OF 11/19/19                      
*PHASE SPRN02T                                                                  
*INCLUDE MEDBDESC                                                               
*INCLUDE SPRPFOOT                                                               
*INCLUDE REPCALOV                                                               
         PRINT NOGEN                                                            
         TITLE 'SPREPRN02 - NETWORK SCHEDULE'                                   
* PWES - NOTE: SVIADDR NOT SET AS NO FILCON ROUTINE                             
SPRN02   CSECT                                                                  
         NMOD1 0,SPRN02                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,=A(SP60WK)                                                    
         USING SP60WK,R2                                                        
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         STM   RA,RC,SP60RA                                                     
         ST    R2,SP60R2                                                        
*                                                                               
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         XC    DMCB,DMCB                                                        
         LA    R1,DMCB                                                          
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNL      TRANSLATE LIST                               
         MVI   DDRETN,DDCASEU                                                   
         MVI   DDSYS,2                                                          
         MVC   DDLANG,RCLANG                                                    
         LA    RF,DCLIST                                                        
         STCM  RF,7,DDIADR                                                      
         LA    RF,DSLIST                                                        
         STCM  RF,7,DDOADR                                                      
         GOTO1 DICTATE                                                          
         DROP  R5,R1                                                            
*                                                                               
BYPW     DS    0H                                                               
         CLI   MODE,ESTFRST                                                     
         BNH   *+12                                                             
         CLI   QSTART,C' '                                                      
         BE    EXIT                                                             
         GOTO1 =V(SPRPFOOT),DMCB,(RA)                                           
         CLI   MODE,RUNFRST                                                     
         BNE   M1                                                               
         MVC   SVMAXLIN,MAXLINES                                                
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         LA    RF,PTSDESC                                                       
         ST    RF,APTSDESC                                                      
         LA    RF,PRSDESC                                                       
         ST    RF,APRSDESC                                                      
         L     RF,=A(PNTABLE)                                                   
         ST    RF,VPNTABLE                                                      
         L     RF,=A(SSTABLE)                                                   
         ST    RF,VSSTABLE                                                      
         L     RF,=A(PLAREA)                                                    
         ST    RF,VPLAREA                                                       
         L     RF,=A(SORTC)                                                     
         ST    RF,VRSORT                                                        
         L     RF,=V(SPRPFOOT)                                                  
         ST    RF,VFOOT                                                         
         L     RF,=A(SVMDBLK)                                                   
         ST    RF,VSVMDBLK                                                      
         L     RF,=A(GETBUF)                                                    
         ST    RF,VGETBUF                                                       
         L     RF,=A(CALCPP)                                                    
         ST    RF,VCALCPP                                                       
         L     RF,=A(STATOTC)                                                   
         ST    RF,VSTATOT                                                       
         L     RF,=V(REPCALOV)                                                  
         ST    RF,REPCALOV                                                      
         L     RF,=A(EDTDEMSC)                                                  
         ST    RF,VEDTDEMS                                                      
         L     RF,=A(SUBPAREA)                                                  
         ST    RF,VSUBPARA                                                      
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
         L     RE,=A(GETREP)                                                    
         ST    RE,VGETREP                                                       
         L     RE,=A(COMPRNT)                                                   
         ST    RE,VCOMPRNT                                                      
         L     RE,=A(PRDLST)                                                    
         ST    RE,VPRDLST                                                       
         MVI   FIRST,1                                                          
         B     EXIT                                                             
         SPACE 2                                                                
M1       CLI   MODE,MKTFRST                                                     
         BL    M2                                                               
         CLI   ESTACT,0            ANY ESTIMATES FOR PRODUCT                    
         BE    EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M4                                                               
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CANADIAN                          
         JNE   *+8                                                              
         OI    RQOPT2,RQOPT2_NETBUYS                                            
*                                                                               
         BRAS  RE,RQFIRST                                                       
         XC    OPTRPT,OPTRPT                                                    
         SPACE 2                                                                
M2KR     LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         CLI   QPNAME,C' '         TEST PRODUCT LEGEND OPTION SET               
         BH    *+10                                                             
         MVC   QPNAME,PROFPLEG     NO-PICK UP FROM PROFILE                      
         TM    PROFMTR,X'0F'                                                    
         BNZ   *+10                                                             
         MVC   PROFMTR,HALF        SET MARKET TOTAL FROM PROFILE                
*                                                                               
         L     RF,=A(PGRIDC)                                                    
         ST    RF,VPGRID                                                        
         MVC   MRPTTYP,PROFMTR                                                  
         SPACE 2                                                                
         BRAS  RE,RQFRSTA                                                       
         CLC   RTYPE,=C'RS '                                                    
         BNE   *+8                                                              
         MVI   SUBPSW,0                                                         
         MVI   MODE,RUNFRST                                                     
         BAS   R9,GOTOSUB                                                       
         MVI   MODE,REQFRST                                                     
M2A      BAS   R9,GOTOSUB                                                       
         MVI   FIRST,0                                                          
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
M4       CLI   MODE,ESTFRST                                                     
         BNE   M5                                                               
         MVI   ESTACT,1                                                         
         MVI   PASS,0                                                           
         BAS   R9,GOTOSUB                                                       
         SPACE 2                                                                
         BRAS  RE,EFRSTC                                                        
         CLI   SPOTPROF+1,0                                                     
         BNE   *+8                                                              
         MVI   SPOTPROF+1,C'N'                                                  
         CLI   SPOTPROF+5,20                                                    
         BL    *+8                                                              
         MVI   SPOTPROF+5,0                                                     
         SR    RE,RE                                                            
         IC    RE,BPRD                                                          
         CLI   BPRD,X'FF'          GET DEMO NAMES FOR PRODUCT                   
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,28(RE,RF)                                                     
         XC    DNAME1(28),DNAME1                                                
         LA    R9,4                                                             
         L     R8,DEMTABLE                                                      
         SH    R8,=H'7'                                                         
         LA    R5,DNAME1                                                        
GETDNAM  SR    R6,R6                                                            
         IC    R6,0(RE)                                                         
         LTR   R6,R6                                                            
         BZ    GETDNAM1                                                         
         MH    R6,=H'7'                                                         
         LA    RF,0(R6,R8)                                                      
         MVC   0(7,R5),0(RF)                                                    
         LA    R5,7(R5)                                                         
         LA    RE,1(RE)                                                         
         BCT   R9,GETDNAM                                                       
GETDNAM1 B     EXIT                                                             
         SPACE 2                                                                
M5       CLI   MODE,PRDFRST                                                     
         BNE   M6                                                               
         MVI   PASS,0                                                           
         MVI   ESTACT,0            RESET ESTIMATE ACTIVE SWITCH                 
         BAS   R9,GOTOSUB                                                       
         B     EXIT                                                             
         EJECT                                                                  
M6       CLI   MODE,PROCBUY                                                     
         BNE   M7                                                               
         MVI   SORTPASS,1          SET SORT FOR STORE                           
* TEST CONFIRMED ONLY                                                           
         XC    CFDS(2),CFDS                                                     
         TM    BDCFD,1                                                          
         BZ    *+8                                                              
         MVI   CFDE,C')'                                                        
         TM    BDCFD,2                                                          
         BZ    *+8                                                              
         MVI   CFDS,C'('                                                        
         CLI   QPRGTYPE,C' '       PROGRAM TYPE FILTER                          
         BE    *+14                                                             
         CLC   BDPROGT,QPRGTYPE                                                 
         BNE   EXIT                                                             
         SPACE 2                                                                
         SPACE 2                                                                
         BAS   R9,GOTOSUB                                                       
         L     RE,=A(CIGLIST)                                                   
         L     RF,=AL4(CIGLSTLQ)                                                
         XCEF                                                                   
M6A1SRT1 L     RE,VPGRID                                                        
         L     RF,=AL4(PGRIDCLQ)                                                
         XCEF                                                                   
         XC    PGNOENT,PGNOENT                                                  
         XC    HIATAB,HIATAB                                                    
         XC    PREMTAB,PREMTAB                                                  
M6A1SRT2 CLI   SORTREQ,1                                                        
         BNE   M6A1SRT3                                                         
         GOTO1 VRSORT                                                           
         CLI   SORTPASS,1          BUILD PASS                                   
         BE    SORTX                YES - EXIT                                  
         CLI   SORTPASS,3          END OF SORT                                  
         BE    SORTX                YES - EXIT                                  
M6A1SRT3 DS    0H                                                               
         BRAS  RE,CUTIN                                                         
         L     RE,ACISLIST                                                      
         L     RF,=A(CIGLIST)                                                   
BLDCIG   CLI   0(RF),0             END OF SAVED CUTINS                          
         BE    *+12                                                             
         LA    RF,11(RF)                                                        
         B     BLDCIG                                                           
         ST    RF,ACISLIST                                                      
BLDCIG2  CLI   0(RE),0             END OF CUTINS                                
         BE    BLDCIG3                                                          
         MVC   0(11,RF),0(RE)                                                   
         LA    RF,11(RF)                                                        
         LA    RE,11(RE)                                                        
         B     BLDCIG2                                                          
BLDCIG3  DS    0C                                                               
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         BRAS  RE,EXTRCT                                                        
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+12                                                             
         CLI   SPOTPROF+5,2                                                     
         BNE   SORTX                                                            
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         OC    MEDBYD(12),MEDBYD                                                
         BZ    SORTX                                                            
*                                                                               
         BRAS  RE,RNFAX                                                         
*                                                                               
         CLI   IDSW,1              MULTIPLE IDS ON ONE PAGE                     
         BNE   M6A1SR31                                                         
         CLI   FBCONTR,C'Y'        FIRST BUY ON CONTRACT                        
         BNE   M6A1SR31                                                         
         CLI   FORCEHED,C'Y'       HEADLINES WILL SET IT                        
         BE    M6A1SR31                                                         
         LA    RE,MID2             FIND PRINT POSITION                          
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         MVC   0(12,RE),BUYIDNAM                                                
         MVC   13(12,RE),BUYID                                                  
         MVI   FORCEMID,C'Y'                                                    
         SPACE 2                                                                
M6A1SR31 MVI   FBCONTR,C'N'        RESET FIRST BUY ON CONTRACT                  
         CLI   PGCNDSW,0           CONDENSE REQUIRED                            
         BE    M6A1SRT4             NO - PRINT LINE                             
         L     R1,PGNOENT                                                       
         LTR   R1,R1                                                            
         BZ    SORTX                                                            
         BE    M6A1SRT4             NO - PRINT LINE                             
         CLC   PGNOENT,=AL4(PGRIDMXQ)                                           
         BH    M6A1SRT4                                                         
         CLI   SORTREQ,1                                                        
         BNE   *+14                                                             
         CLC   CURRSORT,NEXTSORT                                                
         BE    M6A1SRT2                                                         
M6A1SRT4 CLI   QBOOK1,C' '                                                      
         BNE   M6AA1                                                            
         CLI   DETOPTS,1                                                        
         BNE   M6AA1                                                            
         L     RE,MEDADEMO                                                      
         L     R6,0(RE)                                                         
         USING NDELEM,R6                                                        
         LA    RF,NDEMNO           SET START ADDRESS                            
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               SET END ADDRESS                              
         XC    OVRFLAG(4),OVRFLAG                                               
         LA    RE,OVRFLAG                                                       
M6AA     CR    RF,R6                                                            
         BNL   M6AA1                                                            
         TM    0(RF),X'80'                                                      
         BZ    *+8                                                              
         MVI   0(RE),C'*'                                                       
         LA    RF,4(RF)                                                         
         LA    RE,1(RE)                                                         
         B     M6AA                                                             
M6AA1    DS    0H                                                               
         DROP  R6                                                               
         MVI   BUYACT,1                                                         
M6A      DS    0H                                                               
         DROP  R5                                                               
         GOTO1 =V(VMDBDESC),DMCB,(RA),PRTLINE                                   
         BRAS  RE,GETCAP                                                        
         MVI   CURRSORT,X'FF'                                                   
         LA    R5,PRTLINE                                                       
         L     RF,DDESC            MOVE IN DESCRIPTION                          
         BASR  R9,RF                                                            
         L     R6,DSTAGRID         SET GRID ADDRESS                             
         BAS   RE,CSDEMCP          GET DEMOS                                    
         BAS   R9,BGRID                                                         
         SPACE 2                                                                
         SPACE 2                                                                
         B     SORTX                                                            
         SPACE 2                                                                
SORTX    CLI   SORTREQ,1                                                        
         BNE   EXIT                                                             
         CLI   SORTPASS,2          GET PASS                                     
         BNE   EXIT                 NO - EXIT                                   
         CLC   CURRSORT,NEXTSORT                                                
         BE    M6A1SRT2                                                         
         B     M6A1SRT1                                                         
         EJECT                                                                  
M7       CLI   MODE,STAFRST                                                     
         BNE   M7A                                                              
* WAS NO STAFRST CALL FROM 03 - THIS ADDED FOR STATION NAMES                    
         CLI   PROGPROF+PROFSNAM-PROFDSCT,C'Y'                                  
         BNE   EXIT                                                             
         BRAS  RE,GETSADDR                                                      
         B     EXIT                                                             
* CODE BELOW EXISTED PRE ABOVE BUT NEVER EXECUTED AS NO STAFRST CALL            
         BAS   R9,GOTOSUB                                                       
         BRAS  RE,GETSADDR                                                      
         XC    PDNCNTR,PDNCNTR                                                  
         XC    SSCNTR,SSCNTR                                                    
         L     RE,VPRDLST                                                       
         LA    RF,PRDLSTL                                                       
         XCEFL ,                                                                
         BRAS  RE,RNFAX                                                         
         B     EXIT                                                             
*                                                                               
M7A      CLI   MODE,CBHLAST                                                     
         BNE   M8                                                               
* THERE IS NO TRUE CBHLAST CALL FROM 03 - THIS IS REALLY A STALAST FOR          
* AN INACTIVE NETWORK. ADDED TO ENSURE 02 RESETS ITS STORAGE POINTERS           
* AS PER A STALAST (IF DO STALAST ON NON ACTIVE NWK WE GET A PAGE               
* PRINTING FOR THE NWK WITH ZERO ROTATION SCHEDULE IN IT!)                      
         MVI   PASS,0                1ST PASS FOR NEW STATION                   
         MVC   WORK(12),PASSTAB                                                 
         MVC   PASSQST(12),PASSTAB   BEGIN WITH 1ST PERIOD                      
         B     EXIT                                                             
*                                                                               
M8       CLI   MODE,STALAST                                                     
         BNE   M12                                                              
         CLI   SORTREQ,1           SORT REQUIRED                                
         BNE   M8A                                                              
         MVI   SORTPASS,2           YES - SET SORT PASS FOR EXTRACT             
         MVI   MODE,PROCBUY                                                     
         GOTO1 SORT2                                                            
         MVI   MODE,STALAST                                                     
*                                                                               
M8A      OC    OPTRPT,OPTRPT                                                    
         BZ    M8B                                                              
         MVI   OPTRMODE,STALAST                                                 
         GOTO1 OPTRPT,DMCB,(RA)                                                 
*                                                                               
M8B      DS    0C                                                               
         GOTO1 VSTATOT                                                          
         MVI   FBCONTR,C'Y'        SET FOR MULTI BYID PER PAGE                  
*                                                                               
         CLI   QPNAME,C'Y'         TEST PRODUCT NAME LEGEND REQUIRED            
         BNE   M9                                                               
         L     R4,VPRDLST                                                       
         SR    R8,R8                                                            
         CLI   0(R4),0                                                          
         BE    *+16                                                             
         LA    R4,3(R4)                                                         
         LA    R8,1(R8)                                                         
         B     *-16                                                             
         LTR   R8,R8                                                            
         BZ    M9                                                               
         L     R4,VPRDLST                                                       
         GOTO1 XSORT,DMCB,(R4),(R8),3,3,0                                       
         LA    R8,3(R8)                                                         
         SRL   R8,1                                                             
         LA    R8,3(R8)                                                         
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CR    RE,R8                                                            
         BNL   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   P,0                                                              
         MVC   P2,SPACES                                                        
         GOTO1 REPORT                                                           
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         MVI   P,0                                                              
*---->   MVC   P2(24),=C'*****PRODUCT LEGEND*****'                              
         MVC   P2(L'SP@PROLE),SP@PROLE                                          
         DROP  RE                                                               
         GOTO1 REPORT                                                           
*                                                                               
M8E      MVC   WORK(12),0(R4)                                                   
         LA    R5,P2                                                            
         LA    R8,WORK                                                          
         LA    R0,4                                                             
*                                                                               
M8F      MVC   0(3,R5),0(R8)                                                    
         MVI   3(R5),C'-'                                                       
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PKEY,R6                                                          
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   4(7,R5),=C'UNKNOWN'                                              
         MVC   4(L'SP@UNKN,R5),SP@UNKN                                          
         DROP  RE                                                               
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,0(R8)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   M8G                                                              
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         MVC   4(20,R5),PNAME                                                   
*                                                                               
M8G      LA    R8,3(R8)                                                         
         CLI   0(R8),0                                                          
         BE    *+12                                                             
         LA    R5,26(R5)                                                        
         BCT   R0,M8F                                                           
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
         LA    R4,12(R4)                                                        
         CLI   0(R4),0                                                          
         BNE   M8E                                                              
         L     RE,VPRDLST                                                       
         LA    RF,PRDLSTL                                                       
         XCEFL ,                                                                
*                                                                               
M9       BAS   R9,GOTOSUB                                                       
         CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
         BE    *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         BRAS  RE,RNFAX                                                         
         B     EXIT                                                             
         SPACE 2                                                                
*        TRAP FINAL SORT EXIT                                                   
SORT2    NTR1                                                                   
         B     M6A1SRT1                                                         
         SPACE 2                                                                
         SPACE 2                                                                
M12      CLI   MODE,MKTFRST                                                     
         BNE   M14                                                              
         MVI   BUYACT,0                                                         
M12A     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         BAS   R9,GOTOSUB                                                       
         B     EXIT                                                             
         SPACE 2                                                                
M14      CLI   MODE,MKTLAST                                                     
         BNE   M16                                                              
         CLC   RTYPE,=C'RS '                                                    
         BE    M14A1                                                            
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVI   BUFCDE,X'90'                                                     
         MVI   LEVEL,1                                                          
         MVC   MCOUNT,=F'1'                                                     
         CLI   BUYACT,1                                                         
         BNE   M14B                                                             
         BRAS  RE,MLASTC                                                        
         CLI   MODE,MKTLAST                                                     
         BNE   M14B                                                             
M14A1    BAS   R9,GOTOSUB                                                       
M14B     MVI   PASS,0                                                           
         CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
M16      CLI   MODE,PROCGOAL                                                    
         BNE   M18                                                              
         CLI   BUYACT,1                                                         
         BNE   EXIT                                                             
         BAS   R9,GOTOSUB                                                       
         BRAS  RE,GETGL                                                         
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
M18      CLI   MODE,PRDLAST                                                     
         BNE   M20                                                              
         MVI   BUFCDE,X'90'                                                     
         MVI   LEVEL,2                                                          
         BAS   RE,DOSUM                                                         
         B     EXIT                                                             
*                                                                               
M20      CLI   MODE,MGR1LAST                                                    
         BNE   M22                                                              
         MVI   BUFCDE,X'90'                                                     
         MVI   LEVEL,3                                                          
         BAS   RE,DOSUM                                                         
         B     EXIT                                                             
*                                                                               
M22      CLI   MODE,MGR2LAST                                                    
         BNE   M24                                                              
         MVI   BUFCDE,X'90'                                                     
         MVI   LEVEL,4                                                          
         BAS   RE,DOSUM                                                         
         B     EXIT                                                             
*                                                                               
M24      CLI   MODE,MGR3LAST                                                    
         BNE   M26                                                              
         MVI   BUFCDE,X'90'                                                     
         MVI   LEVEL,5                                                          
         BAS   RE,DOSUM                                                         
         B     EXIT                                                             
         SPACE 2                                                                
M26      CLI   MODE,CLTLAST                                                     
         BNE   M28                                                              
         CLC   QPRD,=C'ALL'                                                     
         BNE   EXIT                                                             
         MVI   BUFCDE,X'91'                                                     
         MVI   LEVEL,2                                                          
         BAS   RE,DOSUM                                                         
         B     M34                                                              
*                                                                               
M28      CLI   MODE,PGR1LAST                                                    
         BNE   M30                                                              
         MVI   BUFCDE,X'91'                                                     
         MVI   LEVEL,3                                                          
         BAS   RE,DOSUM                                                         
         B     M34                                                              
*                                                                               
M30      CLI   MODE,PGR2LAST                                                    
         BNE   M32                                                              
         MVI   BUFCDE,X'91'                                                     
         MVI   LEVEL,4                                                          
         BAS   RE,DOSUM                                                         
         B     M34                                                              
M32      CLI   MODE,PGR3LAST                                                    
         BNE   M34                                                              
         MVI   BUFCDE,X'91'                                                     
         MVI   LEVEL,5                                                          
         BAS   RE,DOSUM                                                         
         B     M34                                                              
M34      CLI   MODE,REQLAST                                                     
         BNE   M36                                                              
*>>> FIXED RN03 JAN2002                                                         
*>>> NO! GOTO1 M342                SET UP STATION LAST RETURN                   
         MVI   MODE,REQLAST                                                     
         CLI   FOOT1,C' '                                                       
         BE    M36                                                              
         MVI   FORCEHED,C'N'                                                    
         MVI   P,0                                                              
         MVC   P2(132),FOOT1                                                    
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     M36                                                              
         SPACE 2                                                                
M342     NTR1                      DO STATION LAST                              
         MVI   MODE,STALAST                                                     
         B     M8                                                               
         SPACE 2                                                                
M36      BAS   R9,GOTOSUB                                                       
EXIT     XMOD1 1                                                                
         EJECT                                                                  
GOTOSUB  CLI   PASS,0              PASS = 0                                     
         BNER  R9                   NO - BYPASS SUBPROGRAM                      
         CLI   SUBPSW,0                                                         
         BNE   *+10                                                             
         CLI   MODE,ESTFRST                                                     
         BHR   R9                                                               
         MVC   SVOPTS,QOPT1                                                     
         MVC   QOPT1(7),MSOPT                                                   
         MVC   SVPROF,PROGPROF                                                  
         MVC   PROGPROF,MSPROF                                                  
         MVC   SVSPECS,SPECS       GO TO SUBPROGRAM                             
         MVC   SVSUPMKT,SPSUPMKT                                                
         MVC   SVMDTAB,MEDTABLE                                                 
         MVC   SPECS,SVPH01        SET SPECS                                    
         MVC   MEDTABLE,SVPH04                                                  
         CLI   MODE,MKTLAST                                                     
         BL    GOTOSUB1                                                         
         MVI   FORCEMID,C'N'                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVI   FORCEHED,C'Y'                                                    
GOTOSUB1 MVC   SVRCSUB,RCSUBPRG                                                 
         L     RF,MEDBUFF          RESTORE MEDIA SUMMARY DATES                  
         L     RE,VSVMDBLK                                                      
         LA    R1,1208                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   SVSPHK,SPOTHOOK                                                  
         MVC   SPOTHOOK,MSSPHK                                                  
         MVC   SVHDHOOK,HEADHOOK                                                
         MVC   HEADHOOK,MSHDHOOK                                                
         MVC   RCSUBPRG,MSRCSUB                                                 
         MVC   SPSUPMKT,MSSUPMKT                                                
         L     RF,SVPH02                                                        
         GOTO1 (RF),DMCB,(RA)                                                   
         MVC   MSOPT,QOPT1                                                      
         MVC   QOPT1(7),SVOPTS                                                  
         MVC   MSSUPMKT,SPSUPMKT                                                
         MVC   SPSUPMKT,SVSUPMKT                                                
         MVC   PROGPROF,SVPROF                                                  
         L     RE,MEDBUFF                                                       
         L     RF,VSVMDBLK                                                      
         LA    R1,1208                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   MSSPHK,SPOTHOOK                                                  
         MVC   SPOTHOOK,SVSPHK                                                  
         MVC   MSHDHOOK,HEADHOOK                                                
         MVC   MSRCSUB,RCSUBPRG                                                 
         MVC   RCSUBPRG,SVRCSUB                                                 
         MVC   HEADHOOK,SVHDHOOK                                                
         MVC   SPECS,SVSPECS                                                    
         MVC   MEDTABLE,SVMDTAB                                                 
         BR    R9                                                               
         EJECT                                                                  
* BUILD WEEKLY GRID AND SUM INTO STATION BUCKETS  R6=PRINT POSITION             
BGRID    L     R5,MEDAFRST                                                      
         LA    R8,STAGRID                                                       
         ST    R6,FULL             SAVE PRINT LINE ADDRESS                      
BGRID2   L     R4,4(R5)                                                         
         OC    0(4,R5),0(R5)                                                    
         BZ    BGRID5                                                           
BGRID4   L     RE,0(R8)            SUM WEEKLY SPOTS                             
         A     RE,MEDBYSPT                                                      
         ST    RE,0(R8)                                                         
         LA    R6,4(R6)                                                         
         LA    R8,4(R8)                                                         
         LA    R5,12(R5)                                                        
         B     BGRID2                                                           
BGRID5   CLI   MODE,PROCBUY        PRINT PREMPTIONS                             
         BNE   BGRIDX                                                           
         GOTO1 =A(PTSGRID),DMCB,(LENGRID,(RA))                                  
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(2,PASSSD2)                                   
         GOTO1 (RF),(R1),QEND,(2,PASSED2)                                       
         BRAS  RE,PRCOSTOV                                                      
BGRIDX   BR    R9                                                               
         EJECT                                                                  
         USING BDEXTD,R5                                                        
PTSDESC  MVC   P1(L'BDPEST),BDPEST      SET BUY DESCRIPTION                     
         MVC   P2(L'BDPLIN),BDPLIN                                              
         MVC   P1+4(L'BDPBDATE),BDPBDATE                                        
         MVC   P1+3(1),CFDS                                                     
         MVC   P1+15(1),CFDE                                                    
         MVC   P1+16(L'BDPWKS),BDPWKS                                           
         MVC   P1+22(L'BDPDAY),BDPDAY                                           
         MVC   P1+30(L'BDPNPWK),BDPNPWK                                         
         MVC   P2+4(L'BDPTIME),BDPTIME                                          
         MVC   P2+17(L'BDPDPT),BDPDPT                                           
         MVC   P2+21(L'BDPSLN),BDPSLN                                           
         MVC   P3+4(L'BDPPROG),BDPPROG                                          
         MVC   P3+22(L'BDPPTYP),BDPPTYP                                         
         MVC   P3+25(9),BDPCOST+3                                               
         OC    SVSPREP,SVSPREP                                                  
         BZ    PTSDESC1                                                         
*---->   MVC   P4(21),=C'***SPECIAL REP=   ***'                                 
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         MVC   P4(L'SP@SPREP),SP@SPREP                                          
         DROP  RE                                                               
         EDIT  SVSPREP,(3,FULL)                                                 
         MVC   P4+15(3),FULL                                                    
PTSDESC1 DS    0H                                                               
         CLI   QPROG,C'U'                                                       
         BNE   *+10                                                             
         MVC   P2+3(L'BDPAST),BDPAST                                            
         CLI   PKGAREA,0                                                        
         BE    *+14                                                             
         MVC   P4+31(16),PKGAREA                                                
         MVI   P5,0                                                             
         SPACE 2                                                                
* FIND DEMO ELEMENT AND EXTRACT BOOK AND NAME                                   
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING NDELEM,RE                                                        
PTSDSC2  CLI   NDCODE,2                                                         
         BE    PTSDSC2A                                                         
         ZIC   R0,NDLEN                                                         
         AR    RE,R0                                                            
         B     PTSDSC2                                                          
PTSDSC2A MVC   HLDPNAM,NDPROG                                                   
         CLI   NDBOOK,0                                                         
         BE    PTSDSC2B                                                         
         GOTO1 DATCON,DMCB,(X'03',NDBOOK),(X'09',HLDBOOK)                       
PTSDSC2B DS    0H                                                               
         MVC   SVP1,P1                                                          
         MVC   SVP2,P2                                                          
         MVC   SVP3,P3                                                          
         MVC   SVP4,P4                                                          
         DROP  RE                                                               
         SPACE 2                                                                
         BR    R9                                                               
*              PRINT STATION ROTATION DESCRIPTION                               
PRSDESC  MVC   P1(L'BDPDAY),BDPDAY                                              
         MVC   P1+9(L'BDPTIME),BDPTIME                                          
         MVC   P1+21(L'BDPPROG),BDPPROG                                         
         MVC   P1+37(L'BDPSLN),BDPSLN                                           
         CLI   DETOPTS+2,0         TEST SUPPRESS COSTS                          
         BE    *+10                                                             
         MVC   P2+27(9),BDPCOST+3                                               
         MVC   SVP2(40),P2                                                      
         MVC   SVP1(40),P1                                                      
         BR    R9                                                               
         EJECT                                                                  
* PRINT DEMOS AND CPP FOR BRS,BTS,BDS,SAL                                       
BTSPD    LA    R8,P1               SET PRINT LINE                               
         CLI   MODE,STALAST                                                     
         BNE   *+8                                                              
         LA    R8,P2                                                            
         CLI   DETOPTS+1,1         DEMOS REQUESTED                              
         BNE   BTSPDX                                                           
         LA    R6,2                                                             
         ST    R8,FULL                                                          
         LA    RE,PLD1                                                          
         LA    RF,OVRFLAG                                                       
         LA    R1,DNAMES                                                        
PTSPD1   MVC   101(5,R8),0(RE)     MOVE DEMOS TO PRINT LINE                     
         MVC   106(1,R8),0(RF)                                                  
         MVC   93(7,R8),0(R1)                                                   
         MVC   113(7,R8),7(R1)                                                  
         MVC   126(1,R8),1(RF)                                                  
         MVC   120(5,R8),11(RE)                                                 
         LA    RE,22(RE)                                                        
         LA    RF,2(RF)                                                         
         LA    R1,14(R1)                                                        
         LA    R8,132(R8)                                                       
         BCT   R6,PTSPD1                                                        
         MVC   113(12,R8),HLDPNAM                                               
         MVC   125(6,R8),HLDBOOK                                                
         SPACE 2                                                                
         L     R8,FULL                                                          
         LA    R6,2                                                             
         LA    RE,PLD1CP+1                                                      
PTSPD2   CLI   DETOPTS+3,1                                                      
         BNE   BTSPDX                                                           
PTSPD3   MVC   107(5,R8),0(RE)                                                  
         MVC   127(5,R8),11(RE)                                                 
         LA    R8,132(R8)                                                       
         LA    RE,22(RE)                                                        
         BCT   R6,PTSPD3                                                        
BTSPDX   BR    R9                                                               
         EJECT                                                                  
* CALCULATE AND SAVE DEMOS AND CPP/CPM                                          
CSDEMCP  NTR1                                                                   
         LA    R5,MEDPERD                                                       
         L     R4,4(R4)                                                         
         L     R4,4(R5)                                                         
         XC    SVD1(32),SVD1                                                    
         OC    MEDBYD(12),MEDBYD                                                
         BZ    CSDEMCPX                                                         
         L     RE,STASPOT                                                       
         A     RE,MEDBYSPT                                                      
         ST    RE,STASPOT                                                       
         L     RE,STACOST                                                       
         A     RE,MEDBYD                                                        
         ST    RE,STACOST                                                       
         L     RE,STACOST+4                                                     
         A     RE,MEDBYDEQ                                                      
         ST    RE,STACOST+4                                                     
         LA    R0,8                                                             
         LA    RF,STADEMS                                                       
         LA    R6,MEDBY1                                                        
CSSTA    L     RE,0(RF)                                                         
         A     RE,0(R6)                                                         
         ST    RE,0(RF)                                                         
         LA    RF,4(RF)                                                         
         LA    R6,4(R6)                                                         
         BCT   R0,CSSTA                                                         
         XC    PRTLINE,PRTLINE                                                  
         LA    R3,PRTLINE                                                       
         MVC   SUMDL(8),MEDBYD                                                  
         MVC   SUMD1(32),MEDBY1                                                 
         GOTO1 VCALCPP,DMCB,MEDBYSPT                                            
CSDEMCPX XIT1                                                                   
         EJECT                                                                  
* DO SUMMARIES FOR VARIOUS BREAKS                                               
DOSUM    NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         CLI   SPDUPTOT,C'Y'                                                    
         BE    DOSUM2                                                           
         MVC   WEIGHT,SPWEIGHT                                                  
***NOP   GOTO1 VSUMMRY                                                          
DOSUM2   SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         L     R8,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R8)),(X'80',(R9))                
         MVI   FORCEHED,C'Y'                                                    
         BAS   R9,GOTOSUB                                                       
         MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
         EJECT                                                                  
* HEADLINE ROUTINES                                                             
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SP60RB                                                      
         DROP  RF                                                               
         L     R2,SP60R2                                                        
         LM    RA,RC,SP60RA                                                     
MYHDNOW  DS    0H                                                               
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         CLI   MODE,MGR1LAST                                                    
         BNH   *+10                                                             
*---->   MVC   H7+51(20),=C'**** SUMMARY ****   '                               
         MVC   H7+51(L'SP@SUM),SP@SUM                                           
         DROP  RE                                                               
         BRAS  RE,PRSHEAD                                                       
         B     MYHEADX                                                          
MYHEAD4  DS    0H                                                               
MYHEADX  XIT1                                                                   
SP60RA   DC    F'0'                                                             
SP60RB   DC    F'0'                                                             
SP60RC   DC    F'0'                                                             
SP60R2   DC    F'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
RNFAX    NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
*                                                                               
         CLC   QPROG,=C'RX'        CHECK IT'S THE RX REPORT                     
         BNE   DXFAXX                                                           
*                                                                               
         L     R4,VFAXINFO         ADDRESS FAXLINK INFO BLOCK                   
         USING FAXINFOD,R4                                                      
*                                                                               
         CLI   MODE,STALAST        TEST END OF STATION                          
         BNE   DXFAX2                                                           
         TM    NXSW,NXOPEN         TEST FAX OPEN                                
         BZ    DXFAXX              NO                                           
         MVI   FXISTAT,FXISCLOS    YES-CALL FAXLINK TO CLOSE                    
         GOTO1 VFAXLINK                                                         
         NI    NXSW,255-NXOPEN     RESET OPEN FLAG                              
         B     DXFAXX                                                           
*                                                                               
DXFAX2   TM    NXSW,NXOPEN         TEST ALREADY OPEN                            
         BO    DXFAXX                                                           
*                                                                               
         OI    NXSW,NXOPEN         SET TO OPEN NOW                              
         NI    NXSW,255-NXNEWSTA                                                
*                                                                               
         XC    FAXINFOD(FXILEN),FAXINFOD   SET UP FAXINFO BLOCK                 
         MVI   FXISTAT,FXISPEND    PENDING INITIALIZATION                       
         L     R1,VMASTC                                                        
         MVC   FXISIDNO,MCORIGID-MCBLOCK(R1)   ORIGINATING ID                   
         MVC   FXISAGY,QAGY                                                     
         MVC   FXISFXCD,SPACES                                                  
         MVC   FXISFXCD(5),STA     FAX REC CODE = STATION CALL LETTERS          
         CLI   STA+3,C' '                                                       
         BH    *+16                                                             
         CLI   STA+4,C' '                                                       
         BNH   *+8                                                              
         MVI   FXISFXCD+3,C'-'                                                  
         MVC   FXISRQST,SVQUEST    REQUESTOR CODE                               
         MVC   FXISTN,STA          DESTINATION IS STATION                       
         CLI   STA+4,C' '                                                       
         BH    *+8                                                              
         MVI   FXISTN+4,C'N'                                                    
         CLI   STA+3,C' '                                                       
         BH    *+8                                                              
         MVI   FXISTN+3,C'9'                                                    
         CLI   FXISTN,C'0'         TEST CABLE                                   
         BL    *+14                                                             
         MVI   FXISTN,C'T'         YES-MAKE IT TNNNN                            
         MVC   FXISTN+1(4),STA                                                  
         OI    FXIOPTS,FXIOEASY                                                 
         MVC   FXITRN(14),RXTRAN                                                
         LA    R1,FXITRAPP         APPLICATION TRANSACTION DATA                 
         BAS   RE,DXFMTAPP                                                      
         LA    R1,FXIEBILL         EASYLINK BILLING INFORMATION                 
         BAS   RE,DXFMTBIL                                                      
         GOTO1 VFAXLINK            CALL FAXLINK                                 
         CLI   FXISTAT,FXISACT     TEST FAX RECORD FOUND                        
         BE    DXFAXX              YES-DONE                                     
         MVI   FXISTAT,FXISACT     NO-MARK ACTIVE SO CLOSE WILL OCCUR           
*                                                                               
         LA    R1,P                AND WE'LL DO IT OURSELVES                    
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(5,R1),STA         EASYLINK'S STATION DESTINATION               
         MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVI   35(R1),C'P'         PAGE BREAKS                                  
         MVC   38(5,R1),STA      FORMATTED DESTINATION NAME                     
         LA    R1,54(R1)                                                        
         BAS   RE,DXFMTBIL                                                      
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         GOTO1 REPORT                                                           
         MVC   P(14),RXTRAN                                                     
         LA    R1,P+15                                                          
         BAS   RE,DXFMTAPP                                                      
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         GOTO1 REPORT                                                           
         MVI   SKIPSPEC,C'N'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   FORCEMID,C'Y'                                                    
*                                                                               
DXFAXX   XIT1                                                                   
         SPACE 2                                                                
DXFMTBIL DS    0H                  FORMAT EASYLINK BILLING INFO                 
         MVC   0(1,R1),QMED                                                     
         MVC   1(3,R1),QCLT                                                     
         BR    RE                                                               
         SPACE 2                                                                
DXFMTAPP NTR1                      FORMAT APPLICATION PART OF ++DDS REC         
         USING SPEDICTD,R1                                                      
         MVI   SPCPTYPE,SPCPDATQ                                                
         MVC   SPCPMED,MED                                                      
         MVC   SPCPCLT,CLT                                                      
         MVC   SPCPPRD,PRD                                                      
         MVC   SPCPEST,EST                                                      
         MVC   SPCPMKT,MKT                                                      
         MVC   SPCPSTA,STA                                                      
         MVC   SPCPRQST,SVQUEST                                                 
         LA    R2,SPCDRUSR+15                                                   
         GOTO1 DATCON,DMCB,(2,MEDPERD),(7,0(R2))                                
         MVI   5(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,MEDPERD+2),(7,6(R2))                              
         XIT1                                                                   
         DROP  R1                                                               
         SPACE 2                                                                
* - RX REPORT IS SR FOR $ETI PURPOSES                                           
RXTRAN   DC    CL14'++DDS SPRRXTRN'                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
       ++INCLUDE DDFAXINFOD                                                     
SPEDICTD DSECT                                                                  
       ++INCLUDE SPEDICT                                                        
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
         LTORG                                                                  
         EJECT                                                                  
SPRN02   CSECT                                                                  
*                                                                               
RQFIRST  NTR1  BASE=*,LABEL=*                                                   
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         MVC   SVQUEST,QUESTOR                                                  
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
*---->   MVC   STACAP(7),=C'STATION'                                            
         MVC   STACAP(L'SP@STATN),SP@STATN                                      
         DROP  R5                                                               
         MVI   QCOMPARE,C'A'                                                    
         CLI   QBOOK1,C' '                                                      
         BE    RQ1                                                              
         CLI   QRERATE,C'I'                                                     
         BNE   *+8                                                              
         MVI   QCOMPARE,C'B'                                                    
RQ1      DS    0H                                                               
         MVC   MAXLINES,SVMAXLIN                                                
         GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   FOOT1,C' '                                                       
         BE    RQ1NOFT                                                          
         ZIC   R0,MAXLINES                                                      
         SH    R0,=H'3'                                                         
         STC   R0,MAXLINES                                                      
         MVC   FOOT1,SPACES                                                     
RQ1NOFT  DS    0H                                                               
         SPACE 2                                                                
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         XC    SPOTPROF,SPOTPROF                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   PASS,0                                                           
         MVC   RTYPE,=C'RS'                                                     
         MVI   RTYPE+2,C' '                                                     
         LA    R1,H11+35                                                        
         ST    R1,AHDATES                                                       
         MVI   SPACESW,1                                                        
         L     RF,APTSDESC                                                      
         ST    RF,DDESC                                                         
         LA    RF,P1+34                                                         
         ST    RF,DSTAGRID                                                      
         LA    RF,P2+31                                                         
         ST    RF,PSTASPT                                                       
         LA    RF,P2+101                                                        
         ST    RF,PSTACOST                                                      
         LA    RF,P2+31                                                         
         ST    RF,PSTAGRID                                                      
         MVI   PENNYSW,1                                                        
         MVC   NUMWK,=F'60'                                                     
         CLI   QOPT2,C' '                                                       
         BE    *+10                                                             
         MVC   PROFFRMT,QOPT2                                                   
         CLI   QOPT3,C' '                                                       
         BE    *+10                                                             
         MVC   PROFCNDS,QOPT3                                                   
         LA    RE,PROGPROF                                                      
         SPACE 2                                                                
         LA    RF,DATEOPTS         SET UP OPTIONS                               
RQPR1    CLI   0(RF),X'FF'         DATE OPTIONS                                 
         BE    RQPR1X                                                           
         CLC   0(1,RF),PROFDCTL                                                 
         BE    RQPR1X                                                           
         LA    RF,3(RF)                                                         
         B     RQPR1                                                            
RQPR1X   MVC   DATEOPT,1(RF)                                                    
         SPACE 2                                                                
******** LA    RF,SORTOPTS                                                      
*RQPR2   CLI   0(RF),X'FF'         SORT OPTIONS                                 
******** BE    RQPR2X                                                           
******** CLC   0(1,RF),PROFSORT                                                 
******** BE    RQPR2X                                                           
******** LA    RF,2(RF)                                                         
******** B     RQPR2                                                            
*RQPR2X  MVC   SORTOPT,1(RF)                                                    
*                                                                               
         MVI   SORTOPT,0           NOT PROFILE DRIVEN                           
         SPACE 2                                                                
         LA    RF,FRMTOPTS                                                      
RQPR3    CLI   0(RF),X'FF'         FORMAT OPTIONS                               
         BE    RQPR3X                                                           
         CLC   0(1,RF),PROFFRMT                                                 
         BE    RQPR3X                                                           
         LA    RF,4(RF)                                                         
         B     RQPR3                                                            
RQPR3X   MVC   FRMTOPT,1(RF)                                                    
         SPACE 2                                                                
         LA    RF,CNDSOPTS                                                      
RQPR4    CLI   0(RF),X'FF'                                                      
         BE    RQPR4X                                                           
         CLC   0(1,RF),PROFCNDS                                                 
         BE    RQPR4X                                                           
         LA    RF,2(RF)                                                         
         B     RQPR4                                                            
RQPR4X   MVC   CNDSOPT,1(RF)                                                    
         MVI   CNDSOPT,0           SUPPRESS THIS OPTION !                       
         EJECT                                                                  
*        SET UP OPTON SWITCHS                                                   
         MVC   VARFRMT,DATEOPT                                                  
         MVC   SCNDDTSW,DATEOPT+1                                               
         MVC   SORTFRMT,SORTOPT                                                 
         MVI   SORTREQ,0                                                        
         CLI   SORTFRMT,0                                                       
         BE    *+8                                                              
         MVI   SORTREQ,1                                                        
         MVI   SORTPASS,1                                                       
         MVC   LENGRID,FRMTOPT                                                  
         XC    NOINGRID,NOINGRID                                                
         MVC   NOINGRID+1(1),FRMTOPT+1                                          
         MVC   PGCNDSW,CNDSOPT                                                  
         EJECT                                                                  
         L     RF,APRSDESC                                                      
         ST    RF,DDESC                                                         
         LA    RF,P1+41                                                         
         ST    RF,DSTAGRID                                                      
         MVC   NUMWK,=F'60'                                                     
         XC    NOINGRID,NOINGRID                                                
         MVC   NOINGRID+1(1),FRMTOPT+2                                          
         SPACE 2                                                                
         SPACE 2                                                                
M2RPTX   DS    0H                                                               
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
RQFRSTA  NTR1  BASE=*,LABEL=*                                                   
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
*                                                                               
         MVC   DETOPTS,=X'01010001'  SET DETAIL OPTIONS (COST=NO)               
         CLI   QOPT1,C' '                                                       
         BE    M21A                                                             
         CLI   QOPT1,C'Y'                                                       
         BNE   *+8                                                              
         MVI   DETOPTS+2,1         SET TO PRINT COSTS                           
*                                                                               
M21A     MVC   SUMOPTS,=X'010101'  SET SUMMARY OPTIONS                          
         CLI   QOPT5,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT5,PROFMPC       MARKET PRINT CONTROL                         
         TM    QOPT5,X'F0'                                                      
         BNO   M2AA                                                             
         PACK  DUB,QOPT5                                                        
         CVB   R6,DUB                                                           
         MHI   R6,3                                                             
         LA    R6,SUMOPT(R6)                                                    
         MVC   SUMOPTS,0(R6)                                                    
M2AA     DS    0H                                                               
         MVI   SUBPSW,0                                                         
         MVC   MSOPT,SPACES        INITIALIZE MS OPTIONS                        
         MVC   SVOPTS,QOPT1        SAVE REPORT OPTIONS                          
         MVC   SUBPROG1,=C'M2'                                                  
         CLC   QPRD,=C'POL'        POL REQUEST                                  
         BNE   M2AA1                NO                                          
         TM    PROFPMS,X'0F'        YES - ANY MEDIA SUMMARY                     
         BZ    M2AA3                                                            
         MVC   SUBPROG1+1(1),PROFPMS                                            
         B     M2AA2                                                            
M2AA1    TM    PROFBMS,X'0F'                                                    
         BZ    M2AA3                                                            
         MVC   SUBPROG1+1(1),PROFBMS                                            
M2AA2    MVI   SUBPSW,1                                                         
M2AA3    MVC   WORK(12),=CL12'S000'   READ MS PROFILE                           
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVC   WORK+2(2),SUBPROG1                                               
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(L'COFFICE),COFFICE                                       
         DROP  R6                                                               
         DROP  RE                                                               
         GOTO1 GETPROF,DMCB,WORK,MSPROF,DATAMGR                                 
         CLI   QRERATE,C'I'        FIX DEFAULT DATA COMPARE                     
         BNE   AFFCOMP                                                          
         CLI   MSPROF,C'B'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'A'                                                      
         CLI   MSPROF,C'D'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'C'                                                      
         CLI   MSPROF,C'F'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'E'                                                      
         B     COMPOK                                                           
         SPACE 2                                                                
AFFCOMP  CLI   MSPROF,C'A'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'B'                                                      
         CLI   MSPROF,C'C'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'D'                                                      
         CLI   MSPROF,C'E'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'F'                                                      
COMPOK   DS    0H                                                               
         SPACE 2                                                                
         MVC   SUBPROG2,=C'01'                                                  
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         L     RE,DMCB+4                                                        
         ST    RE,SVPH01                                                        
         MVC   SUBPROG2,=C'02'                                                  
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         L     RE,DMCB+4                                                        
         ST    RE,SVPH02                                                        
         MVC   SUBPROG2,=C'04'                                                  
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         L     RF,VSVMDBLK                                                      
         L     RE,MEDBUFF                                                       
         LA    R1,980                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   SVPH04,DMCB+4                                                    
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
EFRSTC   NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         MVC   REASTART,QSTART                                                  
         MVC   PASSQST(12),QSTART                                               
         MVC   PASSTAB(12),QSTART                                               
* SET NUMBER OF LEVELS                                                          
         LA    RF,LVCNTRL                                                       
         LA    RE,5                                                             
         NI    0(RF),X'7F'                                                      
         LA    RF,1(RF)                                                         
         BCT   RE,*-8                                                           
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         L     RE,BUFFROWS                                                      
         BCTR  RE,0                                                             
         MH    RE,=H'4'                                                         
         LA    RE,LVCNTRL(RE)                                                   
         OI    0(RE),X'80'                                                      
         DROP  RF                                                               
         SPACE 2                                                                
* CREATE WEEKLY TABLES FOR ALL REPORTS                                          
         MVC   MEDNUMWK,=F'60'     CREATE BTS TABLES                            
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMQT,=F'4'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
EFRSTA   DS    0H                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   SVRDTE,MEDPERD      SAVE REQUEST DATES                           
         MVI   PASS,0                                                           
         MVI   MAXPASS,1                                                        
         CLI   VARFRMT,0           FIXED FORMAT                                 
         BNE   EFRSTX               NO - EXIT                                   
         LA    RE,1                                                             
         LA    R6,PASSTAB                                                       
         L     R9,MEDAFRST                                                      
SETPASS  STC   RE,MAXPASS          SAVE HIGHEST PASS                            
         LH    R8,NOINGRID         SET TO NUMBER OF WEEKS IN PASS               
         GOTO1 DATCON,DMCB,(X'02',(R9)),(X'00',0(R6))                           
SETPASS1 GOTO1 DATCON,DMCB,(X'02',2(R9)),(X'00',6(R6))                          
SETPASS2 LA    R9,12(R9)                                                        
         C     R9,MEDALAST                                                      
         BH    SETPASSX                                                         
         CLI   0(R9),0                                                          
         BE    SETPASS2                                                         
         BCT   R8,SETPASS1                                                      
         ZIC   RE,MAXPASS                                                       
         LA    RE,1(RE)            BUMP MAXPASS                                 
         LA    R6,12(R6)           BUMP DATE SAVE                               
         B     SETPASS                                                          
SETPASSX MVC   PASSQST(12),PASSTAB                                              
EFRSTX   MVC   QSTART(12),PASSQST                                               
*                                                                               
* INITIALIZE RN PROFILES                                                        
         MVI   IDSW,0              DEFAULT IS PAGE THROW PER ID                 
         CLI   QBYID,C'Y'          ID SEQUENCE REQUESTED                        
         BNE   M4A                                                              
         CLI   PROGPROF+14,C'N'    PAGE BREAK AT CONTRACT                       
         BNE   M4A                                                              
         MVI   IDSW,1              TURN ON MULTIPLE IDS ON PAGE                 
M4A      EQU   *                                                                
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET SPREPD202  AT LEVEL 086 AS OF 08/14/09                      
         EJECT                                                                  
GETCAP   NTR1  BASE=*,LABEL=*                                                   
         USING SP60WK,R2                                                        
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING PKGELEM,R5                                                       
         XC    PKGAREA,PKGAREA                                                  
*                                                                               
GETCAP1  CLI   PKGCODE,5                                                        
         BE    GETCAP2                                                          
         CLI   PKGCODE,0                                                        
         BE    GETCAP4                                                          
         SR    RE,RE                                                            
         IC    RE,PKGLEN                                                        
         AR    R5,RE                                                            
         B     GETCAP1                                                          
*                                                                               
GETCAP2  DS    0H                                                               
         MVC   BYTE,PKGIND                                                      
         NI    BYTE,X'0F'          DROP2-BYTE LINE FLAG                         
         CLI   BYTE,1                                                           
         BNE   *+14                                                             
         MVC   PKGAREA(7),=C'PKG MST'                                           
         B     GETCAPX                                                          
*                                                                               
         CLI   BYTE,3                                                           
         BNE   *+14                                                             
         MVC   PKGAREA(7),=C'ORB MST'                                           
         B     GETCAPX                                                          
*                                                                               
         CLI   BYTE,5                                                           
         BNE   *+14                                                             
         MVC   PKGAREA(7),=C'REV MST'                                           
         B     GETCAPX                                                          
*                                                                               
         MVC   PKGAREA(4),=C'MST='                                              
         LLC   R0,PKGLINES                                                      
         TM    PKGIND,X'10'        TEST 2-BYTE LINE NUMBERS                     
         BZ    *+8                                                              
         ICM   R0,3,PKGLINES                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PKGAREA+4(3),DUB                                                 
         B     GETCAPX                                                          
*                                                                               
GETCAP4  L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING BDELEM,RE                                                        
         CLI   BDMGDATE,X'C0'                                                   
         BNH   GETCAPX                                                          
         MVC   PKGAREA(7),=C'**MKGD '                                           
         MVC   PKGAREA+7(6),=C'GROUP '                                          
         MVC   PKGAREA+13(2),BDMGDATE                                           
         MVI   PKGAREA+15,C'*'                                                  
*                                                                               
GETCAPX  DS    0H                                                               
         DROP  R5,RE                                                            
         EJECT                                                                  
GETCOM1  L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING COMELEM,R5                                                       
         LA    RE,COMAREA                                                       
         LA    RF,L'COMAREA                                                     
         XCEF                                                                   
GETCOM2  CLI   CMCODE,0                                                         
         BE    GETCOMX                                                          
         CLI   CMCODE,X'66'                                                     
         BE    GETCOM4                                                          
GETCOM3  SR    R0,R0                                                            
         IC    R0,CMLEN                                                         
         AR    R5,R0                                                            
         B     GETCOM2                                                          
*                                                                               
GETCOM4  LA    R7,PROGPROF                                                      
         USING PROFDSCT,R7                                                      
         CLI   PROFCC,C'1'                                                      
         BNE   *+12                                                             
         CLI   CMDATA,C'$'                                                      
         BNE   GETCOM3                                                          
         CLI   PROFCC,C'2'                                                      
         BNE   *+14                                                             
         CLC   CMDATA(8),=C'COMMENT-'                                           
         BNE   GETCOM3                                                          
         DROP  R7                                                               
         LA    R4,COMAREA                                                       
         CLI   CMNUM,5             GET COMMENT SLOT                             
         BH    GETCOM3                                                          
         SR    R7,R7                                                            
         IC    R7,CMNUM                                                         
         BCTR  R7,0                                                             
         MH    R7,=H'80'                                                        
         AR    R4,R7                                                            
         SR    R7,R7                                                            
         IC    R7,CMLEN                                                         
         SH    R7,=H'4'                                                         
         LTR   R7,R7                                                            
         BM    GETCOM3                                                          
         EX    R7,*+8                                                           
         B     GETCOM3                                                          
         MVC   0(0,R4),CMDATA                                                   
         DROP  R3                                                               
GETCOMX  XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
GETSADDR NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         MVC   SAKYSAVE,KEY                                                     
         LA    R6,KEY                                                           
         USING ADDRREC,R6                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   ADDKTYPE,C'A'                                                    
         MVC   ADDKMED,QMED                                                     
         MVC   ADDKCALL,STA                                                     
         CLI   ADDKCALL+4,C' '                                                  
         BNE   *+10                                                             
         MVC   ADDKCALL+4(1),QMED                                               
         CLI   QMED,C'C'                                                        
         BNE   *+8                                                              
         MVI   ADDKCALL+4,C'C'                                                  
         MVC   ADDKAGY,AGY                                                      
         MVI   DMOUTBTS,0          SET NO ERROR ON REC NOT FOUND                
         GOTO1 READSTAD                                                         
         MVI   DMOUTBTS,X'FF'                                                   
         L     RF,ADSTATAD                                                      
         TM    DMCB+8,X'10'        TEST NO ADDRESS RECORD BECAUSE               
         BZ    *+10                STUPID ROUTINE PASSES BACK NEXT REC          
         MVC   0(L'ADDRREC,RF),SPACES     SO CLEAR IT OUT                       
         MVC   STADDRNM,SPACES     PRIME NAME TO PRINT                          
         CLI   ANAME-ADDRREC(RF),C' '                                           
         BE    M84                 START WITH SPACE MEANS DON'T PRINT           
         MVC   STADDRNM(4),STA     CHECK NAME SAME AS CALL LETTERS              
         OC    ANAME-ADDRREC(L'ANAME,RF),SPACES                                 
         CLC   STADDRNM,ANAME-ADDRREC(RF)                                       
         MVC   STADDRNM,ANAME-ADDRREC(RF)     SET TRUE NAME                     
         BNE   *+10                                                             
         MVC   STADDRNM,SPACES     DON'T PRINT SAME TWICE                       
         B     M84                                                              
*82      GOTO1 VGETREP             GET REP ADDRESS                              
M84      DS    0H                                                               
*84MID   CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
*        BE    M8EXIT                                                           
*        MVI   FORCEHED,C'Y'                                                    
M8EXIT   MVC   KEY,SAKYSAVE                                                     
         XMOD1 1                                                                
         DROP  R5                                                               
         LTORG                                                                  
         DROP  R6                                                               
SAKYSAVE DS    CL32                                                             
         EJECT                                                                  
COMPRNT  NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         LA    R4,COMAREA                                                       
         LA    R5,5                                                             
         L     R6,FULL                                                          
         CLI   0(R6),0                                                          
         BNE   *+8                                                              
         MVI   0(R6),C' '                                                       
         CLC   1(50,R6),0(R6)                                                   
         BE    COMPRNT1                                                         
         LA    R6,132(R6)                                                       
         B     *-14                                                             
COMPRNT1 OC    0(76,R4),0(R4)                                                   
         BZ    *+14                                                             
         MVC   0(76,R6),0(R4)                                                   
         LA    R6,132(R6)                                                       
         LA    R4,80(R4)                                                        
         BCT   R5,COMPRNT1                                                      
         CLI   COMAREA,0                                                        
         BE    *+8                                                              
         MVI   0(R6),0                                                          
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
GETGL    NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
GOALX    XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
GETREP   NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         MVC   RPKYSAVE,KEY                                                     
         LA    R6,KEY                                                           
         USING REPREC,R6                                                        
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,QMED                                                     
         MVC   REPKREP,WORK                                                     
         MVC   REPKAGY,AGY                                                      
         GOTO1 READREP                                                          
         MVC   KEY,RPKYSAVE                                                     
         L     R6,ADREP                                                         
         L     RE,ADSTATAD                                                      
         USING ADDRREC,RE                                                       
         MVC   ANAME,RNAME                                                      
         MVC   A1LINE,R1LINE                                                    
         MVC   A2LINE,R2LINE                                                    
         MVC   A3LINE(8),R3LINE                                                 
         MVC   ABIGZIP,RBIGZIP                                                  
         XMOD1 1                                                                
         DROP  R6                                                               
         DROP  RE                                                               
         LTORG                                                                  
RPKYSAVE DS    CL32                                                             
         EJECT                                                                  
STATOTC  NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         MVC   MEDNUMWK,NUMWK      SET UP FOR MEDDATE                           
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'4'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'128'                                                 
         MVC   QSTART(12),PASSQST  SET PASS START AND END                       
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   QSTART(12),REASTART RESTORE REQUEST START AND END                
STATOT1  OC    STASPOT,STASPOT                                                  
         BZ    M10B53                                                           
         MVI   P1,0                                                             
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CH    RE,=H'5'                                                         
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XC    P1,P1                                                            
         XC    P2,P2                                                            
         XC    P3,P3                                                            
         LA    R5,MEDPERD                                                       
         GOTO1 DATCON,DMCB,(X'02',(R5)),(X'08',P2+14)                           
         MVI   P2+22,C'-'                                                       
         GOTO1 DATCON,DMCB,(X'02',2(R5)),(X'08',P2+23)                          
*>>      MVC   P2(7),STACAP                                                     
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   P2+8(5),=C'TOTAL'                                                
         MVC   P2+8(L'SP@TOTL),SP@TOTL                                          
*        MVC   P2(7),STAPRINT     STAPRINT NULL - OVERWRITES 'STATION'!         
         MVC   P2(4),STA                                                        
*                                                                               
         CLI   PROGPROF+PROFSNAM-PROFDSCT,C'Y'                                  
         BNE   *+10                                                             
         MVC   P3(L'ANAME),STADDRNM                                             
*                                                                               
*---->   MVC   P4+25(6),=C'TLCSTS'                                              
         MVC   P4+25(L'SP@TLCST),SP@TLCST                                       
         CLI   QMED,C'R'                                                        
         BE    *+8                                                              
         CLI   QMED,C'X'                                                        
         BNE   *+10                                                             
*---->   MVC   P4+25(7),=C'BRDCSTS'                                             
         MVC   P4+25(L'SP@BRDCS),SP@BRDCS                                       
         EDIT  STASPOT,(3,P4+21)                                                
         CLI   DETOPTS+2,0         SUPPRESS COST                                
         BE    M10B                 YES                                         
         LA    R6,P3+20                                                         
         SPACE 2                                                                
M10A     L     RF,STACOST                                                       
         C     RF,=F'99999999'                                                  
         BH    M101A                                                            
         MVI   CURTAB+3,2                                                       
         CURED STACOST,(10,(R6)),CURTAB,CURSYMB=YES,FLOAT=-                     
         B     M10B                                                             
         SPACE 2                                                                
M101A    L     RF,STACOST          DROP PENNIES                                 
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'100'                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         MVI   CURTAB+3,0                                                       
         CURED (RF),(10,(R6)),CURTAB,CURSYMB=YES,FLOAT=-                        
         SPACE 2                                                                
* BUILD GRID                                                                    
M10B     LA    R5,STAGRID                                                       
         LA    R1,14                                                            
         L     R6,PSTAGRID                                                      
         L     R4,MEDAFRST                                                      
M10B1    OC    0(4,R5),0(R5)                                                    
         BZ    M10B2                                                            
         GOTO1 DATCON,DMCB,(X'02',(R4)),(X'08',WORK)                            
         MVC   1(3,R6),WORK                                                     
         MVC   134(2,R6),WORK+3                                                 
         LA    R6,264(R6)                                                       
         L     R8,0(R5)                                                         
         EDIT  (R8),(4,(R6))                                                    
         SH    R6,=H'264'                                                       
         LA    R6,4(R6)                                                         
M10B2    LA    R5,4(R5)                                                         
         LA    R4,12(R4)                                                        
         OC    0(4,R4),0(R4)                                                    
         BZ    M10B2A                                                           
         BCT   R1,M10B1                                                         
         L     R6,PSTAGRID                                                      
         A     R6,=F'528'                                                       
         LA    R1,14                                                            
         B     M10B1                                                            
M10B2A   DS    0H                                                               
         SPACE 2                                                                
* CALCULATE DEMOS AND CPM                                                       
         LA    R3,PRTLINE                                                       
         XC    PRTLINE,PRTLINE                                                  
         MVC   SUMDL(8),STACOST                                                 
         MVC   SUMD1(32),STADEMS                                                
         MVC   FULL,=F'1'                                                       
         GOTO1 VCALCPP,DMCB,FULL                                                
         GOTO1 VEDTDEMS                                                         
         LA    R8,P2                                                            
         CLI   DETOPTS+1,1         DEMOS REQUESTED                              
         B     M10B52                                                           
         ST    R8,FULL             MOVE DEMOS AND CPP TO PRINT                  
         LA    RE,PLD1                                                          
         LA    R1,DNAMES                                                        
         LA    R6,2                                                             
M10B21   MVC   101(5,R8),0(RE)                                                  
         MVC   93(7,R8),0(R1)                                                   
         MVC   120(5,R8),11(RE)                                                 
         MVC   113(7,R8),0(R1)                                                  
         LA    R1,14(R1)                                                        
         LA    RE,22(RE)                                                        
         LA    R8,132(R8)                                                       
         BCT   R6,M10B21                                                        
         L     R8,FULL                                                          
M10B51   CLI   DETOPTS+3,1         CPP/M REQUESTED                              
         BNE   M10B52                                                           
         MVI   BYTE,C' '           SET FOR EQUIVALENCE FLAG                     
         CLC   SUMDL,SUMDLEQ                                                    
         BE    *+8                                                              
         MVI   BYTE,C'+'                                                        
         LA    R6,2                PRINT CPP/M                                  
         LA    RE,PLD1CP+1                                                      
M10B51A  MVC   107(5,R8),0(RE)                                                  
         MVC   112(1,R8),BYTE                                                   
         MVC   127(5,R8),11(RE)                                                 
         LA    R8,132(R8)                                                       
         LA    RE,22(RE)                                                        
         BCT   R6,M10B51A                                                       
M10B52   GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   PASS,X'FF'                                                       
         BE    M10B54                                                           
         CLI   MAXPASS,1           ONLY ONE PASS                                
         BE    M10B54                                                           
         LA    RE,STASPOT           NO - ADD TO OVERALL TOTALS                  
         LA    R1,STTSPOT                                                       
         LA    R0,11                                                            
M10B53A  L     R9,0(R1)                                                         
         A     R9,0(RE)                                                         
         ST    R9,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,M10B53A                                                       
         SPACE 2                                                                
* SET UP FOR NEXT PASS                                                          
M10B53   CLI   MAXPASS,1                                                        
         BE    M10B54                                                           
         CLI   PASS,X'FF'                                                       
         BE    M10B54                                                           
         SR    RE,RE                                                            
         IC    RE,PASS                                                          
         LA    RE,1(RE)                                                         
         STC   RE,PASS                                                          
         CLC   PASS,MAXPASS        TRUE END OF STATION                          
         BL    M10B6                                                            
         MVI   PASS,X'FF'                                                       
         XC    STAGRID(56),STAGRID                                              
         MVC   STASPOT(44),STTSPOT                                              
         XC    STTSPOT(44),STTSPOT                                              
         MVC   MEDPERD,SVRDTE                                                   
         B     STATOT1                                                          
M10B54   MVI   PASS,0              YES - RESET AND EXIT                         
         MVC   WORK(12),PASSTAB                                                 
         MVC   PASSQST(12),PASSTAB                                              
         MVC   MID1,SPACES                                                      
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         XC    STAGRID(56),STAGRID                                              
         XC    STASPOT,STASPOT                                                  
         XC    STADEMS(32),STADEMS                                              
         XC    STACOST(8),STACOST                                               
         B     M10EXIT                                                          
M10B6    MVI   MODE,REREAD                                                      
         SR    R6,R6                                                            
         IC    R6,PASS                                                          
         MH    R6,=H'12'                                                        
         LA    R6,PASSTAB(R6)                                                   
         USING PASSTABD,R6                                                      
         MVC   WORK(12),0(R6)                                                   
         MVC   PASSQST(12),0(R6)   SET PASS START AND END                       
         XC    STAGRID(56),STAGRID                                              
         XC    STASPOT,STASPOT                                                  
         XC    STADEMS(32),STADEMS                                              
         XC    STACOST(8),STACOST                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,MAXLINES                                                      
         SR    RF,RE                                                            
         L     R1,AHDATES                                                       
         CLI   FORCEHED,C'Y'                                                    
         BE    M10B7                                                            
         MVI   FORCEHED,C'Y'                                                    
         C     RF,=F'14'                                                        
         BL    M10EXIT                                                          
         MVI   FORCEHED,C'N'                                                    
         L     R1,PSTAGRID                                                      
M10B7    DS    0C                                                               
*10B7    GOTO1 VHDATES                                                          
         MVI   ALLOWLIN,14                                                      
         MVI   FORCEMID,C'Y'                                                    
M10EXIT  GOTO1 DATCON,DMCB,WORK,(X'03',PASSSD3)                                 
         GOTO1 DATCON,DMCB,WORK+6,(X'03',PASSED3)                               
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         C     RE,=F'8'                                                         
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
EDTDEMSC NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         LA    R4,SVDEMS                                                        
         LA    R5,PLDEMS                                                        
         XC    PLDEMS,PLDEMS                                                    
         LA    R6,4                                                             
EDTDEMS2 OC    0(4,R4),0(R4)                                                    
         BZ    EDTDEM4                                                          
         L     R8,0(R4)            GET DEMO VALUE                               
         C     R8,=F'9999'         DECIMAL PRECISION OK                         
         BH    EDTDEM3              NO - DIVIDE BY 10                           
         MVI   CURTAB+3,1                                                       
         CURED (R8),(5,(R5)),CURTAB YES - MAINTAIN PRECISION                    
         B     EDTDEM4                                                          
EDTDEM3  SRDA  R8,32               DROP DECIMAL PRECISION                       
         SLA   R9,1                                                             
         D     R8,=F'10'                                                        
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         EDIT  (R9),(5,(R5))                                                    
EDTDEM4  LA    R5,5(R5)            GET CPP                                      
         LA    R4,4(R4)                                                         
         L     R8,0(R4)                                                         
         C     R8,=F'99999'                                                     
         BH    EDTDEM4A                                                         
         LTR   R8,R8                                                            
         BZ    EDTDEM5                                                          
         MVI   CURTAB+3,2                                                       
         CURED (R8),(6,(R5)),CURTAB                                             
         B     EDTDEM5                                                          
EDTDEM4A SRDA  R8,32                                                            
         SLA   R9,1                                                             
         D     R8,=F'10'                                                        
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         MVI   CURTAB+3,1                                                       
         CURED (R9),(6,(R5)),CURTAB                                             
EDTDEM5  LA    R4,4(R4)                                                         
         LA    R5,6(R5)                                                         
         BCT   R6,EDTDEMS2                                                      
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
MLASTC   NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         MVC   WEIGHT,SPWEIGHT                                                  
         GOTO1 VSUMMRY                                                          
         L     R8,BUFFBUFF                                                      
         MVC   DMCB+8(20),LVCNTRL                                               
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'90',(R8))                                
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'91',(R8))                                
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'90',(R8)),(X'80',1)                    
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'91',(R8)),(X'80',1)                    
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
EXTRCT   NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         MVC   QSTART(12),PASSQST                                               
         MVC   MEDNUMWK,NUMWK                                                   
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'4'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'128'                                                 
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   MEDBRAND,BPRD                                                    
         MVC   BYTE,MEDSPTLN                                                    
         MVI   MEDSPTLN,0                                                       
         MVI   MEDEXTDM,4                                                       
         SPACE 2                                                                
*                                                                               
         LA    R5,2                SET DEMO LOOKUP CODE                         
         CLI   QRERATE,C' '                                                     
         BE    EXTRCT2                                                          
         CLI   QRERATE,C'A'        ADJUST ONLY                                  
         BNE   EXTRCT1                                                          
         LA    R5,5                                                             
         B     EXTRCT2                                                          
EXTRCT1  LA    R5,3                SET FOR PURCHASED RERATED                    
         CLC   QHUT1,=C'NO'                                                     
         BE    *+8                                                              
         LA    R5,1(R5)            SET FOR ADJUSTMENTS                          
         CLI   QRERATE,C'I'        SET FOR AFFID RERATE                         
         BNE   *+8                                                              
         LA    R5,3(R5)                                                         
*                                                                               
EXTRCT2  L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         MVI   BDPURP,X'FD'        REQUEST NETWORK COSTS                        
         DROP  RE                                                               
         GOTO1 MEDGETBY,DMCB,(RA),(R5)                                          
         BAS   RE,GETHP                                                         
         XC    PRTLINE,PRTLINE                                                  
         LA    R3,PRTLINE                                                       
         L     R5,MEDAFRST         RELEASE DATA TO BUFFALO                      
         CLI   MRPTTYP,C'2'        RPT 2 DOESNT NEED WEEKLYS                    
         BNE   EXTRCT3                                                          
         LA    R5,MEDMON01                                                      
EXTRCT3  MVI   SUMCODE,X'90'                                                    
         MVC   SUMDPGNO(8),MEDDPGNO     SET DAYPART                             
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         MVC   SUMSLN,BDSEC                                                     
         MVC   MEDSPTLN,BDSEC                                                   
         DROP  RE                                                               
         L     R4,4(R5)                                                         
         MVI   SUMRTYP,1                                                        
         LA    RF,MEDPERD                                                       
         CR    R5,RF               END                                          
         BH    EXTRCTX                                                          
         BNE   EXTRCT3A            WEEKLY OR MONTHLY                            
         OC    MEDBYD(12),MEDBYD                                                
         BZ    EXTRCT5              NO - EXIT                                   
         MVI   SUMRTYP,3                                                        
         MVC   SUMDT,=X'FFFFFFFF'                                               
         B     EXTRCT4                                                          
EXTRCT3A OC    0(4,R5),0(R5)       ACTIVE SLOT                                  
         BZ    EXTRCT5                                                          
         LA    RF,MEDMON01                                                      
         CR    R5,RF               MONTHLY                                      
         BL    EXTRCT3B                                                         
         MVI   SUMRTYP,2            YES-SET RECORD CODE                         
         CLI   MRPTTYP,C'2'        MONTHLYS FOR RPT 2 ONLY                      
         BE    EXTRCT3B                                                         
         LA    R5,MEDPERD                                                       
         B     EXTRCT3                                                          
EXTRCT3B MVC   SUMDT,0(R5)                                                      
EXTRCT4  LA    RE,SUMKEY           SET UP DATA ITEM DISPLACEMENTS               
         USING SUMDATA,RE                                                       
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     RE,BUFFLKEY                                                      
         XC    SUMDATA,SUMDATA                                                  
         MVC   SUMSPOTS,MEDBYSPT   MOVE IN DATA                                 
         MVC   SUMDL,MEDBYD                                                     
         MVC   SUMDLEQ,MEDBYDEQ                                                 
         MVC   SUMD1,MEDBY1                                                     
         MVC   SUMD1EQ,MEDBY1EQ                                                 
         MVC   SUMD2,MEDBY2                                                     
         MVC   SUMD2EQ,MEDBY2EQ                                                 
         MVC   SUMD3,MEDBY3                                                     
         MVC   SUMD3EQ,MEDBY3EQ                                                 
         MVC   SUMD4,MEDBY4                                                     
         MVC   SUMD4EQ,MEDBY4EQ                                                 
         LA    R8,PROGPROF                                                      
         USING PROFDSCT,R8                                                      
         CLI   PROFDPT,C'Y'        DAYPART ANALYSIS REQUIRED                    
         BNE   EXTRCT4T             NO - PUT OUT TOTALS ONLY                    
         DROP  R8                                                               
         L     R8,BUFFBUFF                                                      
         BAS   R9,EXTPUT                                                        
* PUT OUT PRODUCT GROUP DETAILS                                                 
         MVI   SUMCODE,X'91'                                                    
         BAS   R9,EXTPUT                                                        
EXTRCT4T L     R8,BUFFBUFF                                                      
         MVC   SUMDPGNO(9),=9X'FF'                                              
         MVI   SUMCODE,X'90'                                                    
         BAS   R9,EXTPUT                                                        
         MVI   SUMCODE,X'91'                                                    
         BAS   R9,EXTPUT                                                        
* GET NEXT BUFFALO ITEM                                                         
EXTRCT5  LA    R5,12(R5)                                                        
         B     EXTRCT3                                                          
EXTPUT   CLI   SUMCODE,X'91'                                                    
         BER   R9                                                               
         OC    SUMSPOTS(12),SUMSPOTS                                            
         BZR   R9                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',(R8),PRTLINE                                
         BR    R9                                                               
         SPACE 2                                                                
EXTRCTX  XMOD1 1                                                                
         DROP  RE                                                               
         EJECT                                                                  
GETHP    NTR1                                                                   
         MVI   MGSW,0                                                           
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         L     R5,MEDAFRST                                                      
         MVC   STRDTE,0(R5)                                                     
         L     R5,MEDALAST                                                      
         MVC   ENDDTE,2(R5)                                                     
         XC    CSPNO,CSPNO                                                      
         OC    PGNOENT,PGNOENT                                                  
         BNZ   GETHP1                                                           
         XC    HPSNO,HPSNO                                                      
         MVC   LASTGSLT,VPGRID                                                  
         XC    PGNOENT,PGNOENT                                                  
GETHP1   L     R5,ADBUY                                                         
         LA    R5,BDELEM-BUYREC(R5)                                             
         USING REGELEM,R5                                                       
REGNXT   ZIC   RE,RLEN                                                          
         AR    R5,RE                                                            
         CLI   0(R5),0                                                          
         BE    GETHPX                                                           
         CLI   RCODE,X'0B'                                                      
         BL    REGNXT                                                           
         CLI   RCODE,X'0D'                                                      
         BH    REGNXT                                                           
         TM    RSTATUS,X'80'                                                    
         BO    REGNXT                                                           
         L     R1,CSPNO                                                         
         STC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         ST    R1,CSPNO                                                         
         TM    RSTATUS,X'40'                                                    
         BO    REGNXT                                                           
         L     RE,ACISLIST                                                      
         XC    CURRCIS,CURRCIS                                                  
CHKCUT   CLI   0(RE),0                                                          
         BE    CHKCUTX                                                          
         CLC   CSPNO+3(1),0(RE)                                                 
         BE    *+12                                                             
         LA    RE,11(RE)                                                        
         B     CHKCUT                                                           
         ST    RE,CURRCIS                                                       
CHKCUTX  DS    0C                                                               
         CLC   RDATE,STRDTE                                                     
         BL    REGNXT                                                           
         CLC   RDATE,ENDDTE                                                     
         BH    REGNXT                                                           
         TM    RSTATUS,X'80'                                                    
         BO    REGNXT                                                           
         ZIC   RE,HPSNO                                                         
         LA    RE,1(RE)                                                         
         ST    RE,HPSNO                                                         
         CLI   RLEN,14             UNALLOCATED                                  
         BL    REGNXT               BYPASS                                      
         L     RF,ADBUY                                                         
         USING BUYREC,RF                                                        
         OC    BDMASPRD,BDMASPRD                                                
         BNZ   GETHP2                                                           
         DROP  RF                                                               
         CLI   MEDBRAND,X'FF'      POL PRODUCT                                  
         BE    GETHP2               OK                                          
         CLI   RLEN,14                                                          
         BE    *+14                                                             
         CLC   MEDBRAND,RPPRD+4    CHECK PIGGY                                  
         BE    *+10                                                             
         CLC   MEDBRAND,RPPRD      WRONG BRAND                                  
         BNE   REGNXT              BYPASS                                       
         SPACE 2                                                                
GETHP2   LA    R8,WORK                                                          
         USING PGDWK,R8                                                         
         USING PGSORT,R8                                                        
         XC    WORK,WORK                                                        
         ZIC   R9,1(R5)                                                         
         AR    R9,R5                                                            
         CLI   0(R9),0                                                          
         BE    GETHPFX                                                          
         CLI   0(R9),X'0F'                                                      
         BL    GETHPFX                                                          
GETHPF   CLI   0(R9),X'12'         FILM ELEMENT                                 
         BNE   GETHPF1                                                          
         USING FLMELEM,R9                                                       
         MVC   PGDFDAY,FLMDAY       YES - SAVE FILM                             
         MVC   PGDFNO,FLMNUM                                                    
         B     GETHPFX                                                          
GETHPF1  ZIC   R0,1(R9)                                                         
         AR    R9,R0                                                            
         CLI   0(R9),X'0F'                                                      
         BH    GETHPF                                                           
GETHPFX  DS    0H                                                               
         MVC   PGDWK,RDATE                                                      
         ST    R5,FULL                                                          
         MVC   PGDELAD,FULL        SAVE ELEMENT ADDRESS                         
         CLC   RTYPE,=C'RS '                                                    
         BNE   GETHP2A0                                                         
         TM    RSTATUS,X'40'                                                    
         BO    REGNXT                                                           
GETHP2A0 DS    0H                                                               
         CLI   SORTREQ,0                                                        
         BE    *+10                                                             
         XC    PGDELAD,PGDELAD                                                  
         MVC   PGDSBRN,RPPRD                                                    
         MVC   PGDCUTIN,CURRCIS                                                 
         CLI   RLEN,13                                                          
         BH    GETHP2A                                                          
         CLC   RTYPE,=C'RS '                                                    
         BE    REGNXT                                                           
         MVI   PGDSBRN,X'FF'                                                    
         MVI   PGDSSLN,0                                                        
         MVC   PGDSNO,HPSNO                                                     
         B     GETHP2C                                                          
         SPACE 2                                                                
GETHP2A  TM    RSTATUS,X'04'                                                    
         BZ    GETHP2B                                                          
         MVI   PGDSBRN,X'FF'                                                    
         MVI   PGDSSLN,0                                                        
         MVC   PGDSNO,HPSNO                                                     
         MVI   PGDIND,X'08'        SET HIATUS INDICATOR                         
         B     GETHP2C                                                          
         SPACE 2                                                                
GETHP2B  TM    RSTATUS,X'40'                                                    
         BZ    *+8                                                              
         MVI   PGDIND,X'01'        SET MISSED INDICATOR                         
         TM    RSTATUS,X'42'                                                    
         BNO   *+8                                                              
         MVI   PGDIND,X'04'        SET PREMPT INDICATOR                         
         CLC   RTYPE,=C'RS '                                                    
         BNE   *+12                                                             
         CLI   PGDIND,0            RS BYPASSES MINUS AND HIATUS SPOTS           
         BNE   REGNXT                                                           
         MVC   PGDSBRN,RPPRD                                                    
         MVC   PGDSSLN,RPTIME                                                   
         MVC   PGDSNO,HPSNO                                                     
         SPACE 2                                                                
GETHP2C  L     RE,LASTGSLT         SET TO LAST GRID SLOT                        
         L     RF,PGNOENT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PGNOENT                                                       
         STC   RF,PGLSLOT                                                       
         MVC   0(PGDLEN,RE),WORK                                                
         LA    RE,PGDLEN(RE)                                                    
         ST    RE,LASTGSLT                                                      
*                                                                               
         CLI   QPNAME,C'Y'         TEST PRODUCT NAME LEGEND REQUIRED            
         BNE   REGNXT                                                           
         STM   RE,R1,WORK                                                       
         CLI   1(R5),14            TEST ALLOCATED                               
         BL    GETHP3X                                                          
         LA    R1,10(R5)           YES-ADD PRODUCT(S) TO PRODUCT LIST           
         BRAS  RE,ADDPRD                                                        
*                                                                               
         CLI   1(R5),18                                                         
         BL    GETHP3X                                                          
         LA    R1,14(R5)                                                        
         BRAS  RE,ADDPRD                                                        
*                                                                               
GETHP3X  LM    RE,R1,WORK                                                       
         B     REGNXT                                                           
*                                                                               
GETHPX   CLI   VARFRMT,1           VARIABLE FORMAT                              
         BE    GETHPX2              YES - ALLOW VARIABLE SLOT                   
         L     R5,MEDAFRST         NO - FORCE TO DATE SLOT                      
         L     R1,PGNOENT                                                       
         LTR   R1,R1                                                            
         BZ    GETHPX2                                                          
         L     R8,VPGRID                                                        
         LA    R2,1                                                             
GETHPS   CLC   PGDWK,0(R5)                                                      
         BNL   *+12                                                             
         LA    R2,1                                                             
         L     R5,MEDAFRST                                                      
         CLC   PGDWK,2(R5)         SET FIXED SLOT FOR WEEK                      
         BNH   GETHPS2                                                          
         LA    R5,12(R5)                                                        
         LA    R2,1(R2)                                                         
         B     GETHPS                                                           
GETHPS2  STC   R2,PGLSLOT                                                       
         LA    R8,PGDLEN(R8)                                                    
         BCT   R1,GETHPS                                                        
GETHPX2  XIT1                                                                   
         DROP  R6                                                               
         DROP  R5                                                               
         DROP  R3                                                               
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
GETBUF   NTR1  BASE=*,LABEL=*                                                   
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SUMDSECT,R3                                                      
         CLI   BUFHI,1                                                          
         BNE   GETBUF1                                                          
         XC    MYBUFIO(30),MYBUFIO                                              
         MVC   MYBUFIO(1),BUFCDE                                                
         MVI   BUFHI,0                                                          
         L     R8,BUFFBUFF                                                      
         SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',(BUFCDE,(R8)),MYBUFIO,(R9)                 
         B     GETBUF2                                                          
GETBUF1  L     R8,BUFFBUFF                                                      
         SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFCDE,(R8)),MYBUFIO,(R9)                  
GETBUF2  CLC   MYBUFIO(1),BUFCDE                                                
         BNE   *+12                                                             
         TM    DMCB+8,X'80'                                                     
         BZ    GETBUF3                                                          
         XC    MYBUFIO(30),MYBUFIO                                              
         B     GETBUFX                                                          
GETBUF3  MVC   PRTLINE,MYBUFIO                                                  
         LA    R3,MYBUFIO                                                       
         XC    MYBUFIO(8),MYBUFIO                                               
         MVI   SUMRPT,1                                                         
         MVC   SUMKEY,PRTLINE                                                   
         LA    R8,PRTLINE                                                       
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     R8,BUFFLKEY                                                      
         DROP  RF                                                               
         MVC   SUMDATA,0(R8)                                                    
         CLI   SPOTPROF+1,C'N'                                                  
         BE    GETBUFX                                                          
         LA    R4,DNAMES           SET UP FOR UNWEIGHTING                       
         LA    R5,4                                                             
         LA    R6,SUMD1                                                         
GBUF4    CLI   SPOTPROF+1,C'D'     UNWEIGHT TOTALS                              
         BE    *+12                                                             
         CLI   0(R4),C'R'                                                       
         BNE   GBUF5                                                            
         OC    SPWEIGHT,SPWEIGHT                                                
         BNZ   GBUF4A                                                           
         XC    0(8,R6),0(R6)                                                    
         B     GBUF5                                                            
GBUF4A   DS    0H                                                               
         L     RE,0(R6)                                                         
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R6)                                                         
         L     RE,4(R6)                                                         
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,4(R6)                                                         
GBUF5    LA    R4,7(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,GBUF4                                                         
         CLI   SPOTPROF+1,C'D'                                                  
         BE    *+12                                                             
         CLI   DNAMES,C'R'                                                      
         BNE   GETBUFX                                                          
         OC    SPWEIGHT,SPWEIGHT                                                
         BNZ   GBUF5A                                                           
         XC    SUMGD1(8),SUMGD1                                                 
         B     GETBUFX                                                          
GBUF5A   DS    0H                                                               
         L     RE,SUMGD1                                                        
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,SUMGD1                                                        
         L     RE,SUMGD1E                                                       
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,SUMGD1E                                                       
GETBUFX  XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
PRSHEAD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         MVC   H12(L'SP@DAY),SP@DAY                                             
         MVC   H13(L'SPUDAY),SPUDAY                                             
         MVC   H12+9(L'SP@TIME),SP@TIME                                         
         MVC   H13+9(L'SPUTIME),SPUTIME                                         
         MVC   H12+21(L'SP@PROG),SP@PROG                                        
         MVC   H13+21(L'SPUPROG),SPUPROG                                        
         MVC   H12+37(L'SP@LEN),SP@LEN                                          
         MVC   H13+37(L'SPULEN),SPULEN                                          
         MVI   H12+41,C'-'                                                      
         SPACE 2                                                                
* CENTER GRID HEADLINES                                                         
         ZIC   RF,LENGRID          MOVE IN DASHES                               
         MH    RF,NOINGRID                                                      
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   H12+42(0),H12+41                                                 
         LA    RE,H12+42           CENTER CAPTION                               
         AHI   RE,-7                                                            
         SRL   RF,1                                                             
         AR    RE,RF                                                            
*---->   MVC   0(17,RE),=C'ROTATION SCHEDULE'                                   
         MVC   0(L'SP@ROTSC,RE),SP@ROTSC                                        
*                                                                               
         CLI   PROGPROF+PROFSNAM-PROFDSCT,C'Y'                                  
         BE    PRSH010                                                          
*                                                                               
PRSH005  MVC   H1+20(4),STA                                                     
         MVC   H1+25(L'SP@NTRSC),SP@NTRSC                                       
         OC    H1+25(L'SP@NTRSC),SPACES                                         
         B     PRSH020                                                          
*                                                                               
PRSH010  CLC   STADDRNM,SPACES                                                  
         BE    PRSH005             NO NAME - STD TITLE LOOKS NICER              
         MVC   H1+20(4),STA                                                     
         LA    RE,H1+23                                                         
         CLI   STA+3,C' '                                                       
         BNH   *+8                                                              
         AHI   RE,1                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(20,RE),STADDRNM                                                
         OC    1(20,RE),SPACES                                                  
         MVC   H1+47(L'SP@NTRSC),SP@NTRSC                                       
         OC    H1+47(L'SP@NTRSC),SPACES                                         
PRSH020  EQU   *                                                                
         GOTO1 SQUASHER,DMCB,H1+20,56                                           
         GOTO1 CENTER,DMCB,H1+20,56                                             
         GOTO1 UNDERLIN,DMCB,(56,H1+20),H2+20                                   
*                                                                               
         MVC   P1(40),SVP1                                                      
         DROP  R5                                                               
*                                                                               
         CLI   MODE,STALAST        BYPASS CONTRACT GT. STALAST                  
         BH    PTHEXIT                                                          
         CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
         BE    PTHID4                                                           
PTHID1   CLI   QBYID,C'Y'                                                       
         BNE   *+16                                                             
         MVC   H4(12),BUYIDNAM                                                  
         MVC   H4+13(12),BUYID                                                  
         B     PTHEXIT                                                          
         SPACE 2                                                                
PTHID4   MVC   MID1(12),BUYIDNAM                                                
         MVC   MID1+13(12),BUYID                                                
         MVI   FORCEMID,C'Y'                                                    
PTHEXIT  EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CALCPP   NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING MEDBLOCK,R7                                                      
         USING SUMDSECT,R3                                                      
         L     RE,SUMDL                                                         
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         BNP   CALCPPX                                                          
         CLI   SPOTPROF+1,C'N'                                                  
         BE    UNWTX                                                            
         CLI   SPOTPROF+1,0                                                     
         BE    UNWTX                                                            
         OC    WEIGHT,WEIGHT                                                    
         BZ    UNWTX                                                            
         LA    R4,DNAMES                                                        
         LA    R5,4                                                             
         LA    R6,SUMD1                                                         
UNWT1    CLI   SPOTPROF+1,C'D'     UNWEIGHT DEMOS                               
         BE    *+12                                                             
         CLI   0(R4),C'R'                                                       
         BNE   UNWT2                                                            
         L     RE,0(R6)                                                         
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,WEIGHT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R6)                                                         
         L     RE,4(R6)                                                         
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,WEIGHT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,4(R6)                                                         
UNWT2    LA    R4,7(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,UNWT1                                                         
UNWTX    DS    0H                                                               
         XC    SVD1(32),SVD1                                                    
         LA    R0,4                                                             
         LA    R5,SVD1                                                          
         LA    R6,SUMD1                                                         
CALCPP1  L     RE,SUMDL            GET DOLLARS                                  
         CLI   CPPSW,C'D'                                                       
         BNE   *+8                                                              
         L     RE,SUMDLEQ          USE EQUIVALENCED DOLLARS                     
         L     R8,0(R6)            GET DEMOS                                    
         CLI   CPPSW,C'D'                                                       
         BE    *+8                                                              
         L     R8,4(R6)            USE EQUIVALENCED DEMOS                       
         CLI   MODE,PROCBUY                                                     
         BNE   *+12                                                             
         L     RE,SUMDL                                                         
         L     R8,0(R6)                                                         
         LTR   R8,R8                                                            
         BZ    CALCPP2                                                          
         SRDA  RE,32                                                            
         LTR   RF,RF                                                            
         BZ    CALCPP2                                                          
         SLA   RF,1                                                             
         MH    RF,=H'10'           SCALE FOR DEMO DECIMAL                       
         DR    RE,R8                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,4(R5)            SAVE CPP                                     
         SPACE 2                                                                
CALCPP2  L     R8,0(R6)            CALCULATE DEMO AVERAGES                      
         SRDA  R8,32                                                            
         SLA   R9,1                                                             
         L     RF,0(R1)                                                         
         D     R8,0(RF)                                                         
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         ST    R9,0(R5)            SAVE DEMO                                    
         SPACE 2                                                                
         LA    R5,8(R5)            GET NEXT                                     
         LA    R6,8(R6)                                                         
         BCT   R0,CALCPP1                                                       
         SPACE 2                                                                
         OC    SUMGDL(16),SUMGDL                                                
         BZ    CALCPPX                                                          
         L     RE,SUMGDL                                                        
         CLI   CPPSW,C'D'                                                       
         BNE   *+8                                                              
         L     RE,SUMGDLE                                                       
         L     R8,SUMGD1                                                        
         CLI   CPPSW,C'D'                                                       
         BE    *+8                                                              
         L     RE,SUMGD1E                                                       
         LTR   R8,R8                                                            
         BZ    CALCPPX                                                          
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         DR    RE,R8                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,4(R5)                                                         
CALCPPX  XMOD1 1                                                                
         LTORG                                                                  
               EJECT                                                            
* PRINT POOL BUYSHEET GRID                                                      
*        0 - LENGTH OF GRID BLOCK                                               
*       1-3- A(SPWORK)                                                          
         SPACE 2                                                                
PTSGRID  NMOD1 10,PTSGRID                                                       
         L     RA,0(R1)                                                         
         LA    RA,0(RA)                                                         
         LR    R3,RC                                                            
         USING PTSGD,R3                                                         
         MVC   GRIDLEN,0(R1)                                                    
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         L     RE,VPLAREA                                                       
         L     RF,=AL4(PLAREALQ)                                                
         XCEF                                                                   
         L     R6,VPGRID                                                        
         USING PGRIDD,R6                                                        
         EJECT                                                                  
*        SORT SPOTS INTO DAY/BRAND/SPOT LENGTH/TYPE ORDER                       
         MVC   SSTAGRD,DSTAGRID                                                 
         L     R6,VPGRID           SET UP SORT KEYS                             
         LR    RE,R6                                                            
         USING PGSRT1D,RE                                                       
         LA    R7,1                                                             
         L     R1,PGNOENT                                                       
         LTR   R1,R1                                                            
         BZ    PTSGRIDX                                                         
PGCNDSRT XC    PGSORT,PGSORT                                                    
         MVC   PGDS1WK,PGDWK                                                    
         MVI   PGDS1SLT,0                                                       
         CLI   PGCNDSW,1           CONDENSE REQUIRED                            
         BE    *+8                  YES - DONT SET SPOT NUMDBER                 
         STC   R7,PGDS1SLT          NO - SET SPOT NUMBER                        
         MVC   PGDSNO,PGDS1SLT                                                  
         MVC   PGDS1BR,PGDSBRN                                                  
         MVC   PGDS1SL,PGDSSLN                                                  
         MVC   PGDS1IND,PGDIND                                                  
         CLI   PGDIND,0            OTO                                          
         BNE   *+8                                                              
         LA    R7,1(R7)             NO - BUMP SPOT NUMBER                       
         LA    RE,PGDLEN(RE)                                                    
         LR    R6,RE                                                            
         BCT   R1,PGCNDSRT                                                      
         DROP  RE                                                               
         L     R8,PGNOENT                                                       
         L     R6,VPGRID                                                        
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
         SPACE 2                                                                
*        CONDENSE LIKE SPOTS                                                    
         CLI   PGCNDSW,1           CONDENSE - REQUIRED                          
         BNE   PGCNDX               NO - CHECK FORMAT TYPE                      
*              TABLE SORTED NOW CONDENSE                                        
         L     R6,VPGRID                                                        
PGCND1   OC    0(PGDLEN,R6),0(R6)  END                                          
         BZ    PGCND2                                                           
         MVI   PGDSNO,1            SET SPOT NUMBER TO 1                         
         LA    R6,PGDLEN(R6)                                                    
         B     PGCND1                                                           
         SPACE 2                                                                
PGCND2   L     R6,VPGRID                                                        
         LR    RE,R6                                                            
         LR    R4,R6                                                            
PGCND3   LA    R4,PGDLEN(R4)       SET POINTER TO NEXT SLOT                     
         OC    0(PGDLN1,R4),0(R4)                                               
         BZ    PGCND4                                                           
         CLC   0(L'PGSORT,RE),0(R4)     THIS SLOT TO NEXT                       
         BNE   PGCND3A                                                          
         LR    R6,RE                    POINT TO ADD SLOT                       
         ZIC   RF,PGDSNO           BUMP NUMBER OF SPOTS BY 1                    
         LA    RF,1(RF)                                                         
         STC   RF,PGDSNO                                                        
         LR    R6,R4                                                            
         MVI   PGSORT,X'FF'             ELIMINATE NEXT ELEMENT                  
         B     PGCND3                                                           
PGCND3A  LR    RE,R4                                                            
         B     PGCND3                                                           
PGCND4   L     R6,VPGRID                                                        
         L     R8,PGNOENT                                                       
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
         L     R6,VPGRID                DELETE DUPS                             
         L     R1,PGNOENT                                                       
PGCND5   CLI   0(R6),0                                                          
         BE    PGCNDX                                                           
         CLI   0(R6),X'FF'                                                      
         BNE   PGCND6                                                           
         XC    0(PGDLEN,R6),0(R6)                                               
         BCTR  R1,0                                                             
         ST    R1,PGNOENT                                                       
PGCND6   LA    R6,PGDLEN(R6)                                                    
         B     PGCND5                                                           
         SPACE 2                                                                
PGCNDX   DS    0H                                                               
         EJECT                                                                  
*              CHECK FOR VARIABLE FORMAT                                        
         CLI   VARFRMT,1                                                        
         BNE   VAR2                                                             
         LA    R5,1                INITIALIZE SLOT NUMBER                       
         L     R6,VPGRID                                                        
         SPACE 2                                                                
VARF1    OC    PGSORT,PGSORT                                                    
         BZ    VARFX                                                            
         STC   R5,PGLSLOT          SET SLOT NUMBER                              
         CLC   PGDLEN(10,R6),PGSORT                                             
         BNE   VARF2                                                            
         LA    R6,PGDLEN(R6)                                                    
         B     VARF1                                                            
VARF2    LA    R6,PGDLEN(R6)                                                    
         LA    R5,1(R5)                                                         
         B     VARF1                                                            
         SPACE 2                                                                
VAR2     CLI   VARFRMT,2           VARIABLE FORMAT WITH FIXED SLOTS             
         BNE   VARFX                                                            
         LA    R5,1                INITIALIZE SLOT NUMBER                       
         L     R6,VPGRID                                                        
         SPACE 2                                                                
VAR2F1   OC    PGSORT,PGSORT                                                    
         BZ    VARFX                                                            
         STC   R5,PGLSLOT          SET SLOT NUMBER                              
         CLC   PGDLEN(2,R6),PGSORT SLOT BY DATE ONLY                            
         BNE   VAR2F2                                                           
         LA    R6,PGDLEN(R6)                                                    
         B     VAR2F1                                                           
VAR2F2   LA    R6,PGDLEN(R6)                                                    
         LA    R5,1(R5)                                                         
         B     VAR2F1                                                           
         SPACE 2                                                                
VARFX    DS    0H                                                               
         EJECT                                                                  
*              SORT INTO SLOT NUMBER ORDER                                      
         L     R6,VPGRID                                                        
         LR    RE,R6                                                            
         USING PGSRT2D,RE                                                       
         L     R1,PGNOENT                                                       
PGSLTSRT XC    PGSORT,PGSORT                                                    
         LR    RE,R6                                                            
         MVC   PGDS2SLT,PGLSLOT                                                 
         MVC   PGDS2WK,PGDWK                                                    
         MVI   PGDS2SNO,0                                                       
         CLI   PGCNDSW,1                                                        
         BE    *+10                                                             
         MVC   PGDS2SNO,PGDSNO                                                  
         MVC   PGDS2BR,PGDSBRN                                                  
         MVC   PGDS2SL,PGDSSLN                                                  
         MVC   PGDS2IND,PGDIND                                                  
         LA    R6,PGDLEN(R6)                                                    
         BCT   R1,PGSLTSRT                                                      
         DROP  RE                                                               
         L     R8,PGNOENT                                                       
         L     R6,VPGRID                                                        
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
         EJECT                                                                  
*              TABLE SORTED INTO SLOT NUMBER ORDER                              
*               NOW ASSIGN LINE NUMBERS                                         
PGSLNO   L     R6,VPGRID                                                        
         LA    R5,1                                                             
         LH    RF,NOINGRID                                                      
         ST    RF,PGWMAX                                                        
PGSLNO1  MVC   WORK(L'PGSORT),PGSORT                                            
         STC   R5,PGSUBLI          SET SUB-LINE                                 
         ZIC   RF,PGLSLOT                                                       
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         D     RE,PGWMAX           GET LINE NUMBER                              
         STC   RF,PGLINNO                                                       
         STC   RE,PGLSLOT                                                       
         LA    R6,PGDLEN(R6)                                                    
         CLC   PGSORT(1),WORK                                                   
         BNE   PGSLNO2                                                          
         LA    R5,1(R5)                                                         
         B     PGSLNO1                                                          
         SPACE 2                                                                
PGSLNO2  LA    R5,1                                                             
         OC    PGSORT,PGSORT                                                    
         BNZ   PGSLNO1                                                          
         SPACE 2                                                                
*              SORT INTO LINE/SUBLINE/SLOT NUMBER ORDER                         
         L     R6,VPGRID                                                        
         L     R1,PGNOENT                                                       
PGSRTPR  XC    PGSORT,PGSORT                                                    
         MVC   PGSORT(3),PGLINNO                                                
         LA    R6,PGDLEN(R6)                                                    
         BCT   R1,PGSRTPR                                                       
         SPACE 2                                                                
         L     R8,PGNOENT                                                       
         L     R6,VPGRID                                                        
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
         EJECT                                                                  
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         MVC   PGPRLNO,PGLINNO                                                  
         XC    PRPGWMAX,PRPGWMAX                                                
         MVC   GRIDSLN,BDSEC                                                    
         MVC   SSTAGRD,DSTAGRID    SET UP FOR 100 PRINT LINES                   
         XC    PGWNOL,PGWNOL                                                    
         L     RE,VPLAREA                                                       
         ST    RE,DSTAGRID                                                      
         L     RF,=AL4(PLAREALQ)                                                
         XCEF                                                                   
PTSGRDA  MVC   GRIDST,DSTAGRID                                                  
         XC    PGWKCNT,PGWKCNT                                                  
         XC    PGWMAX,PGWMAX                                                    
         OC    OPTRPT,OPTRPT       OPTIONAL REPORT ACTIVE                       
         BZ    PTSGRID1                                                         
         MVI   OPTRMODE,PROCBUY                                                 
         GOTO1 OPTRPT,DMCB,(RA)                                                 
         SPACE 2                                                                
PTSGRID1 L     R4,GRIDST           SET BEGINNING OF BLOCK                       
         ZIC   RF,PGLSLOT                                                       
         ST    RF,PGWKCNT                                                       
         ZIC   RF,GRIDLEN                                                       
         MH    RF,PGWKCNT+2         SET BEGINNING OF THIS SLOT                  
         AR    R4,RF                                                            
PTSGRID2 OC    PGDWK,PGDWK         END                                          
         BZ    PTSGRIDX                                                         
         CLC   PGLINNO(2),PGPRLNO  SAME LINE/SUBLINE                            
         BE    PTSGRID3                                                         
         CLI   FORCEHED,C'Y'                                                    
*        BE    PTSGRD2                                                          
         MVC   PGPRLNO,PGLINNO                                                  
         L     RE,PGWMAX            YES - SET NEXT GRID                         
         XC    PGWMAX,PGWMAX                                                    
         A     RE,PRPGWMAX                                                      
         ST    RE,PRPGWMAX                                                      
*--->    C     RE,=F'95'                                                        
         C     RE,=AL4(PLARMXQ)                                                 
         BNH   PTSGRD2A                                                         
PTSGRD2  L     R9,VPLAREA          PRINT FROM PL BUFFER                         
         LA    R1,0                                                             
         LA    R0,PLARMXQ                                                       
PTPCTLN  OC    0(70,R9),0(R9)                                                   
         BZ    PTPCTLN1                                                         
         LA    R1,1(R1)                                                         
         LA    R9,70(R9)                                                        
         BCT   R0,PTPCTLN                                                       
*                                                                               
         SH    R9,=H'70'           BACK UP TO LAST ENTRY                        
         XC    0(70,R9),0(R9)      AND CLEAR IT FOR NOW                         
PTPCTLN1 STC   R1,BYTE                                                          
         ZIC   RE,LINE                                                          
         ZIC   RF,BYTE                                                          
         AR    RF,RE                                                            
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         L     R9,VPLAREA                                                       
PTPGEX2  L     R8,SSTAGRD                                                       
         LA    R0,14                                                            
PTPGEX3  OC    0(70,R9),0(R9)                                                   
         BZ    PTPGEX4                                                          
         MVC   0(70,R8),0(R9)                                                   
         LA    R8,132(R8)                                                       
         LA    R9,70(R9)                                                        
         BCT   R0,PTPGEX3                                                       
*                                                                               
PTPGEX4  GOTO1 REPORT                                                           
         OC    0(70,R9),0(R9)                                                   
         BNZ   PTPGEX2                                                          
         MVC   DSTAGRID,SSTAGRD                                                 
         MVI   P1,0                                                             
         GOTO1 REPORT                                                           
         L     RE,VPLAREA                                                       
         ST    RE,DSTAGRID                                                      
         L     RF,=AL4(PLAREALQ)                                                
         XCEF                                                                   
         B     PTSGRID1                                                         
         SPACE 2                                                                
PTSGRD2A MH    RE,=H'70'                                                        
         A     RE,DSTAGRID                                                      
         ST    RE,GRIDST                                                        
         B     PTSGRID1                                                         
         EJECT                                                                  
* SET ALLOCATIONS IN PRINT LINE                                                 
PTSGRID3 L     R5,PGDELAD                                                       
         USING REGELEM,R5                                                       
         OC    PGDCUTIN,PGDCUTIN                                                
         B     *+6                                                              
         DC    H'0'                                                             
         OC    PGDELAD,PGDELAD     HAVE ELEMENT ADDRESS                         
         BNZ   PTSG3EOK             YES - PROCESS                               
         LA    R5,PGELEM            NO - BUILD DUMMY ELEMENT                    
         XC    PGELEM,PGELEM                                                    
         MVC   RDATE,PGDWK                                                      
         MVC   RPPRD,PGDSBRN                                                    
         MVC   PGELEM(2),=X'0B0E'                                               
PTSG3EOK DS    0H                                                               
         CLI   PGCNDSW,1           CONDENSE                                     
         BE    *+8                  YES                                         
         MVI   PGDSNO,1            NO - PGDSNO HAS SUBLINE-RESET                
         XC    DUB,DUB                                                          
         CLI   GRIDLEN,13                                                       
         BNE   *+14                                                             
         OC    PGDFNO,PGDFNO       FORCE SECOND DATE IF FILM                    
         BNZ   PTSG3DT                                                          
         CLI   SCNDDTSW,1          PRINT DATES ON SECOND LINE                   
         BE    *+12                 YES                                         
         CLI   PGSUBLI,1           FIRST SUBLINE - PRINT DATE                   
         BNE   PTSGRD3A            NO                                           
PTSG3DT  GOTO1 DATCON,DMCB,(2,RDATE),(4,DUB)                                    
PTSGRD3A CLI   GRIDLEN,6                                                        
         BE    P06GRID                                                          
         MVC   FILMNO,SPACES                                                    
         CLI   GRIDLEN,10                                                       
         BE    P10GRID                                                          
         CLI   GRIDLEN,13                                                       
         BE    P13GRID                                                          
         BAS   R9,LINEA                                                         
         CLI   DUB,0                                                            
         BE    PTSG3A1                                                          
         MVC   0(3,RE),DUB                                                      
         MVC   70(2,RE),DUB+3                                                   
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
         AH    RE,=H'70'                                                        
PTSG3A1  TM    RSTATUS,X'46'                                                    
         BZ    PTSGRD3B                                                         
         TM    RSTATUS,X'40'            PRE-EMPT                                
         BZ    *+10                                                             
         MVC   0(3,RE),=C'*P*'                                                  
         TM    RSTATUS,X'02'            MAKEGOOD                                
         BZ    *+10                                                             
         MVC   0(3,RE),=C'*M*'                                                  
         TM    RSTATUS,X'04'            HIATUS                                  
         BZ    *+10                                                             
         MVC   0(3,RE),=C'*H*'                                                  
         BAS   R9,INCR                                                          
PTSGRD3B DS    0C                                                               
         CLI   DUB,0                                                            
         BE    PTSGRID4                                                         
         BAS   R9,INCR                                                          
         SPACE 2                                                                
PTSGRID4 BAS   R9,LINEA                                                         
         CLI   RLEN,X'0E'                                                       
         BL    PTSGRID5                                                         
         ZIC   RF,RPPRD                 GET PRODUCT CODE                        
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         BAS   R9,INCR                                                          
         ZIC   RE,RPTIME                                                        
         LTR   RE,RE                                                            
         BZ    PTSGRID5                                                         
         TM    RSTATUS,X'04'                                                    
         BO    PTSGRID5                                                         
         CLC   GRIDSLN,RPTIME                                                   
         BE    PTSGRID5                                                         
         EDIT  (B1,RPTIME),(3,DUB)                                              
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),DUB                                                      
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
         ZIC   RF,RPPRD+4                                                       
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         BAS   R9,INCR                                                          
         ZIC   RE,RPTIME+4                                                      
         BAS   R9,PBEDIT                                                        
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),DUB                                                      
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
         MVI   0(RE),C' '                                                       
         BAS   R9,INCR                                                          
         SPACE 2                                                                
PTSGRID5 CLI   PGDSNO,1                                                         
         BE    PTSGRID7                                                         
         BAS   R9,LINEA                                                         
         MVC   0(4,RE),=C'(  )'                                                 
         LA    R9,1(RE)                                                         
         EDIT  PGDSNO,(2,(R9))                                                  
         BAS   R9,INCR                                                          
PTSGRID7 BAS   R9,LINEA                                                         
         OC    PGDCUTIN,PGDCUTIN                                                
         BZ    PTSGRID8                                                         
         MVC   DMCB(4),PGDCUTIN                                                 
         L     R8,DMCB                                                          
         MVC   BYTE,0(R8)                                                       
PTSG7A   GOTO1 MSUNPK,DMCB,(X'80',3(R8)),WORK+8,WORK                            
         BAS   R9,LINEA                                                         
         MVC   0(4,RE),WORK                                                     
         CLI   WORK+4,C'/'                                                      
         BNE   *+14                                                             
         MVC   0(3,RE),WORK+4      SHOW SUFFIX FOR CABLE                        
         MVI   3(RE),C' '                                                       
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
         ZIC   RF,1(R8)                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)       MOVE IN PRODUCT CODE                         
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
         ZIC   RF,2(R8)                                                         
         LTR   RF,RF                                                            
         BZ    PTSG7A0                                                          
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVI   0(RE),C'/'                                                       
         MVC   1(3,RE),1(RF)       MOVE IN PRODUCT CODE                         
         BAS   R9,INCR                                                          
PTSG7A0  OC    8(3,R8),8(R8)       CHECK FOR NETWORK COST OVERRIDES             
         BZ    PTSG7B1                                                          
         MVI   CURTAB+3,2                                                       
         CURED (B3,8(R8)),(7,DUB),CURTAB                                        
         BAS   R9,LINEA                                                         
         LA    R1,1                                                             
PTSG7A1  LR    RF,RE                                                            
         SH    RF,=H'3'                                                         
         OC    0(3,RF),0(RF)                                                    
         BZ    PTSG7A2                                                          
         LA    RE,70(RE)                                                        
         LA    R1,1(R1)                                                         
         B     PTSG7A1                                                          
PTSG7A2  SH    RE,=H'2'                                                         
         MVC   0(7,RE),DUB                                                      
         SR    RE,RE                                                            
         ICM   RE,3,PGWNOL                                                      
         AR    RE,R1                                                            
         STCM  RE,3,PGWNOL                                                      
PTSG7B1  CLC   PGWNOL,=AL2(PLARMXQ)                                             
         BL    PTSG7C                                                           
         GOTO1 REPORT                                                           
         MVC   PGWNOL,=H'1'                                                     
         XC    PGWMAX,PGWMAX                                                    
         XC    PRPGWMAX,PRPGWMAX                                                
PTSG7C   DS    0H                                                               
         CLC   11(1,R8),0(R8)                                                   
         BNE   *+12                                                             
         LA    R8,11(R8)                                                        
         B     PTSG7A                                                           
         BAS   R9,LINEA                                                         
         MVI   0(RE),C' '                                                       
         SR    RE,RE                                                            
         ICM   RE,3,PGWNOL         PUT A SPACE BETWEEN SPOTS                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,PGWNOL                                                      
PTSGRID8 DS    0C                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PGWNOL         SET NUMBER OF LINES IN THIS BLOCK            
         XC    PGWNOL,PGWNOL                                                    
         C     RE,PGWMAX           MAX THIS BLOCK GT. THAN PREV BLOCK           
         BNH   *+8                                                              
         ST    RE,PGWMAX                                                        
         LA    R6,PGDLEN(R6)                                                    
         B     PTSGRID1                                                         
         SPACE 2                                                                
PTSGRIDX CLC   RTYPE,=C'RS '                                                    
         BNE   PTSGEX1                                                          
         L     R9,VPLAREA          PRINT FROM PL BUFFER                         
         LA    R1,0                                                             
         LA    R0,PLARMXQ                                                       
PTSCTLN  OC    0(70,R9),0(R9)                                                   
         BZ    PTSCTLN1                                                         
         LA    R1,1(R1)                                                         
         LA    R9,70(R9)                                                        
         BCT   R0,PTSCTLN                                                       
         SH    R9,=H'70'           BACK UP TO LAST ENTRY                        
         XC    0(70,R9),0(R9)      AND CLEAR IT FOR NOW                         
PTSCTLN1 STC   R1,BYTE                                                          
         ZIC   RE,LINE                                                          
         ZIC   RF,BYTE                                                          
         AR    RF,RE                                                            
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         L     R9,VPLAREA                                                       
PTSGEX2  L     R8,SSTAGRD                                                       
         LA    R0,14                                                            
PTSGEX3  OC    0(70,R9),0(R9)                                                   
         BZ    PTSGEX4                                                          
         MVC   0(70,R8),0(R9)                                                   
         LA    R8,132(R8)                                                       
         LA    R9,70(R9)                                                        
         BCT   R0,PTSGEX3                                                       
PTSGEX4  GOTO1 REPORT                                                           
         OC    0(70,R9),0(R9)                                                   
         BNZ   PTSGEX2                                                          
         MVC   DSTAGRID,SSTAGRD                                                 
         MVI   P1,0                                                             
         GOTO1 REPORT                                                           
         B     PTSEXIT                                                          
PTSGEX1  LA    R8,P4+4                                                          
         CLI   PKGAREA,0                                                        
         BE    *+8                                                              
         LA    R8,P5+4                                                          
         ST    R8,FULL                                                          
         GOTO1 VCOMPRNT                                                         
         GOTO1 REPORT                                                           
PTSEXIT  DS    0H                                                               
         XMOD1 1                                                                
         EJECT                                                                  
P06GRID  BAS   R9,LINEA                                                         
         CLI   DUB,0               DATE PRESENT                                 
         BE    P06GRID1             NO                                          
         MVC   0(5,RE),DUB                                                      
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
P06GRID1 ZIC   RF,RPPRD                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         CLI   0(RE),0                                                          
         BNE   *+10                                                             
         MVC   0(3,RE),=C'** '                                                  
         CLI   PGDSNO,1                                                         
         BE    P06GRID2                                                         
         LA    R9,3(RE)            PRINT NUMBER OF SPOTS                        
         MVI   0(R9),C'-'                                                       
         LA    R9,1(R9)                                                         
         MVI   CURTAB+3,0                                                       
         CURED PGDSNO,(2,(R9)),CURTAB,ALIGN=LEFT                                
P06GRID2 DS    0H                                                               
         CLI   2(RE),C' '                                                       
         BNE   P06GRID3                                                         
         MVC   FULL(3),3(RE)                                                    
         MVC   3(3,RE),=C'   '                                                  
         MVC   2(3,RE),FULL                                                     
P06GRID3 DS    0H                                                               
         BAS   R9,INCR                                                          
         B     PTSGRID7                                                         
         SPACE 2                                                                
P10GRID  BAS   R9,LINEA                                                         
         MVC   0(5,RE),DUB                                                      
         MVI   5(RE),C'-'                                                       
         ZIC   RF,RPPRD                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   6(3,RE),1(RF)                                                    
         BAS   R9,INCR                                                          
         OC    PGDFNO,PGDFNO                                                    
         BZ    P10GRID2                                                         
         MVC   132(8,RE),FILMNO    PRINT FILM NUMBER                            
         BAS   R9,INCR                                                          
P10GRID2 DS    0H                                                               
         CLI   PGDSNO,1                                                         
         BE    PTSGRID7                                                         
         BAS   R9,LINEA                                                         
         MVC   4(4,RE),=C'(  )'                                                 
         LA    R9,5(RE)                                                         
         EDIT  PGDSNO,(2,(R9))                                                  
         BAS   R9,INCR                                                          
         B     PTSGRID7                                                         
         SPACE 2                                                                
P13GRID  BAS   R9,LINEA                                                         
         CLI   DUB,0                                                            
         BE    P13GRID1                                                         
         MVC   0(5,RE),DUB                                                      
         SR    R9,R9                                                            
         ICM   R9,3,PGWNOL                                                      
         LA    R9,1(R9)                                                         
         STCM  R9,3,PGWNOL                                                      
P13GRID1 DS    0C                                                               
         CLI   PGDFDAY,0                                                        
         BE    P13GRID3                                                         
* GET FILM DAY                                                                  
         GOTO1 DATCON,DMCB,(2,RDATE),DUB                                        
         GOTO1 GETDAY,DMCB,DUB,WORK                                             
         NI    DMCB,X'0F'                                                       
         MVC   WORK(6),DUB                                                      
         ZIC   RE,DMCB                                                          
         ZIC   R1,PGDFDAY                                                       
         SLL   R1,25                                                            
         SR    R9,R9                                                            
P13GRD2A LTR   R1,R1                                                            
         BZ    P13GRD2B                                                         
         SLL   R1,1                                                             
         LA    R9,1(R9)                                                         
         B     P13GRD2A                                                         
P13GRD2B DS    0C                                                               
         SR    R9,RE                                                            
         GOTO1 ADDAY,DMCB,WORK,DUB,(R9)                                         
         L     R9,FULL                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(4,(R9))                                     
         GOTO1 CODAY,DMCB,PGDFDAY,DUB                                           
         L     RE,FULL                                                          
         MVI   5(RE),C'-'                                                       
         MVC   6(3,RE),DUB                                                      
P13GRID3 BAS   R9,LINEA                                                         
         ZIC   RF,RPPRD                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         OC    PGDFNO,PGDFNO                                                    
         BZ    *+10                                                             
         MVC   0(8,RE),FILMNO      PRINT FILM NUMBER                            
         BAS   R9,INCR                                                          
         B     PTSGRID7                                                         
         EJECT                                                                  
LINEA    LR    RE,R4                                                            
         SR    RF,RF                                                            
         ICM   RF,3,PGWNOL                                                      
         MH    RF,=H'70'                                                        
         AR    RE,RF                                                            
         BR    R9                                                               
         SPACE 1                                                                
INCR     SR    RE,RE                                                            
         ICM   RE,3,PGWNOL                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,PGWNOL                                                      
         BR    R9                                                               
         SPACE 1                                                                
PBEDIT   DS    0H                                                               
         MVI   CURTAB+3,0                                                       
         CURED (RE),(3,DUB),CURTAB,ALIGN=LEFT                                   
         BR    R9                                                               
         SPACE 1                                                                
         LTORG                                                                  
PGELEM   DS    CL30                                                             
         EJECT                                                                  
SORTC    NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         CLI   SORTPASS,1          INPUT PHASE                                  
         BNE   SORTOUT              NO - DO OUTPUT                              
         OC    SSCNTR,SSCNTR                                                    
         BNZ   SORTIN1                                                          
         L     RE,VSSTABLE                                                      
         L     RF,=AL4(SSTABLQ)                                                 
         XCEF                                                                   
         L     RE,VPNTABLE                                                      
         L     RF,=AL4(PNTABLQ)                                                 
         XCEF                                                                   
SORTIN1  DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING PNAMD,RE                                                         
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   WORK(1),PDNCNTR+3                                                
         MVC   PNDNAME,BDPROG+4                                                 
         MVC   PNDNAME+15(1),BDSEC                                              
         L     R9,PDNCNTR                                                       
         L     R8,VPNTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,WORK),(R8),(R9),18,(1,17),255                    
         OC    DMCB,DMCB                                                        
         BNZ   *+6                                                              
         DC    H'0'                PROGRAM NAME TABLE IS FULL                   
         MVC   PDNCNTR,DMCB+8                                                   
         L     R1,DMCB                                                          
         MVC   CURRPNUM,0(R1)                                                   
         XC    WORK,WORK                                                        
         DROP  RE                                                               
         LA    RE,WORK                                                          
         USING SQSTART,RE                                                       
         MVC   SORTKLEN,=F'4'                                                   
         MVC   SORTRLEN,=F'8'                                                   
         MVC   DADRDISP,=F'4'                                                   
         CLI   SORTFRMT,1                                                       
         BNE   SORTIN2                                                          
         MVC   SQ1DAY,BDDAY                                                     
         XI    SQ1DAY,X'FF'                                                     
         MVC   SQ1TIME,BDTIMST                                                  
         MVC   SQ1PNUM,CURRPNUM                                                 
         MVC   SQ1DADDR,KEY+14                                                  
         B     SORTADD                                                          
         SPACE 2                                                                
SORTIN2  CLI   SORTFRMT,2                                                       
         BNE   SORTIN3                                                          
         MVC   SQ2DAY,BDDAY                                                     
         XI    SQ2DAY,X'FF'                                                     
         MVC   SQ2TIME,BDTIMST                                                  
         MVC   SQ2PNUM,CURRPNUM                                                 
         MVC   SQ2DADDR,KEY+14                                                  
         B     SORTADD                                                          
SORTIN3  B     SORTCX                                                           
         SPACE 2                                                                
* ADD A RECORD TO THE SORT BUFFER                                               
SORTADD  L     RF,SSCNTR                                                        
         SR    RE,RE                                                            
         M     RE,SORTRLEN                                                      
         A     RF,VSSTABLE                                                      
         MVC   0(20,RF),WORK                                                    
         L     RF,SSCNTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,SSCNTR                                                        
         MVI   SOUTFRST,1                                                       
         B     SORTCX                                                           
         EJECT                                                                  
SORTOUT  CLI   SOUTFRST,1                                                       
         BNE   SRTOUT1                                                          
         L     R4,VSSTABLE                                                      
         L     R5,SSCNTR                                                        
         L     R6,SORTRLEN                                                      
         L     R7,SORTKLEN                                                      
         GOTO1 XSORT,DMCB,(R4),(R5),(R6),(R7),0                                 
         MVI   SOUTFRST,0                                                       
         MVC   NEXTSSLT,VSSTABLE                                                
         L     RE,ADBUY                                                         
         ST    RE,AREC                                                          
         SPACE 2                                                                
SRTOUT1  L     RE,NEXTSSLT                                                      
         A     RE,DADRDISP                                                      
         MVC   KEY+14(4),0(RE)                                                  
         OC    KEY+14(4),KEY+14                                                 
         BNZ   SRTOUT2                                                          
         MVI   SORTPASS,3                                                       
         MVI   SOUTFRST,1                                                       
         XC    SSCNTR,SSCNTR                                                    
         B     SORTCX                                                           
SRTOUT2  L     RE,ADBUY            GET A BUY RECORD                             
         ST    RE,AREC                                                          
         GOTO1 GET                                                              
         L     RE,ADBUY                                                         
         MVC   KEY(13),0(RE)                                                    
         L     RE,NEXTSSLT                                                      
         XC    CURRSORT,CURRSORT   SET CURRENT KEY AND NEXT KEY                 
         XC    NEXTSORT,NEXTSORT                                                
         L     R9,SORTKLEN                                                      
         BCTR  R9,0                                                             
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   CURRSORT(0),0(RE)                                                
         A     RE,SORTRLEN                                                      
         ST    RE,NEXTSSLT                                                      
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   NEXTSORT(0),0(RE)                                                
         MVI   SORTPASS,2                                                       
SORTCX   XMOD1 1                                                                
         LTORG                                                                  
SORTKLEN DC    F'0'                SORT KEY LENGTH                              
SORTRLEN DC    F'0'                SORT RECORD LENGTH                           
DADRDISP DC    F'0'                DISK ADDRESS DISPLACEMENT                    
NEXTSSLT DC    F'0'                NEXT SORT SLOT                               
SOUTFRST DC    X'00'               FIRST TIME SWITCH                            
         EJECT                                                                  
*=================================================================              
* PRINT NETWORK COST OVERRIDES AS CAPTIONS BELOW THE GRID                       
*=================================================================              
                                                                                
PRCOSTOV NTR1  BASE=*,LABEL=*                                                   
         USING SP60WK,R2                                                        
*                                                                               
         L     R4,=A(NETCOVRD)                                                  
*                                                                               
PRCOV2   OC    0(6,R4),0(R4)                                                    
         JZ    EXIT                                                             
         CLC   0(2,R4),PASSSD2     TEST PRIOR TO PASS START                     
         BNL   PRCOV4                                                           
         AHI   R4,6                                                             
         B     PRCOV2                                                           
*                                                                               
PRCOV4   CLC   0(2,R4),PASSED2     TEST AFTER PASS END                          
         JH    EXIT                YES - DONE                                   
*                                                                               
PRCOV6   MVC   P1+4(28),=C'** NETWORK COST OVERRIDES **'                        
*                                                                               
         LA    R5,P1+40                                                         
*                                                                               
PRCOV10  ST    R5,FULL             SAVE PRINT LINE ADDR                         
         LHI   R6,4                                                             
*                                                                               
PRCOV14  GOTO1 DATCON,DMCB,(2,0(R4)),(4,0(R5))  GET MMMDD                       
*                                                                               
         LA    RF,6(R5)            POINT TO NEXT PRINT POSN                     
         CLI   2(R4),1                                                          
         BNH   PRCOV16                                                          
         BCTR  RF,0                BACK UP ONE POSN                             
         MVI   0(RF),C'-'                                                       
         SR    R0,R0                                                            
         IC    R0,2(R4)           SPOT SEQNUM                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,RF),DUB                                                      
         AHI   RF,4                                                             
*                                                                               
PRCOV16  SR    R0,R0                                                            
         ICM   R0,7,3(R4)          GET COST AMOUNT                              
         BNZ   PRCOV18                                                          
         MVC   0(3,RF),=C'$0 '                                                  
         B     PRCOV20                                                          
*                                                                               
PRCOV18  EDIT  (R0),(10,0(RF)),2,ALIGN=LEFT,FLOAT=$,ZERO=NOBLANK                
         AR    RF,R0               POINT TO END                                 
         AHI   RF,-3                                                            
         CLC   0(3,RF),=C'.00'                                                  
         BNE   *+10                                                             
         MVC   0(3,RF),SPACES                                                   
*                                                                               
PRCOV20  AHI   R4,6                NEXT COST OVERRIDE                           
         OC    0(6,R4),0(R4)                                                    
         BZ    PRCOVX                                                           
         CLC   0(2,R4),PASSED2     TEST AFTER PASS END                          
         BH    PRCOVX                                                           
         AHI   R5,20               NEXT PRINT POSN                              
         BCT   R6,PRCOV14                                                       
*                                                                               
         L     R5,FULL                                                          
         AHI   R5,132              NEXT PRINT LINE                              
         B     PRCOV10                                                          
*                                                                               
PRCOVX   GOTO1 REPORT                                                           
         MVI   P,0                                                              
         GOTO1 REPORT              SKIP A LINE                                  
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*BUILD A LIST OF CUTIN SPOTS                                                    
*                                                                               
CUTIN    NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SP60WK,R2                                                        
         L     RE,=A(CISLIST)     CUT IN STATION LIST                           
         L     RF,CISLSTLN                                                      
         XCEF                                                                   
         L     RE,=A(CICLIST)                                                   
         ST    RE,NXTCI                                                         
         L     RF,CICLSTLN                                                      
         XCEF                                                                   
         L     RE,=A(CISPRD)       PRODUCT LIST FOR STATION                     
         L     RF,CISPRDLN                                                      
         XCEF                                                                   
         L     RE,=A(CINPRD)       PRODUCT LIST FOR NETWORK                     
         L     RF,CINPRDLN                                                      
         XCEF                                                                   
*                                                                               
         L     RE,=A(NETCOVRD)     PRODUCT LIST FOR NETWORK                     
         LHI   RF,NETCOVRX-NETCOVRD                                             
         XCEF                                                                   
*                                                                               
         L     RE,ADBUY                                                         
         L     RF,=A(CISLIST)      BUILD POSSIBLE CUTIN LIST                    
         LA    RE,24(RE)                                                        
         USING NTWKELEM,RE                                                      
CI01     CLI   0(RE),0                                                          
         BE    CI04                                                             
         BE    CIEXIT                                                           
         CLI   0(RE),X'68'                                                      
         BE    CI03                                                             
CI02     ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CI01                                                             
         SPACE 2                                                                
CI03     MVC   0(5,RF),NTWKMKST    PUT STATION IN LIST                          
         LA    RF,5(RF)                                                         
         B     CI02                                                             
         DROP  RE                                                               
CI04     DS    0H                                                               
         EJECT                                                                  
* BUILD LIST OF NETWORK PRODUCTS                                                
         LA    RE,CINPRD           SNO/P1/P2                                    
         L     RF,CINPRDLN                                                      
         XCEF                                                                   
         SR    R1,R1                                                            
         LA    RF,CINPRD                                                        
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING REGELEM,RE                                                       
         XC    ELEMDT,ELEMDT                                                    
*                                                                               
CI10     CLI   0(RE),0                                                          
         BE    CI20A                                                            
         CLI   0(RE),X'0B'         FIND REGELEM                                 
         BL    CI102X                                                           
         CLI   0(RE),X'0D'                                                      
         BH    CI102X                                                           
         TM    RSTATUS,X'80'                                                    
         BO    CI102X                                                           
         AHI   R1,1                ALWAYS COUNT ORIGINAL                        
         TM    RSTATUS,X'40'       BYPASS -SPOTS FOR CUTINS                     
         BO    CI102B                                                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,ELEMNUM                                                       
         CLC   ELEMDT,RDATE                                                     
         BE    *+6                                                              
         SR    R0,R0                                                            
         AHI   R0,1                                                             
         STC   R0,ELEMNUM                                                       
         MVC   ELEMDT,RDATE                                                     
*                                                                               
         CLI   RLEN,14                                                          
         BL    CI102A                                                           
         STC   R1,0(RF)                                                         
         MVC   1(1,RF),RPPRD                                                    
         CLI   RLEN,18                                                          
         BL    *+10                                                             
         MVC   2(1,RF),RPPRD+4                                                  
         LA    RF,3(RF)                                                         
         B     CI102B                                                           
*                                                                               
CI102A   STC   R1,0(RF)                                                         
         MVI   1(RF),219                                                        
         LA    RF,3(RF)                                                         
*                                                                               
CI102B   TM    RSTATUS,X'20'       TEST COST OVERRIDE                           
         BZ    CI102X                                                           
         CLI   DETOPTS+2,0         TEST SUPPRESS COSTS                          
         BE    CI102X                                                           
*                                                                               
         L     R4,=A(NETCOVRD)     POINT TO TABLE                               
*                                                                               
CI102C   OC    0(6,R4),0(R4)       TEST FREE ENTRY                              
         BZ    CI102D                                                           
         AHI   R4,6                                                             
         B     CI102C                                                           
*                                                                               
CI102D   MVC   0(2,R4),RDATE       MOVE SPOT DATE                               
         MVC   2(1,R4),ELEMNUM     SAVE SPOT NUMBER THIS DATE                   
         MVC   3(3,R4),RPCOST      SAVE COST OVRD AMOUNT                        
*                                                                               
CI102X   ZIC   R0,RLEN                                                          
         AR    RE,R0                                                            
         B     CI10                                                             
         EJECT                                                                  
* GET STATION RECORDS AND BUILD PRODUCT LIST                                    
CI20A    MVC   CIKEY,KEY                                                        
         L     R9,=A(CISLIST)                                                   
CI20     OC    0(5,R9),0(R9)       LIST OF CUTINS IS COMPLETE                   
         BZ    CI40                                                             
         MVC   KEY,CIKEY                                                        
         MVC   KEY+4(5),0(R9)                                                   
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     CHECK FOR MISSING BUYLINES                   
         BE    *+18                                                             
         LA    R9,5(R9)                                                         
         MVC   KEY,CIKEY                                                        
         B     CI20                                                             
*                                                                               
         GOTO1 GET                                                              
* BUILD A LIST OF STATION PRODUCTS                                              
         L     RE,=A(CISPRD)                                                    
         L     RF,CISPRDLN                                                      
         XCEF                                                                   
         LA    R1,0                                                             
         L     RF,=A(CISPRD)                                                    
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING REGELEM,RE                                                       
CI202    CLI   0(RE),0                                                          
         BE    CI30                                                             
         CLI   0(RE),X'0B'         FIND REGELEM                                 
         BL    CI204                                                            
         CLI   0(RE),X'0D'                                                      
         BH    CI204                                                            
         TM    RSTATUS,X'80'                                                    
         BO    CI204                                                            
         LA    R1,1(R1)            ALWAYS COUNT THE ORIGINAL                    
         TM    RSTATUS,X'40'       BYPASS MINUS SPOTS                           
         BO    CI204                                                            
         XC    3(3,RF),3(RF)       CLEAR OUT COST OVERRIDE AREA                 
         NI    RSTATUS,X'FF'-X'20' CLEAR COST OVERRIDE FLAG                     
         CLI   RLEN,14                                                          
         BL    CI204U              SET UNALLOCATED                              
         STC   R1,0(RF)                                                         
         MVC   1(1,RF),RPPRD                                                    
         CLI   RLEN,18                                                          
         BL    *+10                                                             
         MVC   2(1,RF),RPPRD+4                                                  
         BAS   R8,CI206                                                         
         LA    RF,6(RF)                                                         
CI204    ZIC   R0,RLEN                                                          
         AR    RE,R0                                                            
         B     CI202                                                            
         SPACE 2                                                                
CI204U   DS    0H                                                               
         STC   R1,0(RF)                                                         
         MVI   1(RF),219                                                        
         BAS   R8,CI206                                                         
         LA    RF,6(RF)                                                         
         B     CI204                                                            
*                                                                               
CI206    CLI   QPNAME,C'Y'         TEST PRODUCT NAME LEGEND REQUIRED            
         BNER  R8                                                               
*                                                                               
         STM   RE,R1,WORK          ADD PRODUCTS TO LIST                         
         LA    R1,1(RF)                                                         
         BRAS  RE,ADDPRD                                                        
         CLI   2(RF),0                                                          
         BE    CI206X                                                           
         LA    R1,2(RF)                                                         
         BRAS  RE,ADDPRD                                                        
*                                                                               
CI206X   LM    RE,R1,WORK                                                       
         BR    R8                                                               
*                                                                               
ADDPRD   NTR1                                                                   
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
*                                                                               
         L     RE,VPRDLST                                                       
ADDPRD2  CLI   0(RE),0                                                          
         JNE   *+14                                                             
         MVC   0(3,RE),1(RF)                                                    
         J     ADDPRDX                                                          
         CLC   0(3,RE),1(RF)                                                    
         JE    ADDPRDX                                                          
         AHI   RE,3                                                             
         J     ADDPRD2                                                          
ADDPRDX  XIT1                                                                   
         EJECT                                                                  
* LIST OF STATION SPOTS IS BUILT                                                
* MATCH IT TO NETWORK SPOTS                                                     
*                                                                               
CI30     LA    RE,CINPRD                                                        
         L     RF,=A(CISPRD)                                                    
CI301    CLI   0(RF),0             END STATION                                  
         BE    *+12                                                             
         CLI   0(RE),0             END NETWORK                                  
         BNE   *+12                                                             
         LA    R9,5(R9)             SET NEXT STATION                            
         B     CI20                                                             
**NOP    CLI   QOPT1,C'Y'          SAVE COST OVR IF REQUIRED                    
**NOP    BNE   CI301A                                                           
**NOP    OC    3(3,RF),3(RF)                                                    
**NOP    BNZ   CI304                                                            
CI301A   CLC   0(3,RE),0(RF)       MATCH PRODUCTS                               
         BNE   CI304                                                            
CI302    LA    RE,3(RE)                                                         
         LA    RF,6(RF)                                                         
         B     CI301                                                            
         SPACE 2                                                                
CI304    L     R1,NXTCI            SAVE CUTIN                                   
         MVC   0(3,R1),0(RF)                                                    
         MVC   3(5,R1),0(R9)                                                    
**NOP    MVC   8(3,R1),3(RF)       SAVE CUTIN COST OVR                          
         LA    R1,11(R1)                                                        
         ST    R1,NXTCI                                                         
         B     CI302                                                            
         SPACE 2                                                                
* LIST OF CUTINS IS BUILT - SORT INTO SPOT ORDER                                
CI40     DS    0H                                                               
         LA    R1,0                                                             
         L     RE,=A(CICLIST)                                                   
CI401    CLI   0(RE),0             COUNT CUTINS                                 
         BE    CI402                                                            
         LA    R1,1(R1)                                                         
         LA    RE,11(RE)                                                        
         B     CI401                                                            
*                                                                               
CI402    LR    R4,R1                                                            
         GOTO1 XSORT,DMCB,A(CICLIST),(R4),11,8,0                                
*                                                                               
CIEXIT   L     RE,=A(CICLIST)      SAVE SORTED CUTIN LIST                       
         ST    RE,ACISLIST                                                      
         MVC   KEY,CIKEY           RESTORE NETWORK RECORD                       
         GOTO1 HIGH                                                             
         GOTO1 GET                                                              
         XIT1                                                                   
         LTORG                                                                  
CIKEY    DS    CL20                                                             
NXTCI    DS    F                                                                
CINPRDLN DC    F'4000'                                                          
CISPRDLN DC    F'4000'                                                          
CISLSTLN DC    F'4000'                                                          
CICLSTLN DC    F'30000'                                                         
CINPRD   DS    4000C                                                            
CISPRD   DS    4000C                                                            
CISLIST  DS    4000C                                                            
CICLIST  DS    30000C                                                           
         EJECT                                                                  
PTSGD    DSECT                                                                  
GRIDST   DS    F                   START OF GRID                                
PRPGWMAX DS    F         <<<  THIS IS CLEARED & SET BUT NOT REFERENCED!         
PRPGDWK  DS    CL2                 PREVIOUS BLOCK DATES                         
PGPRLNO  DS    CL3                 PREVIOUS LINE/SUN LINE/SLOT                  
PGWNOL   DS    H                   NUMBER OF LINES IN THIS BLOCK                
GRIDLEN  DS    C                                                                
GRIDSW1  DS    C                                                                
GRIDSLN  DS    C                                                                
PGWKCNT  DS    F                   WEEKLY SLOT COUNTER                          
         EJECT                                                                  
*                                                                               
SPRN02   CSECT                                                                  
*                                                                               
DICSECT  DS    0D                                                               
*        PRINT GEN                                                              
DCLIST   DS    0C                                                               
         DCDDL SP#SPREP,21,C                                                    
         DCDDL SP#SUM,17,C                                                      
         DCDDL SP#STATN,7                                                       
         DCDDL SP#PKMST,7                                                       
         DCDDL SP#ORMST,7                                                       
         DCDDL SP#RVMST,7                                                       
         DCDDL SP#MG,4,C                                                        
         DCDDL SP#MST,4,R                                                       
         DCDDL SP#TOTL,5                                                        
         DCDDL SP#TLCST,6                                                       
         DCDDL SP#BRDCS,7                                                       
         DCDDL SP#DAY,4,C                                                       
         DCDDL SP#DAY,4,CU,LABEL=SPUDAY                                         
         DCDDL SP#TIME,5,C                                                      
         DCDDL SP#TIME,5,CU,LABEL=SPUTIME                                       
         DCDDL SP#PROG,13,C                                                     
         DCDDL SP#PROG,13,CU,LABEL=SPUPROG                                      
         DCDDL SP#LEN,4,C                                                       
         DCDDL SP#LEN,4,CU,LABEL=SPULEN                                         
         DCDDL SP#ROTSC,19,C                                                    
         DCDDL SP#NTRSC,29,C                                                    
         DCDDL SP#NTRSC,29,CU,LABEL=SP0NTRSC                                    
         DCDDL SP#PROLE,24,C                                                    
         DCDDL SP#UNKN,7                                                        
DCLISTX  DC    X'00'                                                            
*                                                                               
DSLIST   DS    0C                                                               
         DSDDL PRINT=YES                                                        
DSLISTX  EQU   *                                                                
         EJECT                                                                  
PRDLST   DS    0D                                                               
         DS    670C                                                             
PRDLSTL  EQU   *-PRDLST                                                         
         EJECT                                                                  
SP60WK   DS    0D                                                               
RTYPE    DS    CL3                                                              
BUFHI    DS    C                                                                
BUFCDE   DS    C                                                                
BUFRTYP  DS    C                                                                
LEVEL    DS    C                   LEVEL CODE                                   
ESTACT   DS    CL1                                                              
CPPSW    DC    C'D'                                                             
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5)                                                       
DNAMES   DS    0CL28                                                            
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
SUBPROG  DS    0CL8                                                             
         DC    C'SP'                                                            
SUBPROG1 DC    C'M2'                                                            
SUBPROG2 DC    C'01'                                                            
         DC    C'  '                                                            
DASH     DC    80C'-'                                                           
MYBUFIO  DS    CL200                                                            
FIRST    DS    C                                                                
PASS     DS    C                                                                
MAXPASS  DS    C                   HIGHEST REQUIRED PASS                        
SCNDDTSW DC    X'01'               PRINT DATE ON SECOND LINE                    
IDSW     DS    C                   0=NEW PAGE/ID, 1=MULTI IDS ON PAGE           
FBCONTR  DS    C                   FIRST BUY FOR CONTRACT (FBSTA)               
PRTLINE  DS    CL132                                                            
VCALCPP  DC    F'0'                                                             
VSUMMRY  DC    F'0'                                                             
APTSDESC DC    F'0'                                                             
APRSDESC DC    F'0'                                                             
PSTASPT  DC    F'0'                                                             
PSTACOST DC    F'0'                                                             
PSTAGRID DC    F'0'                                                             
DSTAGRID DC    F'0'                A(DETAIL STATION GRID PRINT)                 
DDESC    DC    F'0'                A(DETAIL DESCRIPTION PRINT)                  
DTOTSPT  DC    F'0'                A(DETAIL TOTAL SPOTS)                        
PENNYSW  DS    C                                                                
SPACESW  DS    CL1                                                              
*                                                                               
SVQUEST  DS    CL3                                                              
NXSW     DS    CL1                                                              
NXNEWSTA EQU   X'01'                                                            
NXOPEN   EQU   X'02'                                                            
*                                                                               
HLDPNAM  DS    CL14                                                             
         DS    0F                                                               
HPSNO    DS    C                                                                
LASTGSLT DS    F                                                                
HLDBOOK  DS    CL2                                                              
         DS    0F                                                               
SALSDATA DS    0CL28               SALESPERSONS WORK AREA                       
SALSWKS  DS    F                                                                
SALSSPT  DS    F                                                                
SALSD1   DS    F                                                                
SALSD2   DS    F                                                                
SALSD3   DS    F                                                                
SALSD4   DS    F                                                                
SALSDL   DS    F                                                                
PRTADDR  DS    F                                                                
SVPH01   DS    F                                                                
SVPH02   DS    F                                                                
SVPH04   DS    F                                                                
SVSPECS  DS    F                                                                
SVMDTAB  DS    F                                                                
SVRDTE   DS    F                                                                
VMDADDWT DC    F'0'                                                             
VSTATOT  DC    F'0'                                                             
VEDTDEMS DC    F'0'                                                             
VHDATES  DC    F'0'                                                             
VGETBUF  DC    F'0'                                                             
VSUBPARA DC    F'0'                                                             
VPGRID   DS    F                                                                
VCOMPRNT DS    F                                                                
PGHILNO  DS    F                   HIGHEST LINE NUMBER                          
PGNOENT  DS    F                                                                
PGCNDSW  DS    C                   CONDENSE LIKE SPOTS                          
PGCURLNO DS    F                   CURRENT LINE #                               
PGWMAX   DS    F                   MAXIMUM SLOTS                                
VARFRMT  DS    C                   VARIABLE FORMAT                              
AHDATES  DC    F'0'                                                             
SVRCSUB  DS    C                                                                
MSRCSUB  DS    C                                                                
SVSUPMKT DS    C                                                                
MSSUPMKT DS    C                                                                
DPTSW    DS    C                   DAYPART CHANGE SWITCH                        
MRPTTYP  DS    C                                                                
BUYACT   DS    C                                                                
CFDS     DS    C                                                                
CFDE     DS    C                                                                
OVRFLAG  DS    CL16                                                             
STRDTE   DS    CL2                                                              
ENDDTE   DS    CL2                                                              
SVMGC1   DS    F                                                                
SVMGC2   DS    F                                                                
SVPGC1   DS    F                                                                
SVPGC2   DS    F                                                                
MSHDHOOK DC    F'0'                                                             
SVHDHOOK DC    F'0'                                                             
MSSPHK   DC    F'0'                                                             
SVSPHK   DC    F'0'                                                             
VGETREP  DC    F'0'                                                             
MYSPTHOK DC    F'0'                                                             
VFOOT    DC    F'0'                                                             
*                                                                               
* DETAIL OPTIONS - CONTROLLED BY QOPT2                                          
*                     1=YES,0=NO                                                
*                  QOPT1 OVERRIDES PRINT COST OPTION                            
*                                                                               
*               FIELD  OPTION                                                   
*               -----  ------                                                   
*                 1    PRINT '*' NEXT TO OVERRIDEN DEMOS                        
*                 2    PRINT DEMO VALUES                                        
*                 3    PRINT COST                                               
*                 4    PRINT CPP                                                
DETOPTS  DS    CL4                 CURRENT OPTIONS SAVE                         
DETOPT   DC    AL1(1,1,1,1)        OPTION TABLE                                 
         DC    AL1(0,1,1,1)                                                     
         DC    AL1(0,0,1,1)                                                     
         DC    AL1(0,1,0,0)                                                     
         DC    AL1(1,1,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
* TOTAL OPTIONS - CONTROLLED BY QOPT5                                           
*                        1=YES,0=NO                                             
*                                                                               
*               FIELD  OPTION                                                   
*               -----  ------                                                   
*                 1    PRINT TELECASTS                                          
*                 2    PRINT DOLLARS                                            
*                 3    PRINT DEMOS                                              
*                                                                               
SUMOPTS  DS    CL3                                                              
SUMOPT   DC    AL1(1,1,1)                                                       
         DC    AL1(1,1,0)                                                       
         DC    AL1(1,0,0)                                                       
         DC    AL1(1,0,1)                                                       
         DC    AL1(0,0,1)                                                       
         DC    AL1(0,1,0)                                                       
         DC    AL1(0,1,1)                                                       
         DC    AL1(0,0,0)                                                       
*                                                                               
* DATE OPTIONS - CONTROLLED BY PROFDCTL                                         
*                        SETS VARFRMT AND SCNDDTSW                              
DATEOPTS DC    X'00',AL1(0,0)                                                   
         DC    C' ',AL1(0,0)                                                    
         DC    C'0',AL1(0,0)                                                    
         DC    C'1',AL1(0,1)                                                    
         DC    C'2',AL1(1,0)                                                    
         DC    C'3',AL1(1,1)                                                    
         DC    C'4',AL1(2,0)                                                    
         DC    C'5',AL1(2,1)                                                    
         DC    X'FF',AL1(0,0)                                                   
DATEOPT  DS    CL2                                                              
* SORT OPTIONS      0 = NO SORT                                                 
*                   1 = DAY/TIME/PROGRAM                                        
*                   2 = TIME/DAY/PROGRAM                                        
SORTOPTS DC    C' ',AL1(0)                                                      
         DC    C'1',AL1(1)                                                      
         DC    C'2',AL1(2)                                                      
         DC    X'FF',AL1(0)                                                     
SORTOPT  DS    CL1                                                              
*                                                                               
* REPORT FORMAT OPTIONS                                                         
*                   FIELD1 = LENGTH OF GRID                                     
*                   FIELD2 = NUMBER IN GRID FOR PTS                             
*                   FIELD3 = NUMBER IN GRID FOR RS                              
*                                                                               
FRMTOPTS DC    X'00',AL1(5,13,13)                                               
         DC    C'0',AL1(5,13,13)                                                
         DC    C'1',AL1(6,11,11)                                                
         DC    C'2',AL1(10,6,6)                                                 
         DC    C'3',AL1(13,5,5)                                                 
         DC    X'FF',AL1(5,13,13)                                               
FRMTOPT  DS    CL3                                                              
* CONDENSE OPTIONS                                                              
*                        SETS PGCNDSW                                           
*                                                                               
CNDSOPTS DC    X'00',AL1(0)                                                     
         DC    C'1',AL1(1)                                                      
         DC    C'2',AL1(2)                                                      
         DC    X'FF',AL1(0)                                                     
CNDSOPT  DS    CL1                                                              
SVDEMS   DS    0F                                                               
SVD1     DS    F                   DEMO 1 VALUE                                 
SVD1CP   DS    F                   DEMO 1 CPP/CPM                               
SVD2     DS    F                   DEMO 2 VALUE                                 
SVD2CP   DS    F                   DEMO 2 CPP/CPM                               
SVD3     DS    F                   DEMO 3 VALUE                                 
SVD3CP   DS    F                   DEMO 3 CPP/CPM                               
SVD4     DS    F                   DEMO 4 VALUE                                 
SVD4CP   DS    F                   DEMO 4 CPP/CPM                               
SVDG     DS    F                                                                
SVDGCP   DS    F                                                                
ACTMO    DS    F                                                                
UNIVERSE DS    F                                                                
WEIGHT   DS    F                                                                
MCOUNT   DS    F                                                                
PLDEMS   DS    0CL44                                                            
PLD1     DS    CL5                                                              
PLD1CP   DS    CL6                                                              
PLD2     DS    CL5                                                              
PLD2CP   DS    CL6                                                              
PLD3     DS    CL5                                                              
PLD3CP   DS    CL6                                                              
PLD4     DS    CL5                                                              
PLD4CP   DS    CL6                                                              
STAGRID  DS    14F                                                              
STASPOT  DS    F                   STATION TOTAL SPOTS                          
STADEMS  DS    8F                  STATION TOTAL DEMOS                          
STACOST  DS    2F                  STATION TOTAL DOLLARS                        
STTSPOT  DS    F                   OVERALL STATION COST                         
STTDEMS  DS    8F                  OVERALL STATION DEMOS                        
STTCOST  DS    2F                  OVERALL STATION DOLLARS                      
VSVMDBLK DS    F                                                                
FILMNO   DS    CL8                                                              
STACAP   DS    CL7                                                              
MSOPT    DS    CL7                                                              
SVOPTS   DS    CL7                                                              
SUBPSW   DS    CL1                                                              
MSPROF   DS    CL16                                                             
SVPROF   DS    CL16                                                             
PASSSD3  DS    XL3                                                              
PASSED3  DS    XL3                                                              
PASSSD2  DS    XL2                                                              
PASSED2  DS    XL2                                                              
ELEMNUM  DS    XL1                                                              
ELEMDT   DS    XL2                                                              
PKGAREA  DS    CL16                BUY TYPE CAPTIONS                            
PIGAREA  DS    CL32                PIGGYBACK AREA                               
PIGAREAL DS    CL3                 PIGGYBACK LENGTH                             
PIGPRNT  DS    CL11                PIGGYBACK PRINT                              
STADDRNM DS    CL(L'ANAME)                                                      
         DS    0F                                                               
PREMTAB  DS    CL64                                                             
HIATAB   DS    CL64                                                             
COMAREA  DS    CL500               COMMENT AREA                                 
COVRHLD  DS    596C                COST OVERRIDE HOLD AREA                      
COVRFRST DS    C                   COST OVR FIRST TIME                          
OVRCNT   DS    C                   PL OVERRIDE COUNT                            
SVMAXLIN DS    C                                                                
MGSW     DS    C                                                                
APL      DS    F'0'                A(PL) FOR COST OVERRIDES                     
SVQEND   DS    CL6                                                              
SVSPREP  DS    H                                                                
SVP1     DS    CL40                                                             
SVP2     DS    CL40                                                             
SVP3     DS    CL40                                                             
SVP4     DS    CL40                                                             
NUMWK    DS    F                                                                
NOINGRID DS    H                                                                
LENGRID  DS    C                                                                
CURRSORT DS    CL20                                                             
NEXTSORT DS    CL20                                                             
OPTRMODE DS    C                                                                
OPTRPT   DS    F                                                                
VPNTABLE DC    F'0'                                                             
PDNCNTR  DC    F'0'                                                             
VSSTABLE DC    F'0'                                                             
SSCNTR   DC    F'0'                                                             
VRSORT   DC    F'0'                                                             
ACISLIST DC    F'0'                                                             
VSTACUT  DC    F'0'                                                             
ASTACUT  DC    F'0'                                                             
VPLAREA  DC    F'0'                                                             
SSTAGRD  DC    F'0'                                                             
REPCALOV DC    F'0'                                                             
CURRCIS  DC    F'0'                                                             
CSPNO    DC    F'0'                                                             
VPRDLST  DS    F                                                                
SORTPASS DS    C                                                                
SORTREQ  DS    C                                                                
SORTFRMT DS    C                                                                
CURRPNUM DS    C                                                                
PASSTAB  DS    CL144               LIST OF PASS START-END DATES                 
PASSQST  DS    CL12                THIS PASS START-END                          
REASTART DS    CL12                REQUEST START-END DATES                      
*                                                                               
QPNAME   EQU   Q2USER+0            Y=PRINT PRODUCT NAME LEGEND                  
*                                                                               
PASSTABD DSECT                     SAVE MEDBLOCKS FOR PASSES                    
PASSSD   DS    CL6                 START DATE                                   
PASSED   DS    CL6                 END DATE                                     
PASSP1   DS    CL28                MEDBLOCK LEADER                              
PASSP2   DS    CL168               WEEKS                                        
PASSP3   DS    CL48                MONTHS                                       
PASSP4   DS    CL12                PERIOD                                       
PASSEND  DS    0C                                                               
*                                                                               
SPRN02   CSECT                                                                  
PNTABLE  DS    0D                                                               
PNTABLQ  EQU   5200                                                             
         DS    (PNTABLQ)C                                                       
SSTABLE  DS    0D                                                               
SSTABLQ  EQU   14200                                                            
         DS    (SSTABLQ)C                                                       
PGRIDC   DS    0D                                                               
PGRIDCLQ EQU   20000                                                            
PGRIDMXQ EQU   (PGRIDCLQ/PGDLEN)-1 MAX ENTRIES THAT WILL FIT                    
         DS    (PGRIDCLQ)C                                                      
SVMDBLK  DS    0D                                                               
         DS    1208C               SAVE MEDIA SUMMARY MEDBLOCK                  
*                                                                               
         DS    0D                                                               
NETCOVRD DS    200XL6              NETWORK COST OVERRIDES                       
NETCOVRX EQU   *                                                                
*STACUT   DS   0D                                                               
*STACUTLQ EQU   30000                                                           
*         DS    (STACUTLQ)C                                                     
         LTORG                                                                  
SUBPAREA DS    0D                                                               
         DS    90000C                                                           
CIGLIST  DS    0D                                                               
CIGLSTNQ EQU   2000                #ENTRIES                                     
CIGLSTLQ EQU   CIGLSTNQ*CINNTRLQ   L'REQUIRED FOR #ENTRIES                      
         DS    (CIGLSTLQ)C                                                      
PLAREA   DS    0D                                                               
PLAREALQ EQU   50000                                                            
PLARMXQ  EQU   (PLAREALQ/70)-1     MAX GRID LINES THAT WILL FIT                 
         DS    (PLAREALQ)C         PRINT LINE BUFFER                            
         EJECT                                                                  
CINNTRYD DSECT                     CUT-IN TABLE ENTRY                           
         DS    CL3        +0       - PRODUCT                                    
         DS    CL5        +3       - STATION                                    
         DS    CL3        +8       - COST OVERRIDE                              
CINNTRLQ EQU   *-CINNTRYD                                                       
         EJECT                                                                  
BPRTD    DSECT                                                                  
BPRTSPT  DS    CL10                 0                                           
BPRTD1   DS    CL7                 11                                           
BPRTDL   DS    CL9                 18                                           
BPRTD1C  DS    CL8                 27                                           
BPRTD2   DS    CL7                 35                                           
BPRTD2C  DS    CL8                 43                                           
BPRTD3   DS    CL7                 51                                           
BPRTD3C  DS    CL8                 58                                           
BPRTD4   DS    CL7                 68                                           
BPRTD4C  DS    CL8                 76                                           
         EJECT                                                                  
PGRIDD   DSECT                                                                  
PGSORT   DS    CL11                                                             
PGLINNO  DS    C                   PRINT BLOCK LINE NUMBER                      
PGSUBLI  DS    C                   SUB-LINE NUMBER                              
PGLSLOT  DS    C                   PRINT BLOCK SLOT NUMBER                      
PGDWK    DS    CL2                 WEEK OF                                      
PGDIND   DS    CL1                 REG/MISSD/MG INDICATOR                       
PGDSBRN  DS    CL1                 SORT BRAND                                   
PGDSSLN  DS    CL1                 SORT SPOT LENGTH                             
PGDSNO   DS    CL1                 SORT SPOT NUMBER                             
PGDELAD  DS    CL4                 ELEMENT ADDRESS                              
PGDFDAY  DS    CL1                                                              
PGDFNO   DS    CL2                                                              
PGDCUTIN DS    CL4                 A(CUTIN LIST)                                
PGDEND   DS    0C                                                               
PGDLEN   EQU   PGDEND-PGSORT                                                    
PGDLN1   EQU   PGDELAD-PGLINNO                                                  
PGSRTLN  EQU   L'PGSORT                                                         
         SPACE 2                                                                
PGSRT1D  DSECT                                                                  
PGDS1WK  DS    CL2                                                              
PGDS1SLT DS    C                                                                
PGDS1BR  DS    CL1                                                              
PGDS1SL  DS    CL1                                                              
PGDS1IND DS    CL1                                                              
         SPACE 2                                                                
PGSRT2D  DSECT                                                                  
PGDS2SLT DS    CL1                                                              
PGDS2WK  DS    CL2                                                              
PGDS2SNO DS    C                                                                
PGDS2BR  DS    CL1                                                              
PGDS2SL  DS    CL1                                                              
PGDS2IND DS    CL1                                                              
         EJECT                                                                  
PROFDSCT DSECT                  ***PROGRAM PROFILE 1 DSECT***                   
PROFPLEG DS    CL1        +0       PRODUCT LEGEND                               
*PROFSORT DS    CL1                SORT CONTROL                                 
PROFFRMT DS    CL1                 PRINT FORMAT                                 
PROFCNDS DS    CL1                 CONDENSE CONTROL                             
PROFDCTL DS    CL1                 DATE CONTROL                                 
PROFDPC  DS    CL1                 DETAIL PRINT CONTROL                         
PROFCC   DS    CL1                 COMMENT CONTROL                              
PROFSNAM DS    CL1        +6       PRINT STATION NAME (FROM ADDR REC)           
PROFMSR  DS    CL1                 MARKET/STATION RECAP                         
PROFMPC  DS    CL1                 MARKET PRINT CONTROL                         
PROFDPT  DS    CL1                 MARKET DAYPART PRINT CONTROL                 
PROFMTR  DS    CL1                 MARKET TOTAL REPORT                          
         DS    CL1                                                              
PROFBMS  DS    CL1                 BRAND MEDIA SUMMARY NUMBER                   
PROFPMS  DS    CL1                 POL MEDIA SUMMARY NUMBER                     
PROFPAGE DS    CL1        +14      PAGE BREAK AT CONTRACT                       
         DS    CL2                                                              
         EJECT                                                                  
PNAMD    DSECT                                                                  
PNDCODE  DS    CL1                 PROGRAM NUMBER                               
PNDNAME  DS    CL17                                                             
         SPACE 2                                                                
SEQSORT  DSECT                                                                  
SQSTART  DS    0C                                                               
SQ1DAY   DS    CL1                 DAY                                          
SQ1TIME  DS    CL2                 START-END QUARTER HOURS                      
SQ1PNUM  DS    CL1                 PROGRAM NUMBER                               
SQ1DADDR DS    CL4                 DISK ADDRESS                                 
SQ1END   DS    0C                                                               
         ORG   SQSTART                                                          
SQ2TIME  DS    CL2                                                              
SQ2DAY   DS    CL1                                                              
SQ2PNUM  DS    CL1                                                              
SQ2DADDR DS    CL4                                                              
SQ2END   DS    0C                                                               
         EJECT                                                                  
SUMDSECT DSECT                                                                  
SUMKEY   DS    0CL15                                                            
SUMCODE  DS    CL1                 X'90'                                        
SUMDPGNO DS    CL1                 DAYPART GROUP NO.                            
SUMDPGRP DS    CL3                 DAYPART GROUP CODE                           
SUMDPNO  DS    CL1                 DAYPART NO.                                  
SUMDPART DS    CL3                 DAYPART CODE                                 
SUMSLN   DS    CL1                 SPOT LENGTH                                  
SUMRTYP  DS    CL1                 1=WEEKLY,2=MONTHLY,3=PERIOD                  
SUMDT    DS    CL4                 START-END DATES(FFFF FOR TOTAL)              
SUMRPT   DS    CL1                 REPORT CODE                                  
SUMDATA  DS    0CL60                                                            
SUMSPOTS DS    CL4                 SPOTS                                        
SUMDL    DS    CL4                 DOLLARS                                      
SUMDLEQ  DS    CL4                 DOLLARS EQU                                  
SUMD1    DS    CL4                 DEMO 1                                       
SUMD1EQ  DS    CL4                 DEMO 1 EQU                                   
SUMD2    DS    CL4                 DEMO 2                                       
SUMD2EQ  DS    CL4                 DEMO 2 EQU                                   
SUMD3    DS    CL4                 DEMO 3                                       
SUMD3EQ  DS    CL4                 DEMO 3 EQU                                   
SUMD4    DS    CL4                 DEMO 4                                       
SUMD4EQ  DS    CL4                 DEMO 4 EQU                                   
SUMGDL   DS    CL4                 GOAL $                                       
SUMGDLE  DS    CL4                 GOAL $ EQU                                   
SUMGD1   DS    CL4                 GOAL DEMO                                    
SUMGD1E  DS    CL4                 GOAL DEMO EQU                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPMEDBDESD                                                     
       ++INCLUDE SPGENCLT                                                       
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
* SPDDEQUS                                                                      
       ++INCLUDE SPDDEQUS                                                       
* DDDICTATED                                                                    
       ++INCLUDE DDDICTATED                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056SPREPRN02 11/19/19'                                      
         END                                                                    
