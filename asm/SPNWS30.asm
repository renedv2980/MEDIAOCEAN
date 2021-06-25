*          DATA SET SPNWS30    AT LEVEL 060 AS OF 03/05/07                      
*PHASE T20730B,*                                                                
         TITLE 'T20730 - SYSTEM DRIVER FOR NWS DROOL/DRIVER APPLICS'            
T20730   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20730**,RA,R5,RR=RE                                           
         L     R4,0(R1)            A(GLOBAL)                                    
         USING GLOBALD,R4                                                       
         L     R7,GLAWORKD                                                      
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         ST    RE,APRELO                                                        
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
*                                                                               
         L     R9,AIOAREA1                                                      
         USING BWDRECD,R9                                                       
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLINCOMP     INTERNAL COMPUTE TIME                        
         BE    EXEC                                                             
*                                                                               
EXIT     DS    0H                                                               
XIT      XIT1                                                                   
*                                                                               
SPACES   DS    CL132                                                            
         EJECT                                                                  
*============================*                                                  
* RESOLVE ROUTINE ADDRESSES  *                                                  
*============================*                                                  
*                                                                               
RESOLVE  L     R1,=A(ROUTLIST)     TEST ROUTINE IN THIS OVERLAY                 
         A     R1,APRELO                                                        
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    RESOLVEX                                                         
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
*                                                                               
         L     RF,8(R1)                                                         
         A     RF,APRELO                                                        
         ST    RF,GLAROUT          ROUTINE ADDRESS                              
*                                                                               
         XC    APELEM,APELEM       WE'RE GOING TO USE APELEM FOR DPTRD          
*                                                                               
RESOLVEX B     XIT                 RETURN TO DROOL                              
         SPACE 2                                                                
*======================*                                                        
* EXECUTING ROUTINES   *                                                        
*======================*                                                        
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          RF=A(ROUTINE)                                
         BR    RF                  EXECUTE THE ROUTINE                          
         EJECT                                                                  
*===========================*                                                   
* INPUT AND OUTPUT ROUTINES *                                                   
*===========================*                                                   
*                                                                               
*===========================ROWS================================*               
*                                                                               
IMED     MVC   0(1,R2),QMED       MEDIA INPUT                                   
         B     EXIT                                                             
*                                                                               
*                                                                               
IBYR     MVC   0(3,R2),QBYR       BUYER INPUT                                   
         B     EXIT                                                             
*                                                                               
*                                                                               
OMED     DS    0H                 MEDIA OUTPUT                                  
OBYR     B     EXIT               BUYER OUTPUT                                  
*                                                                               
*                                                                               
ICAMP    MVC   0(2,R2),BCAM       CAMPAIGN INPUT                                
         B     EXIT                                                             
*                                                                               
*                                                                               
OCAMP    GOTO1 AGETCAM,0(R2)      CAMPAIGN OUTPUT                               
         GOTO1 AGETCLT,CMPCLTC    (GET CLIENT)                                  
         BNE   OCAMPX                                                           
         XC    LPRDNM1,LPRDNM1                                                  
         XC    LPRDNM2,LPRDNM2                                                  
         CLI   CMPPRD1,0          TEST FOR PIGGBACKS                            
         BE    OCAMP5                                                           
         GOTO1 AGETPRD,CMPPRD1    GET PIGGYBACK PRD 1                           
         MVC   LPRDNM1,PRDNM                                                    
         CLI   CMPPRD2,0                                                        
         BE    OCAMP5                                                           
         GOTO1 AGETPRD,CMPPRD2    GET PIGGYBACK PRD 2                           
         MVC   LPRDNM2,PRDNM                                                    
OCAMP5   GOTO1 AGETPRD,CMPPRDN    (GET PRODUCT)                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN    GET CAMPAIGN ESTIMATE DETAILS                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   OVDEMO,C'Y'        IF OVERIDING DEMOS - DON'T GET FROM           
         BE    OCAMPX             ESTDEMS - USE SAME AS BEFORE                  
*                                                                               
         L     R8,AIOAREA3                                                      
         USING DBLOCKD,R8                                                       
         XC    DBLOCK,DBLOCK                                                    
         XC    APDUB,APDUB                                                      
         XC    DBLOCK,DBLOCK       GET DEMO NAMES                               
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         LA    RE,ESTDEMS                                                       
         MVI   APBYTE,6                                                         
         ICM   RE,8,APBYTE                                                      
         ST    RE,APPARM                                                        
         LA    RE,ESTUSRNM                                                      
         ST    RE,APPARM+12                                                     
         XC    COMDNAMS,COMDNAMS                                                
         GOTO1 VDEMOCON,APPARM,,(2,COMDNAMS),(C'S',DBLOCK)                      
OCAMPX   B     EXIT                                                             
*                                                                               
*                                                                               
OCMP     GOTO1 AGETCAM,0(R2)      CAMPAIGN & PRODUCT OUTPUT                     
         MVC   APHALF,BCAM                                                      
         XC    APHALF,=X'FFFF'                                                  
         EDIT  APHALF,(4,0(R3)),DUB=APDUB,WRK=APWORK,ALIGN=LEFT                 
         MVC   4(3,R3),CMPPRDC                                                  
         B     EXIT                                                             
*                                                                               
*                                                                               
IMKT     MVC   0(2,R2),BMKT       MARKET INPUT                                  
         B     EXIT                                                             
*                                                                               
OMKT     GOTO1 AGETMKT,0(R2)      MARKET OUTPUT                                 
         B     EXIT                                                             
*                                                                               
*                                                                               
ISTA     CLI   READRC,C'D'                                                      
         BNE   ISTA5                                                            
         MVC   0(L'BWDSTA,R2),BWDSTA     STATION INPUT                          
         B     EXIT                                                             
ISTA5    MVC   0(L'BWDSTA,R2),=8C'9'     FORCE GOALS TO BOTTOM                  
         B     EXIT                                                             
*                                                                               
*                                                                               
OSTA     MVI   ZSTA,1             ITS A 9'S STATION                             
         CLC   0(L'BWDSTA,R2),=8C'9'                                            
         BE    EXIT                                                             
         MVI   ZSTA,0             DISPLAY STATION                               
         MVC   0(L'BWDSTA,R3),0(R2)                                             
         CLI   0(R3),C'0'         IF CABLE STATION                              
         BL    *+12                                                             
         MVI   4(R3),C'/'          PRINT XXXX/XXX                               
         B     OSTAX                                                            
         CLI   3(R3),X'00'                                                      
         BNE   OSTA2                                                            
         MVI   3(R3),C'-'         3 CHARACTER STATION                           
         B     OSTA3                                                            
OSTA2    MVI   4(R3),C'-'         4 CHARACTER STATION                           
OSTA3    MVC   5(1,R3),4(R2)      MOVE IN MEDIA                                 
OSTAX    B     EXIT                                                             
*                                                                               
*                                                                               
ICBLSYS  MVC   0(24,R2),CBLSYS    CABLE SYSTEM NAME                             
         B     EXIT                                                             
*                                                                               
OCBLSYS  CLI   ZSTA,1                                                           
         BE    EXIT                                                             
         MVC   0(24,R3),0(R2)                                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                 SUBORD FOR (RECAP ONLY)                       
ISUBDP   CLI   READRC,C'D'                                                      
         BE    ISUBDP5                                                          
         MVC   APBYTE,BDPT       GOAL -DAYPART                                  
         B     ISUBDP6                                                          
ISUBDP5  MVC   APBYTE,BWDDPT     DAYPART                                        
         CLI   BWDSUBDP,0                                                       
         BE    ISUBDP6                                                          
         MVC   APBYTE,BWDSUBDP                                                  
ISUBDP6  XC    APPARM,APPARM                                                    
         MVC   APPARM(2),CUAALF                                                 
         MVC   APPARM+2(1),QMED                                                 
         MVC   APPARM+3(1),ESTDMENU                                             
*                                                                               
         CLC   APPARM(4),APELEM    SAME DAYPART MENU?                           
         BE    ISUBDP7                                                          
         XC    APELEM,APELEM                                                    
         MVC   APELEM(4),APPARM    FIRST 4 BYTES ARE USE TO SAVE READS          
*                                                                               
         GOTO1 VDPTRD,APPARM,,APELEM+16,VDMGR                                   
         CLI   APPARM+8,X'FF'                                                   
         BE    EXIT                                                             
ISUBDP7  LA    RE,APELEM+16                                                     
ISUBDP8  CLI   0(RE),0                                                          
         BE    EXIT                                                             
         CLC   0(1,RE),APBYTE     MATCH ON DPT?                                 
         BE    ISUBDP9                                                          
         LA    RE,5(RE)           NO - TRY NEXT ONE IN MENU                     
         B     ISUBDP8                                                          
ISUBDP9  ZIC   R1,1(RE)           YES -                                         
         SRL   R1,4               GET MASTER NUMBER INDICATOR                   
         STC   R1,0(R2)           STORE IT FOR PROPER DISPLAY OF DPTS           
         ZIC   R1,1(RE)           THEN GET SUBS SEQUENCE NUMBER                 
         N     R1,=X'0000000F'    GET RID OF HIGHORDER NIBBLE                   
         STC   R1,1(R2)           STORE IT FOR PROPER DISPLAY OF DPTS           
         B     EXIT                                                             
*                                 SUBORD FOR REPORT                             
*                                                                               
ISUB1DP  CLI   READRC,C'D'                                                      
         BE    ISUB1DP5                                                         
         MVC   APBYTE,BDPT       GOAL -DAYPART                                  
         B     ISUB1DP8                                                         
ISUB1DP5 MVC   APBYTE,BWDDPT     DAYPART                                        
         CLI   BWDSUBDP,0                                                       
         BE    ISUB1DP8                                                         
         MVC   APBYTE,BWDSUBDP                                                  
ISUB1DP8 XC    APPARM,APPARM                                                    
         MVC   APPARM(2),CUAALF                                                 
         MVC   APPARM+2(1),QMED                                                 
         MVC   APPARM+3(1),ESTDMENU                                             
         GOTO1 VDPTRD,APPARM,,APWORK,VDMGR                                      
         CLI   APPARM+8,X'FF'                                                   
         BE    EXIT                                                             
         LA    RE,APWORK                                                        
ISUB1DP7 CLI   0(RE),0                                                          
         BE    EXIT                                                             
         CLC   0(1,RE),APBYTE     MATCH ON DPT?                                 
         BE    ISUB1DP9                                                         
         LA    RE,5(RE)           NO - TRY NEXT ONE IN MENU                     
         B     ISUB1DP7                                                         
ISUB1DP9 ZIC   R1,1(RE)           YES -                                         
         SRL   R1,4               GET MASTER NUMBER INDICATOR                   
         STC   R1,0(R2)           STORE IT FOR PROPER DISPLAY OF DPTS           
         OI    0(R2),X'10'        TO MAKE 00 NOT ZERO (FOR DROOL BRK)           
         B     EXIT                                                             
*                                                                               
OSUB1DP  MVC   APBYTE,0(R2)       SAVE MASTER DPT NUMBER                        
         NI    APBYTE,X'EF'       REMOVE X'10'                                  
         TM    GLINDS,GLTOTLIN                                                  
         BNO   OSUB1DP4                                                         
         CLI   APBYTE,0           IF ZERO AND TOTAL LINE                        
         BNE   OSUB1DP4                                                         
         MVI   DONTPRT,C'Y'       DON'T PRINT-BECAUSE NOT REAL TOTAL            
         B     EXIT                                                             
OSUB1DP4 XC    APPARM,APPARM                                                    
         MVC   APPARM(2),CUAALF                                                 
         MVC   APPARM+2(1),QMED                                                 
         MVC   APPARM+3(1),ESTDMENU                                             
         GOTO1 VDPTRD,APPARM,,APWORK,VDMGR                                      
         CLI   APPARM+8,X'FF'                                                   
         BE    EXIT                                                             
         LA    RE,APWORK                                                        
OSUB1DP7 CLI   0(RE),0                                                          
         BE    EXIT                                                             
         ZIC   R1,1(RE)                                                         
         SRL   R1,4               GET MASTER NUMBER INDICATOR                   
         STC   R1,APFULL                                                        
         CLC   APFULL(1),APBYTE                                                 
         BE    OSUB1DP9                                                         
         LA    RE,5(RE)           NO - TRY NEXT ONE IN MENU                     
         B     OSUB1DP7                                                         
OSUB1DP9 MVC   MQDPT,2(RE)        SAVE 3CHAR MASTER DPT                         
         B     EXIT                                                             
*                                                                               
*                                                                               
ISUB2DP  CLI   READRC,C'D'                                                      
         BE    ISUB2DP5                                                         
         MVC   APBYTE,BDPT       GOAL -DAYPART                                  
         B     ISUB2DP8                                                         
ISUB2DP5 MVC   APBYTE,BWDDPT     DAYPART                                        
         CLI   BWDSUBDP,0                                                       
         BE    ISUB2DP8                                                         
         MVC   APBYTE,BWDSUBDP                                                  
ISUB2DP8 XC    APPARM,APPARM                                                    
         MVC   APPARM(2),CUAALF                                                 
         MVC   APPARM+2(1),QMED                                                 
         MVC   APPARM+3(1),ESTDMENU                                             
         GOTO1 VDPTRD,APPARM,,APWORK,VDMGR                                      
         CLI   APPARM+8,X'FF'                                                   
         BE    EXIT                                                             
         LA    RE,APWORK                                                        
ISUB2DP7 CLI   0(RE),0                                                          
         BE    EXIT                                                             
         CLC   0(1,RE),APBYTE     MATCH ON DPT?                                 
         BE    ISUB2DP9                                                         
         LA    RE,5(RE)           NO - TRY NEXT ONE IN MENU                     
         B     ISUB2DP7                                                         
ISUB2DP9 ZIC   R1,1(RE)           YES -                                         
         N     R1,=X'0000000F'    GET RID OF HIGHORDER NIBBLE                   
         STC   R1,0(R2)           STORE IT FOR PROPER DISPLAY OF DPTS           
         B     EXIT                                                             
         EJECT                                                                  
IDPTLN   CLI   READRC,C'D'                                                      
         BNE   IDPTLN5                                                          
         MVC   0(1,R2),BWDDPT     DAYPART-LENGTH INPUT                          
         MVC   1(1,R2),BWDSLN                                                   
         B     EXIT                                                             
IDPTLN5  MVC   0(1,R2),BDPT       GOAL -DAYPART                                 
         MVC   1(1,R2),BSLN             SLN                                     
         B     EXIT                                                             
*                                                                               
*                                                                               
ODPTLN   GOTO1 AGETDPT,0(R2)      DAYPART-LENGTH OUTPUT -GET 3 CHAR DPT         
         MVC   0(3,R3),QDPT       FROM ONE BYTE                                 
         MVI   3(R3),C'-'                                                       
*                                                                               
         CLC   INOFRM,=C'PT'       ARE WE IN F=DPT?                             
         BNE   ODPTLN5                                                          
         CLI   1(R2),X'FF'         TOTAL FOR THAT DAYPART?                      
         BNE   ODPTLN5                                                          
         MVC   0(3,R3),QDPT                                                     
         MVC   3(4,R3),=C'-TOT'    YES, DISPLAY IT AS DPT-TOT                   
         B     EXIT                                                             
ODPTLN5  EDIT  (1,1(R2)),(3,4(R3)),DUB=APDUB,WRK=APWORK,ALIGN=LEFT              
         B     EXIT                                                             
*                                                                               
*                                                                               
OWDPTLN  GOTO1 AGETDPT,0(R2)      GET 3 CHAR DPT                                
         MVC   0(3,R3),QDPT       FROM ONE BYTE                                 
         EDIT  (1,1(R2)),(3,3(R3)),DUB=APDUB,WRK=APWORK,ALIGN=LEFT              
         B     EXIT                                                             
*                                                                               
*                                                                               
MDPT     CLI   READRC,C'D'        MASTER DAYPART                                
         BNE   MDPT5                                                            
         MVC   0(1,R2),BWDDPT     DAYPART                                       
         B     EXIT                                                             
MDPT5    MVC   0(1,R2),MASTDPT    0 IF SINGLE DPT                               
         B     EXIT                                                             
*                                                                               
*                                                                               
MDPTO    TM    GLINDS,GLTOTLIN                                                  
         BNO   EXIT                                                             
         MVC   APFULL,QDPT        SAVE CURRENT DAYPART                          
         GOTO1 AGETDPT,0(R2)                                                    
         MVC   MQDPT,QDPT                                                       
         MVC   QDPT,APFULL        RESTORE CURRENT DAYPART                       
         B     EXIT                                                             
*                                                                               
*                                                                               
SDPT     CLI   READRC,C'D'        SUB DAYPART                                   
         BNE   SDPT5                                                            
         MVI   0(R2),C'Z'         HAVE MASTER SINK TO BOTTOM                    
         CLI   BWDSUBDP,0         ANY SUBDAYPARTS                               
         BE    EXIT                                                             
         MVC   0(1,R2),BWDSUBDP                                                 
         B     EXIT                                                             
SDPT5    MVI   0(R2),C'Z'         HAVE MASTER SINK TO BOTTOM                    
         CLC   MASTDPT,BDPT       TEMPORARY                                     
         BE    EXIT               TEMPORARY                                     
         MVC   0(1,R2),BDPT                                                     
         B     EXIT                                                             
*                                                                               
*                                                                               
ISDPTLN  CLI   READRC,C'D'                                                      
         BNE   ISDPTLN5                                                         
         MVC   0(1,R2),BWDDPT     DAYPART                                       
         CLI   BWDSUBDP,0         ANY SUBDAYPARTS                               
         BE    *+10                                                             
         MVC   0(1,R2),BWDSUBDP                                                 
         MVC   1(1,R2),BWDSLN                                                   
         B     EXIT                                                             
ISDPTLN5 MVC   0(1,R2),BDPT                                                     
         MVC   1(1,R2),BSLN                                                     
         B     EXIT                                                             
*                                                                               
*                                                                               
OSDPTLN  XC    BDPT,BDPT          DAYPART/LEN OUTPUT - FORCE VALUES             
         GOTO1 AGETDPT,0(R2)      TO BE FILLED GET 3 CHAR DPT                   
         MVC   0(3,R3),QDPT       FROM ONE BYTE                                 
         MVI   3(R3),C'-'                                                       
         EDIT  (1,1(R2)),(3,4(R3)),DUB=APDUB,WRK=APWORK,ALIGN=LEFT              
         B     EXIT                                                             
*                                                                               
*                                                                               
IDPT     CLI   READRC,C'D'                                                      
         BNE   IDPT5                                                            
         TM    BWDINDS,BWDIPKG                                                  
         BO    IDPT2                                                            
         TM    BWDINDS,BWDIORB                                                  
         BZ    IDPT4              NOT PACKAGE/ORBIT USE BWDDPT                  
IDPT2    CLI   BWDKELSQ,0         ARE WE MASTER?                                
         BE    IDPT3              YES - SAVE DAYPART FOR SLAVES                 
         MVC   0(1,R2),SVMDPT     SLAVE USE MASTER DAYPART                      
         B     EXIT                                                             
IDPT3    MVC   SVMDPT,BWDDPT      SAVE MASTER DAYPART                           
IDPT4    MVC   0(1,R2),BWDDPT     DAYPART                                       
         B     EXIT                                                             
IDPT5    MVC   0(1,R2),BDPT       DAYPART FOR GOALS                             
         B     EXIT                                                             
*                                                                               
*                                                                               
ILEN     MVC   0(1,R2),BWDSLN     SPOT LENGTH                                   
         B     EXIT                                                             
*                                                                               
*                                                                               
IWEEK    BRAS  RE,INPTWEEK                                                      
         B     EXIT                                                             
*                                                                               
OWEEK    BRAS  RE,OUPTWEEK                                                      
         B     EXIT                                                             
*                                                                               
* -- IWORK - HEADER FOR MAIN REPORT                                             
*                                                                               
IWORK    CLI   READRC,C'D'                                                      
         BNE   EXIT                                                             
         USING HEADRD,R2                                                        
         CLI   GLARGS+1,C'Y'      DAYPART INCLUDED IN WORK?                     
         BNE   IWORK2                                                           
         BRAS  RE,SETWORK                                                       
         B     IWORKX                                                           
*                                                                               
IWORK2   MVI   HDRFSEQ,0          FOR PRINTING SLAVES IN CORRECT ORDER          
         MVC   HDRFSEQ,BWDKELPO                                                 
         MVC   HDRSTA,BWDSTA      MOVE STATION                                  
         MVI   HDRFDAY,0          SET PROPER SORTING SEQUENCE FOR DAYS          
         CLI   BWDDAYS,X'7C'       M-F                                          
         BE    IWORK4                                                           
         MVI   HDRFDAY,1                                                        
         CLI   BWDDAYS,X'7F'       M-SU                                         
         BE    IWORK4                                                           
         XR    RF,RF               START MONDAY AS 2                            
         ZIC   R0,BWDDAYS                                                       
         SRDL  R0,9                                                             
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   RF,*-10                                                          
         LPR   RF,RF                                                            
         STC   RF,HDRFDAY                                                       
*                                                                               
IWORK4   MVC   HDRTIM,BWDKELTM    PACKED TIMES                                  
         MVC   HDRSEQ1,BWDKELSQ   SEQ #                                         
         CLI   COSTIND,2          MOVE IN APPROPRIATE COST                      
         BNE   IWORK3                                                           
         MVC   HDRCOST,BWDCOST2   COST2                                         
         MVI   HDRCIND,2                                                        
         B     IWORK10                                                          
IWORK3   CLI   COSTIND,3                                                        
         BNE   IWORK7                                                           
         MVC   HDRCOST,BWDCOST3   COST3                                         
         MVI   HDRCIND,3                                                        
         B     IWORK10                                                          
IWORK7   MVC   HDRCOST,BWDCOST1  SO MUST BE COST1                               
         MVI   HDRCIND,1                                                        
*                               -----INFO FOR REREADING RECORD----              
IWORK10  MVC   HDRDAY,BWDKELDY    DAYS                                          
         MVC   HDRSEQ2,BWDKSEQ    SEQUENCE NUMBER                               
         MVC   HDRSTCD,BWDKELST   STATION CODE                                  
         MVC   HDRPO,BWDKELPO     PACKAGE/ORBIT #                               
*                               -----CHECK STATION INCLUDED?------              
         CLI   GLARGS,C'N'        INCLUDE STATION HERE                          
         BNE   IWORK12            YES                                           
         MVC   TEMPBLK(1),HDRFSEQ    MOVE SEQUENCE NUMBER                       
         MVC   TEMPBLK+1(14),HDRFDAY MOVE THE REST(EXCLUDING STA)               
         XC    0(L'HDRLN,R2),0(R2)                                              
         MVC   0(L'NHDRLN,R2),TEMPBLK                                           
         B     IWORKX                                                           
*                                                                               
IWORK12  TM    SELKIND2,SELKRDT    ARE WE DAYPART                               
         BZ    IWORKX                                                           
         XC    APWORK,APWORK                                                    
         MVC   APDUB(11),HDRSTA   SAVE STA,DAY,TIM                              
         MVC   DTHDRSTA,APDUB     SWITCH THEM AROUND INORDER                    
         MVC   DTHDRFDY,APDUB+8   TO GET DAY/TIM ACROSS                         
         MVC   DTHDRTIM,APDUB+9   STATION SEQUENCE                              
IWORKX   B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* OWORK - OUTPUT HEADER STUFF FOR MAIN REPORT                                   
         USING HEADRD,R2                                                        
OWORK    DS    0H                                                               
         BAS   RE,RERDTL          RE-READ RECORD                                
         CLI   GLARGS,C'N'        N=STATION NOT INCLUDED                        
         BNE   OWORKA                                                           
         ZIC   RF,NOPTCIND                                                      
         CLI   GLARGS+1,C'Y'      N=STATION NOT INCLUDED                        
         BE    OWORKAX                                                          
         ZIC   RF,NHDRCIND                                                      
         B     OWORKAX                                                          
OWORKA   ZIC   RF,OPTCIND                                                       
         CLI   GLARGS+1,C'Y'                                                    
         BE    OWORKAX                                                          
         ZIC   RF,HDRCIND                                                       
OWORKAX  STC   RF,COSTIND         COSTIND RE-SET                                
         BAS   RE,GETSPTS         RE-SET SPOTTABLE                              
         L     R9,AIOAREA1                                                      
****  WE'RE CHECKING OUT BCAM IF ALL CAMPAIGNS                                  
         TM    SELKIND,SELKCAM                                                  
         BNZ   OWORKAZ                                                          
         MVC   SVIOKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(6),0(R9)      GONNA GET NEW BCAM                           
         MVI   IOKEY+1,X'E7'       USING PASSIVE HEADER KEY                     
         L     R0,AIOAREA3                                                      
         LHI   R1,6000                                                          
         L     RE,AIOAREA4                                                      
         LR    RF,R1                                                            
         MVCL  RE,R0               SAVE OFF AIOAREA3 STUFF                      
*                                                                               
         GOTO1 AIO,DIRHI+IO3                                                    
         CLC   IOKEY(6),IOKEYSAV                                                
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE                                  
*                                                                               
         GOTO1 AIO,FILGET3         GET IT                                       
         L     RF,AIOAREA3                                                      
         GOTO1 AGETCAM,4(RF)                                                    
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'SVIOKEY),SVIOKEY                                         
*                                                                               
         L     R0,AIOAREA4                                                      
         LHI   R1,6000                                                          
         L     RE,AIOAREA3                                                      
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE AIOAREA3 STUFF                       
***  BCAM SHOULD BE SET CORRECTLY NOW                                           
****  WE'RE CHECKING OUT BCAM IF ALL CAMPAIGNS                                  
OWORKAZ  MVC   0(4,R3),SPACES     --- INFO FOR LINE ONE ----                    
         TM    BWDINDS,BWDIPKG    PACKAGE?                                      
         BO    OWORKB             YES                                           
         TM    BWDINDS,BWDIORB    ORBIT?                                        
         BZ    OWORK1             NO                                            
         MVC   0(3,R3),=C'ORB'                                                  
         B     OWORKC                                                           
OWORKB   MVC   0(3,R3),=C'PKG'                                                  
OWORKC   EDIT  BWDPKOR,(1,3(R3)),DUB=APDUB,WRK=APWORK                           
         B     OWORK3A                                                          
*                                                                               
OWORK1   MVC   0(L'BWDSTA,R3),BWDSTA   STATION                                  
         CLI   0(R3),C'0'         IF CABLE STATION                              
         BL    *+12                                                             
         MVI   4(R3),C'/'          PRINT XXXX/XXX                               
         B     OWORK3A                                                          
         CLI   3(R3),X'00'                                                      
         BNE   OWORK2                                                           
         MVI   3(R3),C'-'         3 CHARACTER STATION                           
         B     OWORK3                                                           
OWORK2   MVI   4(R3),C'-'         4 CHARACTER STATION                           
OWORK3   MVC   5(1,R3),BWDSTA+4   MOVE IN MEDIA                                 
OWORK3A  TM    GLINDS,GLTOTLIN                                                  
         BO    EXIT                                                             
         TM    BWDINDS,BWDIPKG    PACKAGE?                                      
         BO    OWORK3C                                                          
         TM    BWDINDS,BWDIORB    ORBIT?                                        
         BZ    OWORK3X            NO                                            
OWORK3C  BAS   RE,DOPO            GO HANDLE PACKAGE/ORBITS                      
         B     OWORK8                                                           
*                                                                               
OWORK3X  BRAS  RE,DATELIM         BUY PERIOD                                    
         OC    SPER,SPER                                                        
         BZ    OWORK4                                                           
         GOTO1 VDATCON,APPARM,(2,SPER),(4,9(R3)) START DATE                     
         MVI   14(R3),C'-'                                                      
OWORK4   OC    EPER,EPER                                                        
         BZ    OWORK6                                                           
         GOTO1 VDATCON,APPARM,(2,EPER),(4,15(R3)) END DATE                      
OWORK6   SR    R6,R6                                                            
         CLI   ESTOWSDY,0                                                       
         BE    OWORK7                                                           
         IC    R6,ESTOWSDY        GIVE DAYUNPK START DAY OF WEEK                
         SLL   R6,4               FOR OUT OF WEEK ROTATORS                      
OWORK7   GOTO1 VDAYUNPK,APPARM,((R6),BWDKELDY),21(R3)  DAY                      
         GOTO1 VUNTIME,APPARM,BWDTIMES,29(R3)      TIME                         
OWORK8   MVC   39(1,R3),BWDDPT                     DAYPART                      
         EDIT  BWDSLN,(3,40(R3)),DUB=APDUB,WRK=APWORK                           
*                                 --- INFO FOR LINE TWO ---                     
         CLI   ASONOFF,ASOFF                                                    
         BE    *+12                                                             
         LA    R3,DRLWIDTH(R3)    ONLINE - DROOL                                
         B     OWORK9                                                           
         LA    R3,DRVWIDTH(R3)    OFFLINE - DRIVER                              
OWORK9   MVC   0(L'BWDSTA,R3),SPACES                                            
         TM    SELKIND2,SELKRST   STATION SEQUENCE                              
         BO    OWORK10                                                          
         TM    SELKIND2,SELKRDT   RANK BY DY/TM ACROSS STATIONS                 
         BO    OWORK10                                                          
         L     R1,ADALPHA         ADDRESS OF CURRENT POS IN TABLE               
         MVC   0(2,R3),0(R1)      SEQUENCE NUMBER                               
         CLC   0(2,R3),=C'**'                                                   
         BE    OWORK10                                                          
         LA    R1,2(R1)           INCREMENT ALPHA TABLE                         
         ST    R1,ADALPHA                                                       
*                                                                               
OWORK10  MVC   9(17,R3),BWDPROG   PROGRAMMING                                   
         CLI   SUPCST,C'Y'        SUPPRESS COST?                                
         BE    OWORK15            YES - SKIP THIS SECTION                       
         CLI   GLARGS,C'N'        N=STATION NOT INCLUDED                        
         BNE   OWORK11                                                          
         ICM   RF,15,NOPTCOST                                                   
         CLI   GLARGS+1,C'Y'      N=STATION NOT INCLUDED                        
         BE    OWORK13                                                          
         ICM   RF,15,NHDRCOST                                                   
         B     OWORK13                                                          
OWORK11  ICM   RF,15,OPTCOST                                                    
         CLI   GLARGS+1,C'Y'                                                    
         BE    OWORK13                                                          
         ICM   RF,15,HDRCOST                                                    
OWORK13  EDIT  (RF),(13,26(R3)),2,DUB=APDUB,WRK=APWORK,FLOAT=$                  
OWORK15  BRAS  RE,SPFREQ          GETS MOST FREQUENT SPOT                       
         EDIT  APBYTE,(3,40(R3)),DUB=APDUB,WRK=APWORK                           
*                                                                               
         CLI   ASONOFF,ASOFF                                                    
         BE    *+12                                                             
         LA    R3,DRLWIDTH(R3)    POINT TO PRINT LINE 3                         
         B     OWORK20                                                          
         LA    R3,DRVWIDTH(R3)                                                  
OWORK20  MVC   0(117,R3),SPACES                                                 
         TM    OPTSW,COMMLIN      COMMENTS?                                     
         BZ    OWORKX                                                           
         BRAS  RE,RDCOM                                                         
OWORKX   B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* HANDLES SET UP FOR ORBIT/PACKAGES                                             
DOPO     NTR1                                                                   
         TM    BWDINDS,BWDIPKG                                                  
         BZ    DOPO50                                                           
         CLI   BWDKELSQ,0         PACK MASTER HAS NO BUY PER,DAYS,TIMES         
         BNE   DOPO10                                                           
         MVC   21(20,R3),=20C'*'                                                
         B     EXIT                                                             
*                                                                               
DOPO10   DS    0H                 PACKAGE SLAVE HAS BUY PER,DAYS,&TIMES         
         BRAS  RE,DATELIM         BUY PERIOD                                    
         OC    SPER,SPER                                                        
         BZ    DOPO12                                                           
         GOTO1 VDATCON,APPARM,(2,SPER),(4,8(R3)) START DATE                     
         MVI   13(R3),C'-'                                                      
DOPO12   OC    EPER,EPER                                                        
         BZ    DOPO15                                                           
         GOTO1 VDATCON,APPARM,(2,EPER),(4,14(R3)) END DATE                      
DOPO15   SR    R6,R6                                                            
         CLI   ESTOWSDY,0                                                       
         BE    DOPO25                                                           
         IC    R6,ESTOWSDY        GIVE DAYUNPK START DAY OF WEEK                
         SLL   R6,4               FOR OUT OF WEEK ROTATORS                      
DOPO25   GOTO1 VDAYUNPK,APPARM,((R6),BWDPODAY),20(R3)   DAY                     
         GOTO1 VUNTIME,APPARM,BWDTIMES,28(R3)      TIME                         
         B     EXIT                                                             
*                                                                               
DOPO50   CLI   BWDKELSQ,0         MUST BE AN ORBIT                              
         BNE   DOPO70                                                           
         BRAS  RE,DATELIM         ORB MAST HAS BUY PER,NO DAY/TIME              
         OC    SPER,SPER                                                        
         BZ    DOPO55                                                           
         GOTO1 VDATCON,APPARM,(2,SPER),(4,8(R3)) START DATE                     
         MVI   13(R3),C'-'                                                      
DOPO55   OC    EPER,EPER                                                        
         BZ    EXIT                                                             
         GOTO1 VDATCON,APPARM,(2,EPER),(4,14(R3)) END DATE                      
         MVC   21(20,R3),=20C'*'                                                
         B     EXIT                                                             
*                                                                               
DOPO70   SR    R6,R6              ORBIT SLAVE HAS DAY/TIME ONLY                 
         CLI   ESTOWSDY,0                                                       
         BE    DOPO80                                                           
         IC    R6,ESTOWSDY        GIVE DAYUNPK START DAY OF WEEK                
         SLL   R6,4               FOR OUT OF WEEK ROTATORS                      
DOPO80   GOTO1 VDAYUNPK,APPARM,((R6),BWDPODAY),20(R3)  DAY                      
         GOTO1 VUNTIME,APPARM,BWDTIMES,28(R3)      TIME                         
         B     EXIT                                                             
         EJECT                                                                  
* RE-READS DETAIL RECORD                                                        
*                                                                               
RERDTL   NTR1                                                                   
         USING HEADRD,R2                                                        
         LA    R9,IOKEY                                                         
         XC    BWDKEY(13),BWDKEY                                                
         MVI   BWDKTYP,BWDKTYPQ                                                 
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         CLI   GLARGS+1,C'Y'      OPTION DAYPART?                               
         BE    RERDTL10                                                         
         CLI   GLARGS,C'N'                                                      
         BNE   RERDTL5                                                          
         MVC   BWDKSEQ,NHDRSEQ2    SEQUENCE NUMBER                              
         MVI   BWDKELCD,1                                                       
         MVC   BWDKELST,NHDRSTCD                                                
         MVC   BWDKELPO,NHDRPO                                                  
         MVC   BWDKELDY,NHDRDAY                                                 
         MVC   BWDKELTM,NHDRTIM                                                 
         MVC   BWDKELSQ,NHDRSEQ1                                                
         B     RERDTL8                                                          
*                                                                               
RERDTL5  MVC   BWDKSEQ,HDRSEQ2    SEQUENCE NUMBER                               
         MVI   BWDKELCD,1                                                       
         MVC   BWDKELST,HDRSTCD                                                 
         MVC   BWDKELPO,HDRPO                                                   
         MVC   BWDKELSQ,HDRSEQ1                                                 
         MVC   BWDKELDY,HDRDAY                                                  
         TM    SELKIND2,SELKRDT    DAYPART ?                                    
         BZ    RERDTL6                                                          
         MVC   BWDKELTM,DTHDRTIM                                                
         B     RERDTL8                                                          
*                                                                               
RERDTL6  MVC   BWDKELTM,HDRTIM                                                  
RERDTL8  DS    0H                                                               
         B     RERDTL20           GO READ THE RECORD                            
*----------OPTION DAYPART ---------------------------                           
*                                                                               
RERDTL10 CLI   GLARGS,C'N'                                                      
         BNE   RERDTL15                                                         
         MVC   BWDKSEQ,NOPTSEQ2    SEQUENCE NUMBER                              
         MVI   BWDKELCD,1                                                       
         MVC   BWDKELST,NOPTSTCD                                                
         MVC   BWDKELPO,NOPTPO                                                  
         MVC   BWDKELDY,NOPTDAY                                                 
         MVC   BWDKELTM,NOPTTIM                                                 
         MVC   BWDKELSQ,NOPTSEQ1                                                
         B     RERDTL20                                                         
RERDTL15 MVC   BWDKSEQ,OPTSEQ2    SEQUENCE NUMBER                               
         MVI   BWDKELCD,1                                                       
         MVC   BWDKELST,OPTSTCD                                                 
         MVC   BWDKELPO,OPTPO                                                   
         MVC   BWDKELSQ,OPTSEQ1                                                 
         MVC   BWDKELDY,OPTDAY                                                  
         MVC   BWDKELTM,OPTTIM                                                  
*                                                                               
*----------READ THE RECORD------------                                          
RERDTL20 GOTO1 AMIN,MINRD1                                                      
         BE    EXIT                                                             
         MVI   ERRNUM,15                                                        
         J     CLDUMP                                                           
         EJECT                                                                  
*======================COLUMNS================================*                 
         SPACE 2                                                                
ISPTWK   CLI   READRC,C'D'        SPOTS PER WEEK INPUT                          
         BNE   EXIT                                                             
         TM    BWDINDS,BWDIORB    ORBIT RECORD?                                 
         BZ    ISPT1              NO                                            
         CLI   BWDKELSQ,0         WE ARE ORBIT RECORD - IF WE ARE               
         BE    ISPT3              MASTER - GET SPOTS PER WEEK                   
         B     ISPT2              BUT SLAVES HAVE NO SPOTS                      
*                                                                               
ISPT1    TM    BWDINDS,BWDIPKG    PACKAGE RECORD?                               
         BZ    ISPT3              NO - MUST BE WORK RECORD - GET SPOTS          
         CLI   BWDKELSQ,0         WE ARE PACKAGE RECORD - IF WE ARE             
         BNE   ISPT3              SLAVE - GET SPOTS PER WEEK                    
ISPT2    XC    SPOTABLE,SPOTABLE  MASTERS HAVE NO SPOTS                         
         B     *+8                                                              
ISPT3    BAS   RE,GETSPTS                                                       
         MVC   0(53,R2),SPOTABLE                                                
*                                                                               
* GDTLCST ON EXIT = TOTAL # OF TELECASTS FOR DETAIL                             
* LINE (THIS WILL BE USED IN GOAL ROUTINES (ON INPUT)                           
         LA    R0,53                                                            
         XR    RF,RF                                                            
         LA    RE,SPOTABLE                                                      
         SR    R1,R1                                                            
ISPT5    IC    R1,0(RE)                                                         
         AR    RF,R1                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,ISPT5                                                         
         STH   RF,GDTLCST                                                       
         B     EXIT                                                             
*                                                                               
OSPTWK   ZIC   R8,CMPNWKS         SPOTS/WEEK OUTPUT (CAN ONLY SHOW 14)          
         CLI   GLHOOK,GLINCOMP                                                  
         BE    EXIT                                                             
         LR    R6,R2              SAVE ENTRY START                              
*                                                                               
         ZIC   R0,INOSTDTD                                                      
         AR    R2,R0                                                            
         SR    R8,R0                                                            
*                                                                               
         CHI   R8,14                                                            
         BNH   OSPTWK5                                                          
         LA    R8,14                                                            
*                                                                               
OSPTWK5  CLI   0(R2),0                                                          
         BE    OSPTWK20                                                         
         EDIT  (1,0(R2)),(4,0(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                 
OSPTWK20 LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R8,OSPTWK5                                                       
*                                                                               
         LA    R8,53                                                            
         LR    R2,R6              RESTORE ENTRY START                           
         LH    R6,DPTNUM                                                        
         LH    RE,STANUM                                                        
         LH    R0,MKTNUM                                                        
         SR    RF,RF                                                            
*                                                                               
OSPTWK30 SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    RF,R1              ADD SPOTS TO THIS LINES CSTS                  
         AR    RE,R1              ADD SPOTS TO STATION TOTAL CSTS               
         AR    R0,R1              ADD SPOTS TO MKT TOTAL CSTS                   
         AR    R6,R1              ADD SPOTS TO DAYPART TOTAL CSTS               
         LA    R2,1(R2)                                                         
         BCT   R8,OSPTWK30                                                      
*                                                                               
         STH   RF,DTLCST          DTLCST=GDTLCST BUT ON OUTPUT                  
         TM    GLINDS,GLTOTLIN    IF TOTAL LINE                                 
         BO    EXIT                                                             
         STH   R6,DPTNUM                                                        
         STH   RE,STANUM                                                        
         STH   R0,MKTNUM                                                        
         B     EXIT                                                             
*                                                                               
*                                                                               
IOVDEMO  LA    RE,0(R2)                                                         
         LA    R0,6                                                             
         LA    RF,LDEMHLD+2       POINT TO RATINGS                              
IOVDEMO5 MVC   0(1,RE),0(RF)      MOVE OVERRIDE IF ANY                          
         LA    RE,1(RE)                                                         
         LA    RF,6(RF)                                                         
         BCT   R0,IOVDEMO5                                                      
         OC    0(6,R2),0(R2)                                                    
         BNZ   *+8                                                              
         MVI   0(R2),X'01'                                                      
         B     EXIT                                                             
*                                                                               
OOVDEMO  LA    RE,OVERS                                                         
         MVC   0(6,RE),0(R2)      MOVE THEM INTO OVERS                          
         ST    RE,ASOVERS                                                       
         B     EXIT                                                             
*                                                                               
*                                                                               
* DEMO FIELD - TWO FIELDS COST(4),RATING(4) USED FOR CPP USE ONLY               
* ** IF MASTER/OR NON-PACKAGE RECORD FIRST FOUR FIELDS WILL BE                  
* IDENTICAL  ** IF SLAVE FIRST TWO WILL REFLECT MASTER VALUES                   
* SECOND TWO FIELDS WILL CONTAIN SLAVE VALUES                                   
*                                                                               
* THE LAST FIVE FIELDS ARE JUST RATINGS                                         
*                                                                               
IDEMO    CLI   READRC,C'D'        DEMO INPUT                                    
         BNE   EXIT                                                             
         LA    R8,DEMNUM                                                        
         LA    R1,LDEMHLD+2       POINT TO RATINGS                              
         MVC   0(4,R2),BWDCOST1                                                 
         MVC   8(4,R2),BWDCOST1                                                 
         CLI   COSTIND,2          COST2                                         
         BNE   IDEMO5                                                           
         MVC   0(4,R2),BWDCOST2                                                 
         MVC   8(4,R2),BWDCOST2                                                 
         B     IDEMO10                                                          
IDEMO5   CLI   COSTIND,3          COST3                                         
         BNE   IDEMO10                                                          
         MVC   0(4,R2),BWDCOST3                                                 
         MVC   8(4,R2),BWDCOST3                                                 
IDEMO10  MVC   5(3,R2),1(R1)      RATING TIME                                   
         MVC   13(3,R2),1(R1)                                                   
         TM    BWDINDS,BWDIORB                                                  
         BO    IDEMO12                                                          
         TM    BWDINDS,BWDIPKG    PACKAGE/ORBIT RECORD                          
         BZ    IDEMO20            NO--  NORMAL RECORD                           
*                                 YES-- TEST IF MASTER OR SLAVE                 
IDEMO12  CLI   BWDKELSQ,0         0=MASTER                                      
         BNE   IDEMO15                                                          
         MVC   MASTCOST,0(R2)     SAVE COST AND RATE FOR SLAVE                  
         MVC   MASTRAT(3),5(R2)   RATING FIRST 3                                
         B     IDEMO20                                                          
*                                                                               
IDEMO15  MVC   0(4,R2),MASTCOST   <> 0 = SLAVE                                  
         MVC   5(3,R2),MASTRAT    MASTER VALUES FIRST                           
IDEMO20  MVC   SVFRTG,13(R2)      SAVE RTG FOR GOALS                            
IDEMO22  LA    R2,16(R2)           POINT TO JUST RATINGS                        
         BCTR  R8,0                                                             
IDEMO25  LA    R1,6(R1)                                                         
         MVC   1(3,R2),1(R1)      RATING                                        
         LA    R2,4(R2)           POINT TO NEXT RATING                          
         BCT   R8,IDEMO25                                                       
IDEMOX   B     EXIT                                                             
*                                                                               
ODEMO    TM    GLINDS,GLTOTLIN    DEMO OUTPUT                                   
         BO    EXIT                                                             
         CLI   GLHOOK,GLINCOMP    CALCULATE CPP , OR MOVE IN RATING             
         BNE   ODEMO10            NOT NOW                                       
         TM    SELKIND,SELKRDM    RANK BY DEMO RATING?                          
         BZ    ODEMO05            NOPE -                                        
         MVI   0(R2),0                                                          
         MVC   1(3,R2),5(R2)      YES --MOVE RATING INTO HOLE                   
         B     ODEMO08                                                          
*                                                                               
ODEMO05  MVC   TPCOST,0(R2)       COST                                          
         MVC   TPRTG,5(R2)        RATING                                        
***  2 DECIMAL                                                                  
         LA    RE,LDEMHLD         NEED THIS BECAUSE LDEMHLDR COMES LATE         
         ST    RE,ALDEMHLD                                                      
***  2 DECIMAL                                                                  
         BRAS  RE,LDEMHLDR         SETS UP MASTFLG FOR 2 DECIMAL BIT            
         MVC   APWORK(1),4(R2)     MOVE THE BIT-BYTE                            
         BRAS  RE,CALCPP           CALCPP NOW USES FIRST BYTE OF APWORK         
         MVC   4(1,R2),APWORK                                                   
         ICM   R1,15,APFULL                                                     
         BZ    ODEMO06            STORE NEGATIVE TO RANK                        
         LNR   RF,R1              CPP IN ASCENDING                              
         ST    RF,APFULL          ORDER                                         
ODEMO06  MVC   0(4,R2),APFULL     MOVE CPP INTO HOLE                            
ODEMO08  MVI   GLHOOK,GLIDID      TELL DRIVER YOU JUST DID THIS                 
         B     EXIT                                                             
*                                                                               
ODEMO10  LA    R8,DEMNUM          NUMBER OF DEMOS                               
         LR    R6,R3              PRINT LINE 1                                  
         CLI   ASONOFF,ASOFF                                                    
         BE    *+12                                                             
         LA    R6,DRLWIDTH(R6)    PRINT LINE 2 FOR ONLINE                       
         B     ODEMO11                                                          
         LA    R6,DRVWIDTH(R6)    PRINT LINE 2 FOR OFFLINE                      
ODEMO11  STCM  R6,15,SVP2         SAVE ADDRESS OF SECOND PRINT LINE             
         LA    R2,8(R2)           POINT TO SECOND GROUP (COST/RTG)              
         MVC   TPCOST,0(R2)       MOVE IN COST                                  
         LA    R2,4(R2)           POINT TO RATING                               
         MVC   TPRTG,1(R2)        MOVE IN RATING                                
*                                                                               
*----FOR STATION TOTAL LINE AND DAYPART TOTAL NEED TO ACCUMULATE                
*----FIRST COST X SPTS PER WK THEN RATINGSXSPTS PER WK FOR EACH DEM             
*                                                                               
         SR    RE,RE                                                            
         ICM   R0,15,STACSTS      STATION COST TOTAL                            
         ICM   RF,15,TPCOST       COST FOR DETAIL LINE                          
         BZ    ODEMO11X                                                         
         MH    RF,DTLCST                                                        
         AR    R0,RF              ADD TO TOTAL STATION COST                     
         STCM  R0,15,STACSTS                                                    
         SR    RE,RE                                                            
         ICM   R0,15,DPTCSTS      DAYPART COST TOTAL                            
         ICM   RF,15,TPCOST       COST FOR DETAIL LINE                          
         BZ    ODEMO12                                                          
         MH    RF,DTLCST                                                        
         AR    R0,RF              ADD TO TOTAL DAYPART COST                     
         STCM  R0,15,DPTCSTS                                                    
         SR    RE,RE                                                            
         ICM   R0,15,MKTCSTS      MARKET COST TOTAL                             
         ICM   RF,15,TPCOST       COST FOR DETAIL LINE                          
         BZ    ODEMO11X                                                         
         MH    RF,DTLCST                                                        
         AR    R0,RF              ADD TO TOTAL STATION COST                     
         STCM  R0,15,MKTCSTS                                                    
*                                                                               
ODEMO11X CLI   SUPDEM,C'Y'        SUPRESS DEMOS?                                
         BE    ODEMOX                                                           
ODEMO12  LA    RF,STADEMOS                                                      
         LA    RE,DPTDEMOS                                                      
         ST    RE,ADPTDEMO                                                      
***  2 DECIMAL                                                                  
         LA    RE,LDEMHLD                                                       
         ST    RE,ALDEMHLD                                                      
***  2 DECIMAL                                                                  
ODEMO13  CH    R8,=H'2'           LAST TWO DEMOS/CPP GO ON LINE 3/4             
         BNE   ODEMO14                                                          
         TM    OPTSW,SIXDEMS      SIX REQUESTED?                                
         BZ    ODEMOX             NOPE                                          
         ICM   R3,15,SVP2                                                       
         CLI   SUPCPPM,C'Y'       IF-SUPPRESSING CPP/CMP POINT R3 TO            
         BE    ODEMO14            SECOND PRINT LINE FOR RATINGS                 
*                                                                               
         CLI   ASONOFF,ASOFF                                                    
         BE    *+16                                                             
         LA    R3,DRLWIDTH(R3)    P3 FOR DROOL                                  
         LA    R6,DRLWIDTH(R3)    P4                                            
         B     ODEMO14                                                          
         LA    R3,DRVWIDTH(R3)    P3 FOR DRIVER                                 
         LA    R6,DRVWIDTH(R3)    P4                                            
*                                                                               
ODEMO14  SR    R1,R1                                                            
         ICM   R1,7,TPRTG                                                       
         MH    R1,DTLCST                                                        
         A     R1,0(RF)                                                         
         STCM  R1,15,0(RF)                                                      
         L     RE,ADPTDEMO        DAYPART TOTAL DEMOS                           
         SR    R1,R1                                                            
         ICM   R1,7,TPRTG                                                       
         MH    R1,DTLCST                                                        
         A     R1,0(RE)                                                         
         STCM  R1,15,0(RE)                                                      
         LA    RE,4(RE)                                                         
         ST    RE,ADPTDEMO                                                      
*                                                                               
         L     RE,ASOVERS                                                       
         MVC   OVRIDE,0(RE)                                                     
         LA    RE,1(RE)                                                         
         ST    RE,ASOVERS                                                       
*                                                                               
***************  APFULL IS SETUP TO BE DISPLAYED!!!                             
         XC    APFULL,APFULL                                                    
         MVC   APFULL+1(3),TPRTG   PRESERVING TPRTG IF 2 DECIMAL                
***************  APFULL IS SETUP TO BE DISPLAYED!!!                             
         SR    R1,R1              CHECK IF NUMBER >THAN SPACES AVAIL            
         ICM   R1,7,TPRTG                                                       
         C     R1,=F'99999'                                                     
         BH    ODEMO16            YES- DON'T DISPLAY DECIMAL                    
***  2 DECIMAL  ***                                                             
         BRAS  RE,LDEMHLDR         CHECK OUT THE DEMO CATEGORY                  
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    ODEMO14C             - NOPE, NOT OK                              
*                                                                               
ODEMO14A TM    OVRIDE,X'40'        2 DECIMAL?                                   
         BNZ   ODEMO14E             - YUP                                       
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    ODEMO14C             - NO, NORMAL                                
         OI    OVRIDE,X'40'         - YES, TURN ON BIT (THIS IS TOTALS)         
         B     ODEMO14E                                                         
***  2 DECIMAL  ***                                                             
ODEMO14C TM    OVRIDE,X'80'       NO - DISP BUT CHECK IF OVERRIDE DEM           
         BZ    ODEMO14D                                                         
         EDIT  APFULL,(6,(R3)),1,DUB=APDUB,WRK=APWORK,FLOAT=*                   
         B     ODEMO20                                                          
*                                                                               
ODEMO14D EDIT  APFULL,(6,(R3)),1,DUB=APDUB,WRK=APWORK                           
         B     ODEMO20                                                          
***  2 DECIMAL  ***                                                             
ODEMO14E TM    OVRIDE,X'80'       NO - DISP BUT CHECK IF OVERRIDE DEM           
         BZ    ODEMO14F                                                         
         EDIT  APFULL,(6,(R3)),2,DUB=APDUB,WRK=APWORK,FLOAT=*                   
         B     ODEMO20                                                          
*                                                                               
ODEMO14F EDIT  APFULL,(6,(R3)),2,DUB=APDUB,WRK=APWORK                           
         B     ODEMO20                                                          
***  2 DECIMAL  ***                                                             
*                                                                               
ODEMO16  DS    0H                                                               
*****  APFULL ALREADY SETUP                                                     
**EMO16  XC    APFULL,APFULL      GET ROUND AND GET RID OF                      
**       MVC   APFULL+1(3),TPRTG  DECIMAL                                       
         BRAS  RE,ROUNDIT2                                                      
***  2 DECIMAL  ***                                                             
         BRAS  RE,LDEMHLDR         CHECK OUT THE DEMO CATEGORY                  
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    ODEMO16C             - NOPE, NOT OK                              
*                                                                               
ODEMO16A TM    OVRIDE,X'40'        2 DECIMAL?                                   
         BNZ   ODEMO16E             - YUP                                       
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    ODEMO16C             - NO, NORMAL                                
         OI    OVRIDE,X'40'         - YES, TURN ON BIT (THIS IS TOTALS)         
         B     ODEMO16E                                                         
***  2 DECIMAL  ***                                                             
ODEMO16C TM    OVRIDE,X'80'       PRINT WITH * IF OVERRIDE DEMO                 
         BZ    ODEMO16D                                                         
         EDIT  APFULL,(6,(R3)),DUB=APDUB,WRK=APWORK,FLOAT=*                     
         B     ODEMO20                                                          
*                                                                               
ODEMO16D EDIT  APFULL,(6,(R3)),DUB=APDUB,WRK=APWORK                             
         B     ODEMO20                                                          
***  2 DECIMAL  ***                                                             
ODEMO16E TM    OVRIDE,X'80'       PRINT WITH * IF OVERRIDE DEMO                 
         BZ    ODEMO16F                                                         
         EDIT  APFULL,(6,(R3)),1,DUB=APDUB,WRK=APWORK,FLOAT=*                   
         B     ODEMO20                                                          
*                                                                               
ODEMO16F EDIT  APFULL,(6,(R3)),1,DUB=APDUB,WRK=APWORK                           
         B     ODEMO20                                                          
***  2 DECIMAL  ***                                                             
*                                                                               
ODEMO20  DS    0H                 NOW DO CPP ON NEXT LINE                       
         CLI   SUPCPPM,C'Y'       SUPPRESS CPP/CPM?                             
         BE    ODEMO30            YES                                           
****  APFULL GETS OVERWRITTEN IN CALCPP!!                                       
         MVC   APWORK(1),OVRIDE                                                 
         BRAS  RE,CALCPP           CALCPP USES APWORK                           
         MVC   OVRIDE(1),APWORK                                                 
         L     R1,APFULL                                                        
         C     R1,=F'99999'                                                     
         BH    ODEMO25                                                          
         EDIT  APFULL,(6,(R6)),2,DUB=APDUB,WRK=APWORK                           
         B     ODEMO30                                                          
ODEMO25  BRAS  RE,ROUNDIT         GET'S RID OF PENNIES                          
         EDIT  APFULL,(6,(R6)),DUB=APDUB,WRK=APWORK                             
*                                                                               
ODEMO30  LA    R3,7(R3)           BUMP PRINT LINE 1                             
         LA    R6,7(R6)           BUMP PRINT LINE 2                             
         LA    R2,4(R2)           NEXT RATING                                   
         MVC   TPRTG,1(R2)        MOVE IN NEW RATING                            
         LA    RF,4(RF)           NEXT SPOTXRATING TOTAL                        
***  2 DECIMAL                                                                  
         L     RE,ALDEMHLD                                                      
         LA    RE,6(RE)                                                         
         ST    RE,ALDEMHLD         DOING THIS TO TEST OUT THE DEMCAT            
***  2 DECIMAL                                                                  
         BCT   R8,ODEMO13                                                       
ODEMOX   B     EXIT                                                             
         EJECT                                                                  
* GOAL DOLLARS (4), ACHIEVED DOLLARS (4)                                        
*                                                                               
IBVGDOL  CLI   READRC,C'D'        BOUGHT VS GOAL DOLLARS INPUT                  
         BE    IBVG15                                                           
         L     R1,AIOAREA3                                                      
         MVC   0(4,R2),0(R1)                                                    
         B     EXIT                                                             
IBVG15   BAS   RE,DOBDOL                                                        
         MVC   4(4,R2),APFULL                                                   
         B     EXIT                                                             
*                                                                               
*                                                                               
OBVGDOL  CLI   GLHOOK,GLINCOMP    BOUGHT VS GOAL DOLS OUTPUT                    
         BNE   OBVGDOL5                                                         
         TM    GLINDS,GLTOTLIN                                                  
         BZ    EXIT                                                             
         MVC   SVTDOL,4(R2)       SAVE TOTAL ACHIEVED DOLLARS                   
         B     EXIT                                                             
*                                                                               
OBVGDOL5 ICM   RF,15,4(R2)        (RF) = ACHIEVED DOLLARS                       
         BZ    EXIT                                                             
         CLI   CUDMED,C'C'        IF CANADIAN & NEGATIVE -SKIP DISPLAY          
         BNE   *+12                                                             
         TM    4(R2),X'80'                                                      
         BO    EXIT                                                             
         CVD   RF,APDUB                                                         
         ZAP   APWORK(16),APDUB                                                 
         MP    APWORK(16),=PL8'100'                                             
         MVC   APFULL,SVTDOL      TOTAL ACHIEVED DOLLARS                        
         CLI   GLARGS,C'S'        IF IT IS STATION FORMAT AND                   
         BNE   OBVGDOL8           IT IS NOT A TOTAL LINE                        
         TM    GLINDS,GLTOTLIN    OK TO USE TOTAL ACHIEVED DOLLARS              
         BNO   *+10               OTHERWISE                                     
OBVGDOL8 MVC   APFULL,0(R2)       USE GOAL DOLLARS                              
         ICM   R1,15,APFULL                                                     
         BZ    EXIT                                                             
         CLI   CUDMED,C'C'        IF CANADIAN & NEGATIVE -SKIP DISPLAY          
         BNE   *+12                                                             
         TM    APFULL,X'80'                                                     
         BO    EXIT                                                             
         SRA   R1,1                                                             
         CVD   R1,APDUB                                                         
         AP    APWORK+8(8),APDUB(8)   ADD HALF OF DIVISOR                       
         ICM   R1,15,APFULL                                                     
         BZ    EXIT                                                             
         CVD   R1,APDUB                                                         
         DP    APWORK(16),APDUB                                                 
         ZAP   APDUB,APWORK(8)                                                  
         CVB   RF,APDUB                                                         
         EDIT  (RF),(4,0(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
IWBVGDL  CLI   READRC,C'D'        BOUGHT VS GOAL DOLLARS INPUT                  
         BE    IWBVGD15                                                         
         L     R1,AIOAREA3                                                      
         MVC   0(4,R2),0(R1)                                                    
         CLI   GLARGS,C'W'                                                      
         BNE   EXIT                                                             
         LA    R1,8(R1)                                                         
         BAS   R8,POSIT                                                         
         MVC   0(4,R2),0(R1)                                                    
         B     EXIT               EXIT FOR GOAL INPUT                           
IWBVGD15 BAS   RE,DOBDOL                                                        
         MVC   4(4,R2),APFULL                                                   
         B     EXIT                                                             
         SPACE                                                                  
* ACHIEVED(BOUGHT) DOLLS X 100 / GOAL DOLLS = % DOLLAR                          
*                                                                               
OWBVGDL  TM    MISCFLG1,MF1DONTP                                                
         BNZ   EXIT                                                             
         CLI   GLHOOK,GLINCOMP    BOUGHT VS GOAL FOR WEEKLY                     
         BNE   OWBVGDL5                                                         
         TM    GLINDS,GLTOTLIN                                                  
         BZ    EXIT                                                             
         MVC   SVPDOL,4(R2)                                                     
         B     EXIT                                                             
OWBVGDL5 ICM   RF,15,4(R2)       ACHIEVED DOLLARS                               
         BZ    EXIT                                                             
         CLI   CUDMED,C'C'        IF CANADIAN & NEGATIVE -SKIP DISPLAY          
         BNE   *+12                                                             
         TM    4(R2),X'80'                                                      
         BO    EXIT                                                             
         CVD   RF,APDUB                                                         
         ZAP   APWORK(16),APDUB                                                 
         MP    APWORK(16),=PL8'100'                                             
         MVC   APFULL,SVPDOL      ACHIEVED TOTAL                                
         CLI   GLARGS,C'S'                                                      
         BNE   OWBVGDL8                                                         
         CLC   INOFRM,=C'PT'                                                    
         BE    OWBVGDL8                                                         
         TM    GLINDS,GLTOTLIN                                                  
         BZ    OWBVGD9A                                                         
         MVC   APFULL,TOTGDOL                                                   
         B     *+10                                                             
OWBVGDL8 MVC   APFULL,0(R2)       GOAL DOLLARS                                  
******** TM    GLINDS,GLTOTLIN    TOTAL LINE?                                   
******** BO    OWBVGD9A                                                         
******** CLC   INOFRM,=C'PT'        NO - DPT?                                   
******** BE    OWBVGD9A                                                         
******** TM    CMPOPTS,X'08'        NO - DON'T SHOW FOR DAILY CMP               
******** BO    EXIT                                                             
OWBVGD9A ICM   R1,15,APFULL                                                     
         BZ    EXIT                                                             
         CLI   CUDMED,C'C'        IF CANADIAN & NEGATIVE -SKIP DISPLAY          
         BNE   *+12                                                             
         TM    APFULL,X'80'                                                     
         BO    EXIT                                                             
         SRA   R1,1                                                             
         CVD   R1,APDUB                                                         
         AP    APWORK+8(8),APDUB(8) ADD HALF OF DIVISOR                         
         ICM   R1,15,APFULL                                                     
         BZ    EXIT                                                             
         CVD   R1,APDUB                                                         
         DP    APWORK(16),APDUB                                                 
         ZAP   APDUB,APWORK(8)                                                  
         CVB   RF,APDUB                                                         
         EDIT  (RF),(4,0(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                      
         B     EXIT                                                             
         EJECT                                                                  
* GOAL POINTS(4) FOLLOWED BY ACHIEVED POINTS (4)                                
*                                                                               
IBVGPTS  CLI   READRC,C'D'        BOUGHT VS GOAL POINTS INPUT                   
         BE    IBVGP15                                                          
         L     R1,AIOAREA3        GOAL SECTION                                  
         MVC   0(4,R2),4(R1)                                                    
         B     EXIT                                                             
IBVGP15  XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,7,SVFRTG                                                      
         BZ    EXIT                                                             
         MH    RF,GDTLCST                                                       
         ST    RF,4(R2)                                                         
         B     EXIT                                                             
         SPACE                                                                  
* ACHIEVED(BOUGHT) PNTS X 100 / GOAL PNTS = % POINTS                            
*                                                                               
OBVGPTS  CLI   GLHOOK,GLINCOMP    BOUGHT VS GOAL POINTS OUTPUT                  
         BNE   OBVGPT5                                                          
         TM    GLINDS,GLTOTLIN                                                  
         BZ    EXIT                                                             
         MVC   SVTPNT,4(R2)       SAVE ACHEIVED TOTAL PNTS                      
         B     EXIT                                                             
OBVGPT5  ICM   RF,15,4(R2)        (RF) = ACHIEVED POINTS                        
         BZ    EXIT                                                             
         CLI   CUDMED,C'C'        IF CANADIAN & NEGATIVE -SKIP DISPLAY          
         BNE   *+12                                                             
         TM    4(R2),X'80'                                                      
         BO    EXIT                                                             
         MVC   APFULL,SVTPNT      TOTAL ACHIEVED POINTS                         
         CLI   GLARGS,C'S'        USE TOTAL ACHIEVED POINTS IF                  
         BNE   OBVGPT8            STATION FORMAT AND NOT TOTAL LINE             
         TM    GLINDS,GLTOTLIN                                                  
         BNO   *+10                                                             
OBVGPT8  MVC   APFULL,0(R2)       GOAL POINTS                                   
         ICM   R1,15,APFULL                                                     
         BZ    EXIT                                                             
         CLI   CUDMED,C'C'        IF CANADIAN & NEGATIVE -SKIP DISPLAY          
         BNE   *+12                                                             
         TM    APFULL,X'80'                                                     
         BO    EXIT                                                             
         XR    RE,RE                                                            
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    OBVGPT10             - NOPE                                      
         BRAS  RE,LDEMHLD2         CHECK OUT THE MAIN DEMO CATEGORY             
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    OBVGPT10             - NOPE, NOT OK                              
*                                                                               
         XR    RE,RE                                                            
         M     RE,=F'10'                                                        
         B     OBVGPT12                                                         
***  2 DECIMAL  ***                                                             
OBVGPT10 M     RE,=F'100'                                                       
OBVGPT12 SRA   R1,1                                                             
         AR    RF,R1                                                            
         ICM   R1,15,APFULL                                                     
         BZ    EXIT                                                             
         DR    RE,R1                                                            
         EDIT  (RF),(4,0(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
IWBVGPT  CLI   READRC,C'D'        BOUGHT VS GOAL POINTS INPUT                   
         BE    IWBVGP15                                                         
         L     R1,AIOAREA3        GOAL SECTION                                  
         MVC   0(4,R2),4(R1)                                                    
         CLI   GLARGS,C'W'                                                      
         BNE   EXIT                                                             
IWBVGP10 LA    R1,220(R1)                                                       
         BAS   R8,POSIT                                                         
         MVC   0(4,R2),0(R1)                                                    
         B     EXIT                                                             
IWBVGP15 XR    RE,RE              BOUGHT (ACHIEVED) SECTION                     
         XR    RF,RF                                                            
         ICM   RF,7,SVFRTG                                                      
         BZ    EXIT                                                             
         MH    RF,GDTLCST                                                       
         ST    RF,4(R2)                                                         
         B     EXIT                                                             
*                                                                               
*                                                                               
OWBVGPT  TM    MISCFLG1,MF1DONTP                                                
         BNZ   EXIT                                                             
         CLI   GLHOOK,GLINCOMP    WEEKLY CALCULATIONS                           
         BNE   OWBVGPT5                                                         
         TM    GLINDS,GLTOTLIN                                                  
         BZ    EXIT                                                             
         MVC   SVPPNT,4(R2)                                                     
         B     EXIT                                                             
*                                                                               
OWBVGPT5 ICM   RF,15,4(R2)        (RF) = ACHIEVED POINTS                        
         BZ    EXIT                                                             
         CLI   CUDMED,C'C'        IF CANADIAN & NEGATIVE -SKIP DISPLAY          
         BNE   *+12                                                             
         TM    4(R2),X'80'                                                      
         BO    EXIT                                                             
         MVC   APFULL,SVPPNT      ACHIEVED TOTAL                                
         CLI   GLARGS,C'S'                                                      
         BNE   OWBVGPT8           NOT WEEKLY (SO STATION/DAYPART?)              
         CLC   INOFRM,=C'PT'      IF  DAYPART USE GOAL POINTS                   
         BE    OWBVGPT8                                                         
         TM    GLINDS,GLTOTLIN    WE ARE STATION - USE ACHIEVED TOTAL           
         BZ    OWBVGPT9                                                         
         MVC   APFULL,TOTGPNT                                                   
         B     OWBVGPT9                                                         
*                                                                               
OWBVGPT8 MVC   APFULL,0(R2)       GOAL POINTS                                   
******** TM    GLINDS,GLTOTLIN    TOTAL?                                        
******** BO    OWBVGPT9                                                         
******** CLC   INOFRM,=C'PT'        NO - DPT?                                   
******** BE    OWBVGPT9                                                         
******** TM    CMPOPTS,X'08'           NO - DON'T SHOW FOR DAILY CMP            
******** BO    EXIT                                                             
OWBVGPT9 ICM   R1,15,APFULL                                                     
         BZ    EXIT                                                             
         CLI   CUDMED,C'C'        IF CANADIAN & NEGATIVE -SKIP DISPLAY          
         BNE   *+12                                                             
         TM    APFULL,X'80'                                                     
         BO    EXIT                                                             
         XR    RE,RE                                                            
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    OWBVGPTC             - NOPE                                      
         BRAS  RE,LDEMHLD2         CHECK OUT THE MAIN DEMO CATEGORY             
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    OWBVGPTC             - NOPE, NOT OK                              
*                                                                               
         CLC   APFULL,SVPPNT       IS IT THE ACHIEVED TOTAL?                    
         BNE   OWBVGPTB             - NOPE, ONLY MULTIPLY BY 10 PLZ             
***** TAKEN FROM OWBVGPT5 IN CASE GOALS TOTAL WAS SAME AS ACHIEVED              
         CLI   GLARGS,C'S'                                                      
         BNE   OWBVGPTB           NOT WEEKLY (SO STATION/DAYPART?)              
         CLC   INOFRM,=C'PT'      IF  DAYPART USE GOAL POINTS                   
         BE    OWBVGPTB                                                         
         TM    GLINDS,GLTOTLIN    WE ARE STATION - USE ACHIEVED TOTAL           
         BZ    OWBVGPTC            IT'S ACHIEVED TOTAL, X 100 PLZ               
*****   ACHIEVED/ACHIEVED IS NORMAL SINCE BOTH ARE 2 DECIMALS                   
***** TAKEN FROM OWBVGPT5                                                       
OWBVGPTB M     RE,=F'10'                                                        
         B     OWBVGPTE                                                         
***  2 DECIMAL  ***                                                             
OWBVGPTC M     RE,=F'100'                                                       
OWBVGPTE SRA   R1,1                                                             
         AR    RF,R1                                                            
         ICM   R1,15,APFULL                                                     
         BZ    EXIT                                                             
         DR    RE,R1                                                            
         EDIT  (RF),(4,0(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
IDOLLS   CLI   READRC,C'D'                                                      
         BE    IDOLL20                                                          
         L     R1,AIOAREA3        GOAL DOLLAR SECTION                           
         MVC   0(4,R2),0(R1)                                                    
         B     EXIT                                                             
IDOLL20  BAS   RE,DOBDOL          BOUGHT DOLLAR                                 
         MVC   4(4,R2),APFULL                                                   
         B     EXIT                                                             
*                                                                               
ODOLLS   MVC   APFULL,0(R2)                                                     
         CLI   CUDMED,C'C'                                                      
         BNE   ODOLL10                                                          
         TM    APFULL,X'80'                                                     
         BNO   ODOLL10                                           -              
         EDIT  APFULL,(13,0(R3)),2,DUB=APDUB,WRK=APWORK,FLOAT=-                 
         B     ODOLL15                                                          
ODOLL10  DS    0H                                                               
         TM    APFULL,X'80'        IS IT A REALLY BIG NUMBER?                   
         BZ    ODOLL12                                                          
         BRAS  RE,ROUNDIT                                                       
         EDIT  APFULL,(9,4(R3)),DUB=APDUB,WRK=APWORK,FLOAT=$                    
         B     ODOLL15                                                          
ODOLL12  EDIT  (4,0(R2)),(13,0(R3)),2,DUB=APDUB,WRK=APWORK,FLOAT=$              
***                                                                             
ODOLL15  MVC   APFULL,4(R2)                                                     
         CLI   ASONOFF,ASOFF                                                    
         BE    *+12                                                             
         LA    RF,DRLWIDTH(R3)    POINT TO PRINT LINE 3                         
         B     ODOLL20                                                          
         LA    RF,DRVWIDTH(R3)                                                  
ODOLL20  TM    APFULL,X'80'                                                     
         BZ    ODOLL22                                                          
         BRAS  RE,ROUNDIT                                                       
         EDIT  APFULL,(9,4(RF)),DUB=APDUB,WRK=APWORK,FLOAT=$                    
         B     EXIT                                                             
ODOLL22  EDIT  APFULL,(13,0(RF)),2,DUB=APDUB,WRK=APWORK,FLOAT=$                 
         B     EXIT                                                             
*                                                                               
*                                                                               
IPTWK    CLI   READRC,C'D'        POINTS PER WEEK (GOALS ACHEIVED)              
         BE    IPTWK30                                                          
         L     R1,AIOAREA3                                                      
         LA    R1,220(R1)                                                       
         ZIC   R0,INOSTDTD         SO WE THE CORRECT GOALS                      
         MHI   R0,4                                                             
         AR    R1,R0                                                            
*                                  HAVE TO WORRY ABOUT THE END                  
         ZIC   R0,INOSTDTD                                                      
         ZIC   RE,CMPNWKS                                                       
         SR    RE,R0                                                            
         CHI   RE,14                                                            
         BNL   IPTWK05                                                          
         LR    R0,RE               WE ONLY HAVE TO DISPLAY SO MUCH              
         B     IPTWK10                                                          
*                                                                               
IPTWK05  LA    R0,14                                                            
IPTWK10  OC    0(4,R1),0(R1)                                                    
         BZ    IPTWK20                                                          
         MVC   APFULL,0(R1)                                                     
         BRAS  RE,ROUNDIT2                                                      
         MVC   0(4,R2),APFULL                                                   
IPTWK20  LA    R2,8(R2)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,IPTWK10                                                       
         B     EXIT                                                             
*                                                                               
IPTWK30  LA    R3,SPOTABLE         BOUGHT PTS                                   
         XR    R0,R0                                                            
         IC    R0,INOSTDTD         SO WE THE CORRECT DISPLACEMENT               
         AR    R3,R0                                                            
*                                  HAVE TO WORRY ABOUT THE END                  
         ZIC   R1,CMPNWKS                                                       
         SR    R1,R0                                                            
         CHI   R1,14                                                            
         BNL   IPTWK33                                                          
         LR    R0,R1               WE ONLY HAVE TO DISPLAY SO MUCH              
         B     IPTWK35                                                          
*                                                                               
IPTWK33  LA    R0,14                                                            
IPTWK35  SR    RF,RF                                                            
         ICM   RF,1,0(R3)                                                       
         BZ    IPTWK40                                                          
         SR    R1,R1                                                            
         ICM   R1,7,SVFRTG                                                      
         BZ    IPTWK40                                                          
         SR    RE,RE                                                            
         MR    RE,R1                                                            
         ST    RF,4(R2)                                                         
IPTWK40  LA    R2,8(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,IPTWK35                                                       
         B     EXIT                                                             
*                                                                               
OPTWK    CLI   GLHOOK,GLINCOMP    OUTPUT POINTS PER WEEK                        
         BE    EXIT                                                             
         LA    R6,14                                                            
OPTWK10  EDIT  (4,0(R2)),(4,0(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                 
         CLI   ASONOFF,ASOFF                                                    
         BE    *+12                                                             
         LA    RF,DRLWIDTH(R3)    POINT TO PRINT LINE 3                         
         B     OPTWK20                                                          
         LA    RF,DRVWIDTH(R3)                                                  
OPTWK20  OC    4(4,R2),4(R2)                                                    
         BZ    OPTWK30                                                          
         MVC   APFULL,4(R2)                                                     
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   IS IT 2 DECIMALS?                            
         BZ    OPTWK23              - NOPE                                      
         BRAS  RE,LDEMHLD2         CHECK OUT THE MAIN DEMO CATEGORY             
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    OPTWK23              - NOPE, NOT OK                              
*                                                                               
         BRAS  RE,ROUNDIT                                                       
         B     OPTWK25                                                          
***  2 DECIMAL                                                                  
OPTWK23  BRAS  RE,ROUNDIT2                                                      
OPTWK25  EDIT  APFULL,(4,0(RF)),DUB=APDUB,WRK=APWORK,ALIGN=RIGHT                
OPTWK30  LA    R2,8(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R6,OPTWK10                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
IPNTS    CLI   READRC,C'D'        POINTS INPUT (G & B)                          
         BE    IPNT20                                                           
         L     R1,AIOAREA3                                                      
         OC    4(4,R1),4(R1)                                                    
         BZ    EXIT                                                             
         MVC   APFULL,4(R1)                                                     
         BRAS  RE,ROUNDIT2                                                      
         MVC   0(4,R2),APFULL                                                   
         B     EXIT                                                             
IPNT20   XR    RF,RF              BOUGHT SECTION                                
         ICM   RF,7,SVFRTG                                                      
         BZ    EXIT                                                             
         MH    RF,GDTLCST                                                       
         ST    RF,4(R2)                                                         
         B     EXIT                                                             
*                                                                               
*                                                                               
****  SEEMS LIKE IT'S THE ACHIEVED POINT TOTAL WHEN IT GETS HERE                
OPNTS    EDIT  (4,0(R2)),(6,0(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                 
         CLI   ASONOFF,ASOFF                                                    
         BE    *+12                                                             
         LA    RF,DRLWIDTH(R3)    POINT TO PRINT LINE 3                         
         B     OPNT20                                                           
         LA    RF,DRVWIDTH(R3)                                                  
OPNT20   OC    4(4,R2),4(R2)                                                    
         BZ    EXIT                                                             
         MVC   APFULL,4(R2)                                                     
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   IS IT 2 DECIMALS?                            
         BZ    OPNT23                                                           
         BRAS  RE,LDEMHLDR         CHECK OUT THE DEMO CATEGORY                  
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    OPNT23               - NOPE, NOT OK                              
*                                                                               
         BRAS  RE,ROUNDIT                                                       
         B     OPNT25                                                           
***  2 DECIMAL                                                                  
OPNT23   BRAS  RE,ROUNDIT2                                                      
OPNT25   EDIT  APFULL,(6,0(RF)),DUB=APDUB,WRK=APWORK                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ---- ROUTINES FOR RECAPS                                                      
*                                                                               
IGDOLLS  CLI   READRC,C'D'        GOAL DOLLARS INPUT                            
         BE    EXIT                                                             
         L     R1,AIOAREA3                                                      
         MVC   0(4,R2),0(R1)                                                    
         B     EXIT                                                             
*                                                                               
OGDOLLS  CLI   ZSTA,1             GOAL DOLLARS OUTPUT                           
         BNE   OGDOLL5                                                          
         TM    SUMIND,SUMIDPT                                                   
         BO    OGDOLL5            GO AHEAD FOR DAYPART SUMMARY                  
         TM    GLINDS,GLTOTLIN                                                  
         BNO   EXIT                                                             
OGDOLL5  OC    0(4,R2),0(R2)                                                    
         BZ    EXIT                                                             
         MVC   APFULL,0(R2)                                                     
         BRAS  RE,ROUNDIT                                                       
         EDIT  APFULL,(8,0(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                    
         B     EXIT                                                             
*                                                                               
*                                                                               
IBDOLLS  CLI   READRC,C'D'        BOUGHT DOLLARS INPUT                          
         BNE   EXIT                                                             
         BAS   RE,DOBDOL                                                        
         MVC   0(4,R2),APFULL                                                   
         B     EXIT                                                             
*                                                                               
OBDOLLS  MVC   APFULL,0(R2)       BOUGHT DOLLARS OUTPUT                         
         TM    MISCFLG1,MF1DONTP                                                
         BNZ   EXIT                                                             
         TM    APFULL,X'80'        IS IT A REALLY BIG NUMBER?                   
         BNZ   OBDOLL05             - YUP IT IS                                 
         CLI   GLARGS,C'P'        WANT PENNIES TO PRINT?                        
         BE    OBDOLL10                                                         
OBDOLL05 BRAS  RE,ROUNDIT         NO - ROUND TO DOLLARS                         
**       L     RE,ATWA                                                          
**       USING TWAD,RE                                                          
**       CLC   =C'RECAP',BWSACT    IS IT RECAP?                                 
**       DROP  RE                                                               
         CLI   0(R2),X'80'         IS IT A REALLY BIG NUMBER?                   
         BO    OBDOLL07             - YUP IT IS                                 
         EDIT  APFULL,(8,0(R3)),DUB=APDUB,WRK=APWORK                            
         B     EXIT                                                             
OBDOLL07 EDIT  APFULL,(8,5(R3)),DUB=APDUB,WRK=APWORK                            
         B     EXIT                                                             
OBDOLL10 EDIT  APFULL,(13,0(R3)),2,DUB=APDUB,WRK=APWORK                         
         B     EXIT                                                             
*                                                                               
*                                                                               
IGPNTS   CLI   READRC,C'D'        GOAL POINTS INPUT                             
         BE    EXIT                                                             
         L     R1,AIOAREA3                                                      
         OC    4(4,R1),4(R1)                                                    
         BZ    EXIT                                                             
         MVC   APFULL,4(R1)                                                     
         BRAS  RE,ROUNDIT2                                                      
         MVC   0(4,R2),APFULL                                                   
         B     EXIT                                                             
*                                                                               
OGPNTS   CLI   ZSTA,1             GOAL POINTS OUTPUT                            
         BNE   OGPNT10                                                          
         TM    SUMIND,SUMIDPT                                                   
         BO    OGPNT10            GO AHEAD FOR DAYPART SUMMARY                  
         TM    GLINDS,GLTOTLIN                                                  
         BNO   EXIT                                                             
OGPNT10  EDIT  (4,0(R2)),(4,0(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                 
         B     EXIT                                                             
*                                                                               
*                                                                               
IBPNTS   CLI   READRC,C'D'        BOUGHT POINTS INPUT                           
         BNE   EXIT                                                             
         XR    RF,RF                                                            
         ICM   RF,7,SVFRTG                                                      
         BZ    EXIT                                                             
         MH    RF,GDTLCST                                                       
         ST    RF,0(R2)                                                         
         B     EXIT                                                             
*                                                                               
OBPNTS   CLI   GLHOOK,GLINCOMP    BOUGHT POINTS OUTPUT                          
         BE    EXIT                                                             
         TM    MISCFLG1,MF1DONTP                                                
         BNZ   EXIT                                                             
         OC    0(4,R2),0(R2)                                                    
         BZ    EXIT                                                             
         MVC   APFULL,0(R2)                                                     
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   IS IT 2 DECIMALS?                            
         BZ    OBPNTS05                                                         
         BRAS  RE,LDEMHLD2         CHECK OUT THE MAIN DEMO CATEGORY             
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    OBPNTS05             - NOPE, NOT OK                              
*                                                                               
         BRAS  RE,ROUNDIT                                                       
         B     OBPNTS10                                                         
***  2 DECIMAL                                                                  
OBPNTS05 BRAS  RE,ROUNDIT2                                                      
OBPNTS10 EDIT  APFULL,(4,0(R3)),DUB=APDUB,WRK=APWORK                            
         B     EXIT                                                             
*                                                                               
*                                                                               
IGCPP    CLI   READRC,C'D'        GOAL COST PER POINT INPUT                     
         BE    EXIT                                                             
         L     R1,AIOAREA3                                                      
         MVC   0(4,R2),0(R1)      DOLLARS                                       
         MVC   4(4,R2),4(R1)      PNTS                                          
         B     EXIT                                                             
*                                                                               
OGCPP    CLI   GLHOOK,GLINCOMP    GOAL COST PER POINT OUTPUT                    
         BE    OGCPPX                                                           
         CLI   ZSTA,1                                                           
         BNE   OGCPP10                                                          
         TM    SUMIND,SUMIDPT                                                   
         BO    OGCPP10            GO AHEAD FOR DAYPART SUMMARY                  
         TM    GLINDS,GLTOTLIN                                                  
         BNO   OGCPPX                                                           
OGCPP10  CLI   CUDMED,C'C'        IF CANADIAN                                   
         BNE   OGCPP20                                                          
         TM    0(R2),X'80'        AND EITHER AMT NEG - SKIP CPP                 
         BO    OGCPPX                                                           
         TM    4(R2),X'80'                                                      
         BO    OGCPPX                                                           
OGCPP20  DS    0H                                                               
         MVI   APWORK,C'G'         LET EDCPP KNOW IT'S FROM GOAL SUB            
         BAS   RE,EDCPP                                                         
OGCPPX   B     EXIT                                                             
*                                                                               
*                                                                               
IBCPP    CLI   READRC,C'D'        BOUGHT COST PER POINT INPUT                   
         BNE   EXIT                                                             
         BAS   RE,DOBDOL                                                        
         MVC   0(4,R2),APFULL                                                   
         XR    RF,RF                                                            
         ICM   RF,7,SVFRTG                                                      
         BZ    EXIT                                                             
         MH    RF,GDTLCST                                                       
         ST    RF,4(R2)           POINTS                                        
         B     EXIT                                                             
*                                                                               
OBCPP    CLI   GLHOOK,GLINCOMP    BOUGHT COST PER POINT OUTPUT                  
         BE    EXIT                                                             
         TM    MISCFLG1,MF1DONTP                                                
         BNZ   EXIT                                                             
         MVI   APWORK,C'B'         BOUGHT SUB, NOT GOAL SUB                     
         BAS   RE,EDCPP                                                         
         B     EXIT                                                             
*                                                                               
*                                                                               
IBCPP2   CLI   READRC,C'D'        BOUGHT COST PER POINT INPUT 2                 
         BNE   EXIT                                                             
         BAS   RE,DOBDOL                                                        
         MVC   0(4,R2),APFULL                                                   
         XR    RF,RF                                                            
         ICM   RF,7,SV2RTG        SECOND DEMO                                   
         BZ    EXIT                                                             
         MH    RF,GDTLCST                                                       
         ST    RF,4(R2)           POINTS                                        
         B     EXIT                                                             
*                                                                               
OBCPP2   CLI   GLHOOK,GLINCOMP    BOUGHT COST PER POINT OUTPUT 2                
         BE    EXIT                                                             
         MVI   APWORK,C'B'         BOUGHT SUB, NOT GOAL SUB                     
         BAS   RE,EDCPP                                                         
         B     EXIT                                                             
*                                                                               
*                                                                               
ISPOTS   CLI   READRC,C'D'        SPOTS INPUT                                   
         BNE   EXIT                                                             
         MVC   0(2,R2),GDTLCST                                                  
         B     EXIT                                                             
*                                                                               
OSPOTS   TM    MISCFLG1,MF1DONTP                                                
         BNZ   EXIT                                                             
         EDIT  (2,0(R2)),(5,0(R3)),DUB=APDUB,WRK=APWORK                         
         B     EXIT                                                             
*                                                                               
*                                                                               
IAVGPTS  CLI   READRC,C'D'        AVERAGE POINTS INPUT                          
         BNE   EXIT                                                             
         XR    RF,RF                                                            
         ICM   RF,7,SVFRTG                                                      
         BZ    EXIT                                                             
         MH    RF,GDTLCST                                                       
         ST    RF,0(R2)           BOUGHT(PURCHASED) POINTS                      
         MVC   4(2,R2),GDTLCST    SPOTS                                         
         B     EXIT                                                             
*                                                                               
OAVGPTS  CLI   GLHOOK,GLINCOMP    AVERAGE POINTS OUTPUT                         
         BE    EXIT                                                             
         TM    MISCFLG1,MF1DONTP                                                
         BNZ   EXIT                                                             
         MVC   APFULL,0(R2)                                                     
         OC    APFULL,APFULL                                                    
         BZ    EXIT                                                             
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   IS IT 2 DECIMALS?                            
         BZ    OAVGPTS3                                                         
         BRAS  RE,LDEMHLD2         CHECK OUT THE MAIN DEMO CATEGORY             
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    OAVGPTS3             - NOPE, NOT OK                              
*                                                                               
         BRAS  RE,ROUNDIT                                                       
         B     OAVGPTS5                                                         
***  2 DECIMAL                                                                  
OAVGPTS3 BRAS  RE,ROUNDIT2                                                      
OAVGPTS5 ICM   RF,15,APFULL       (RF) = PURCHASED PNTS                         
         BZ    EXIT                                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,4(R2)         (R1) = SPOTS                                  
         BZ    EXIT                                                             
         XR    RE,RE                                                            
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(3,0(R3)),DUB=APDUB,WRK=APWORK                              
         B     EXIT                                                             
*                                                                               
*                                                                               
IWGDOL   CLI   READRC,C'D'                                                      
         BE    EXIT                                                             
         L     R1,AIOAREA3                                                      
         LA    R1,8(R1)                                                         
         BAS   R8,POSIT                                                         
         MVC   0(4,R2),0(R1)                                                    
         B     EXIT                                                             
*                                                                               
OWGDOL   CLI   GLHOOK,GLINCOMP                                                  
         BE    EXIT                                                             
         TM    MISCFLG1,MF1DONTP                                                
         BNZ   EXIT                                                             
         MVC   APFULL,0(R2)                                                     
         OC    APFULL,APFULL                                                    
         BZ    EXIT                                                             
         BRAS  RE,ROUNDIT                                                       
         EDIT  APFULL,(8,0(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                    
         B     EXIT                                                             
*                                                                               
*                                                                               
IWGPNT   CLI   READRC,C'D'                                                      
         BE    EXIT                                                             
         L     R1,AIOAREA3                                                      
         LA    R1,220(R1)                                                       
         BAS   R8,POSIT                                                         
         MVC   0(4,R2),0(R1)                                                    
         B     EXIT                                                             
*                                                                               
OWGPNT   CLI   GLHOOK,GLINCOMP                                                  
         BE    EXIT                                                             
         TM    MISCFLG1,MF1DONTP                                                
         BNZ   EXIT                                                             
         MVC   APFULL,0(R2)                                                     
         OC    APFULL,APFULL                                                    
         BZ    EXIT                                                             
         BRAS  RE,ROUNDIT2                                                      
         EDIT  APFULL,(4,0(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                    
         B     EXIT                                                             
*                                                                               
*                                                                               
IWGCPP   CLI   READRC,C'D'                                                      
         BE    EXIT                                                             
         L     R1,AIOAREA3                                                      
         LA    R1,8(R1)                                                         
         BAS   R8,POSIT                                                         
         MVC   0(4,R2),0(R1)      DOLLARS                                       
         L     R1,AIOAREA3                                                      
         LA    R1,220(R1)                                                       
         BAS   R8,POSIT                                                         
         MVC   4(4,R2),0(R1)      POINTS                                        
         B     EXIT                                                             
*                                                                               
OWGCPP   CLI   GLHOOK,GLINCOMP                                                  
         BE    OWGCPPX                                                          
         TM    MISCFLG1,MF1DONTP                                                
         BNZ   EXIT                                                             
         CLI   CUDMED,C'C'        IF CANADIAN                                   
         BNE   OWGCPP10                                                         
         TM    0(R2),X'80'        AND EITHER AMT NEG - SKIP CPP                 
         BO    OWGCPPX                                                          
         TM    4(R2),X'80'                                                      
         BO    OWGCPPX                                                          
OWGCPP10 MVI   APWORK,C'G'         IT'S A GOAL SUB                              
         BAS   RE,EDCPP                                                         
OWGCPPX  B     EXIT                                                             
*                                                                               
*                                                                               
POSIT    ZIC   RE,WEEKNO                                                        
         LTR   RE,RE                                                            
         BZ    POSIT5                                                           
         MH    RE,=H'4'                                                         
         AR    R1,RE                                                            
POSIT5   BR    R8                                                               
*                                                                               
*                                                                               
IDCPPM   CLI   READRC,C'D'                                                      
         BNE   EXIT                                                             
         BAS   RE,DOBDOL                                                        
         MVC   0(4,R2),APFULL     DOLLARS                                       
         LA    R2,4(R2)                                                         
         LA    R0,4                                                             
         LA    R1,LDEMHLD+2       POINT TO RATINGS                              
IDCPPM8  XR    RF,RF                                                            
         ICM   RF,7,1(R1)         GET RATING                                    
         BZ    *+8                                                              
         MH    RF,GDTLCST                                                       
         ST    RF,0(R2)                                                         
***  EVERYTHING SHOULD BE CONVERTED ALREADY IN SPNWS10 GETRTG15!!               
***  2 DECIMAL                                                                  
         OC    1(3,R2),1(R2)       IS IT ZERO?                                  
         BZ    IDCPPM10             - YUP, NO NEED FOR FLAGS BITS               
         MVC   0(1,R2),0(R1)       MOVE IN THE 2 DECIMAL BIT                    
***  2 DECIMAL                                                                  
IDCPPM10 LA    R1,6(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,IDCPPM8                                                       
         B     EXIT                                                             
*                                                                               
*                                                                               
ODCPPM   CLI   GLHOOK,GLINCOMP                                                  
         BE    EXIT                                                             
         OC    0(4,R2),0(R2)                                                    
         BZ    EXIT                                                             
         MVC   TPCOST,0(R2)                                                     
         LA    R2,4(R2)                                                         
         LA    R8,4               # OF DEMOS/CPP/CPM                            
***  2 DECIMAL                                                                  
         LA    RE,LDEMHLD          USING THIS FOR THE DEMO CATEGORIES           
         ST    RE,ALDEMHLD                                                      
***  2 DECIMAL                                                                  
ODCPPM1  MVC   TPRTG,1(R2)                                                      
****  SETTING UP APFULL FOR THE FIRST TIME IN ODCPPM YOU IDIOT!!                
         XC    APFULL,APFULL       GONNA ROUNDIT                                
         MVC   APFULL+1(3),TPRTG                                                
****   THIS WASN'T SETUP EARLY ENOUGH BEFORE                                    
         SR    R1,R1                                                            
         ICM   R1,7,TPRTG                                                       
         C     R1,=F'99999'                                                     
         BH    ODCPPM4                                                          
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    ODCPPM1E             - NO, NORMAL                                
         BRAS  RE,LDEMHLDR                                                      
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    ODCPPM1E             - NOPE                                      
*                                                                               
         TM    GLINDS,GLTOTLIN     WE ON A TOTAL LINE?                          
         BZ    ODCPPM1C             - NOPE                                      
         BRAS  RE,ROUNDIT2                                                      
***      MVC   TPRTG,APFULL+1      PRESERVE THE 2 DECIMAL TOTAL                 
         B     ODCPPM1E                                                         
*                                                                               
ODCPPM1C TM    0(R2),X'40'         2 DECIMAL?                                   
         BNZ   ODCPPM2              - YUP                                       
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    ODCPPM1E             - NO, NORMAL                                
         OI    0(R2),X'40'          - YES, TURN ON BIT (THIS IS TOTALS)         
         B     ODCPPM2                                                          
***  2 DECIMAL  ***                                                             
ODCPPM1E EDIT  APFULL,(6,(R3)),1,DUB=APDUB,WRK=APWORK                           
         B     ODCPPM6                                                          
***  2 DECIMAL  ***                                                             
ODCPPM2  EDIT  APFULL,(6,(R3)),2,DUB=APDUB,WRK=APWORK                           
         B     ODCPPM6                                                          
***  2 DECIMAL  ***                                                             
*                                                                               
ODCPPM4  XC    APFULL,APFULL                                                    
         MVC   APFULL+1(3),TPRTG                                                
         BRAS  RE,ROUNDIT2                                                      
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    ODCPPM4E             - NO, NORMAL                                
         BRAS  RE,LDEMHLDR                                                      
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    ODCPPM4E             - NOPE                                      
*                                                                               
*        TM    GLINDS,GLTOTLIN     WE ON A TOTAL LINE?                          
*        BZ    ODCPPM4A             - NOPE                                      
*        XC    APFULL,APFULL       GONNA ROUNDIT                                
*        MVC   APFULL+1(3),TPRTG                                                
*        BRAS  RE,ROUNDIT                                                       
*        MVC   TPRTG,APFULL+1                                                   
*        B     ODCPPM4E                                                         
*                                                                               
ODCPPM4A TM    0(R2),X'40'         2 DECIMAL?                                   
         BNZ   ODCPPM5              - YUP                                       
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    ODCPPM4E             - NO, NORMAL                                
         OI    0(R2),X'40'          - YES, TURN ON BIT (THIS IS TOTALS)         
         B     ODCPPM5                                                          
***  2 DECIMAL  ***                                                             
ODCPPM4E EDIT  APFULL,(6,(R3)),DUB=APDUB,WRK=APWORK                             
         B     ODCPPM6                                                          
***  2 DECIMAL  ***                                                             
ODCPPM5  EDIT  APFULL,(6,(R3)),1,DUB=APDUB,WRK=APWORK                           
         B     ODCPPM6                                                          
***  2 DECIMAL  ***                                                             
*                                                                               
ODCPPM6  LA    R3,7(R3)                                                         
         MVC   APWORK(1),0(R2)                                                  
         BRAS  RE,CALCPP                                                        
         MVC   0(1,R2),APWORK                                                   
         EDIT  APFULL,(7,0(R3)),2,DUB=APDUB,WRK=APWORK                          
*                                                                               
ODCPPM8  LA    R3,8(R3)                                                         
         LA    R2,4(R2)                                                         
***  2 DECIMAL                                                                  
         L     RE,ALDEMHLD         LDEMHLD IS 2 DEMCAT 4 DEMVAL = 6             
         LA    RE,6(RE)                                                         
         ST    RE,ALDEMHLD                                                      
***  2 DECIMAL                                                                  
         BCT   R8,ODCPPM1                                                       
         B     EXIT                                                             
         EJECT                                                                  
*==========================ROUTINES=============================*               
*                                                                               
* EDCPP - CALCS AND EDITS CPP FIELD FROM                                        
* DOLLARS/POINTS                                                                
*                                                                               
EDCPP    NTR1                                                                   
***  2 DECIMAL                                                                  
         LA    RE,LDEMHLD          EDCPP IS NEVER INVOLVED WITH A LOOP          
         ST    RE,ALDEMHLD                                                      
***  2 DECIMAL                                                                  
         SR    R0,R0                                                            
         ICM   R1,15,0(R2)                                                      
         BZ    EDCPPX                                                           
         ICM   RE,15,4(R2)                                                      
         BZ    EDCPPX                                                           
***  2 DECIMAL                                                                  
         STCM  RE,8,APBYTE                                                      
         NI    APBYTE,X'FF'-X'C0'  TAKE OFF 2 DECIMAL + OVERRIDE BIT            
         ICM   RE,8,APBYTE                                                      
         CLI   APWORK,C'G'         FROM A GOAL SUBROUTINE?                      
         BE    EDCPP10              - YUP, NOT DOING 2 DECIMALS                 
         TM    4(R2),X'40'         IS IT 2 DECIMALS                             
         BNZ   EDCPP05              - YUP                                       
         TM    APROFBTS,A00TWODC   2 DECIMAL FLAG ON?                           
         BZ    EDCPP10              - NOPE                                      
         LR    R0,RE               SAVE OFF RE                                  
         BRAS  RE,LDEMHLDR                                                      
         LR    RE,R0               RESTORE RE                                   
         XR    R0,R0                                                            
*****  MASTLFG IS SETUP IN LDEMHLDR SUBROUTINE                                  
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    EDCPP10              - NAW                                       
         OI    4(R2),X'40'         TURN ON THE 2 DECIMAL BIT                    
EDCPP05  M     R0,=F'200'                                                       
         B     EDCPP20                                                          
***  2 DECIMAL                                                                  
EDCPP10  M     R0,=F'20'                                                        
EDCPP20  DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         EDIT  (R1),(8,0(R3)),2,DUB=APDUB,WRK=APWORK,FLOAT=-                    
EDCPPX   NI    APWORK,X'00'        CLEAR OUT THE C'G' OR C'B'                   
         XIT1                                                                   
         SPACE 2                                                                
*==============================================================*                
* DOBDOL - RETURNS IN APFULL APPROPRIATE COST * # OF TELECASTS *                
*==============================================================*                
*                                                                               
DOBDOL   NTR1                                                                   
         XC    APFULL,APFULL                                                    
         CLI   COSTIND,2          COST2                                         
         BNE   DODOLL10                                                         
         ICM   RF,15,BWDCOST2                                                   
         B     DODOLL15                                                         
DODOLL10 CLI   COSTIND,3          COST3                                         
         BNE   DODOLL12                                                         
         ICM   RF,15,BWDCOST3                                                   
         B     DODOLL15                                                         
DODOLL12 ICM   RF,15,BWDCOST1     SO MUST BE COST1                              
DODOLL15 BZ    EXIT                                                             
         MH    RF,GDTLCST                                                       
         ST    RF,APFULL                                                        
         B     EXIT                                                             
         EJECT                                                                  
*=============================================================*                 
* GETSPTS - SETS SPOTABLE ACCORDING TO THE SPOTS PER WEEK ELEM*                 
*=============================================================*                 
GETSPTS  NTR1                                                                   
         XC    SPOTABLE,SPOTABLE                                                
         LA    R8,BWDEL                                                         
GETSPT5  CLI   0(R8),0            END OF RECORD?                                
         BE    GETSPTX                                                          
         CLI   0(R8),SPWELCDQ     SPOTS PER WEEK ELEMENT?                       
         BE    GETSPT7                                                          
         ZIC   R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     GETSPT5            CHECK NEXT ELEMENT                            
*                                                                               
GETSPT7  SR    RE,RE                                                            
         IC    RE,1(R8)                                                         
         SHI   RE,SPWPERWK-SPWEL+1   RE CONTAINS LENGTH OF TABLE-1              
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SPOTABLE(0),SPWPERWK-SPWEL(R8)  COPY ENTIRE SCHEDULE             
*                                                                               
         XC    APDUB,APDUB        USE FOR DATE                                  
         L     R2,ATWA            CAMPAIGN DATES                                
         AHI   R2,CMPDATSD-TWAD                                                 
*                                                                               
         LA    R8,53              MAX NUM                                       
         CLI   COSTIND,1                                                        
         BNE   GETSPT50                                                         
         OC    BWDEFDT2,BWDEFDT2                                                
         BZ    GETSPTX            ONLY ONE EFFECTIVE COST                       
         GOTO1 VDATCON,APPARM,(3,BWDEFDT2),(0,APDUB)                            
         BRAS  RE,GETBEG          FINDS BEGINNING OF WEEK FOR APDUB             
         LA    RE,SPOTABLE                                                      
GETSPT10 CLC   0(6,R2),APDUB                                                    
         BNL   GETSPT20                                                         
         LA    RE,1(RE)                                                         
         LA    R2,6(R2)                                                         
         BCT   R8,GETSPT10                                                      
         MVI   ERRNUM,36          DATE MUST BE IN CAMPAIGN DATE LIST            
         J     CLDUMP                                                           
*                                                                               
GETSPT20 MVI   0(RE),0            ZERO OUT REST                                 
         LA    RE,1(RE)                                                         
         BCT   R8,GETSPT20                                                      
         B     GETSPTX                                                          
         SPACE 2                                                                
GETSPT50 CLI   COSTIND,2                                                        
         BNE   GETSPT80                                                         
         GOTO1 VDATCON,APPARM,(3,BWDEFDT2),(0,APDUB)                            
         BRAS  RE,GETBEG                                                        
         MVC   APWORK+6(6),APDUB   SAVE BWDEFDT2                                
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    GETSPT52           GOOD TILL END OF TABLE                        
         GOTO1 VDATCON,APPARM,(3,BWDEFDT3),(0,APDUB)                            
         BRAS  RE,GETBEG                                                        
         MVC   APWORK(6),APDUB                                                  
         MVC   APDUB,APWORK+6                                                   
*                                                                               
GETSPT52 LA    RE,SPOTABLE                                                      
GETSPT53 CLC   0(6,R2),APDUB                                                    
         BNL   GETSPT55                                                         
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         LA    R2,6(R2)                                                         
         BCT   R8,GETSPT53                                                      
         MVI   ERRNUM,37          DATE MUST BE IN CAMPAIGN TABLE                
         J     CLDUMP                                                           
*                                                                               
GETSPT55 OC    BWDEFDT3,BWDEFDT3                                                
         BZ    GETSPTX            GOOD TILL END OF TABLE                        
GETSPT60 CLC   0(6,R2),APWORK                                                   
         BNL   GETSPT65                                                         
         LA    RE,1(RE)                                                         
         LA    R2,6(R2)                                                         
         BCT   R8,GETSPT60                                                      
         MVI   ERRNUM,38          DATE MUST BE THERE                            
         J     CLDUMP                                                           
*                                                                               
GETSPT65 MVI   0(RE),0            ZERO OUT REST OF TABLE                        
         LA    RE,1(RE)                                                         
         BCT   R8,GETSPT65                                                      
         B     GETSPTX                                                          
         SPACE 2                                                                
GETSPT80 CLI   COSTIND,3                                                        
         BE    GETSPT83                                                         
         MVI   ERRNUM,39          COSTIND MUST BE SET                           
         J     CLDUMP                                                           
*                                                                               
GETSPT83 GOTO1 VDATCON,APPARM,(3,BWDEFDT3),(0,APDUB)                            
         BRAS  RE,GETBEG                                                        
         LA    RE,SPOTABLE                                                      
GETSPT85 CLC   0(6,R2),APDUB                                                    
         BNL   GETSPTX                                                          
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         LA    R2,6(R2)                                                         
         BCT   R8,GETSPT85                                                      
GETSPTX  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*----------- FIRST AND TOTAL ROUTINE BREAKS ------------*                       
*                                                                               
*================================================*                              
* FSTA - ON STATION FIRST ADD TO MARKET TOTALS & *                              
* CLEAR OUT STATION RELATED COUNTERS             *                              
*================================================*                              
*                                                                               
FSTA     XC    STANUM,STANUM                                                    
         XC    STACSTS,STACSTS                                                  
         XC    STADEMOS,STADEMOS                                                
         B     EXIT                                                             
         SPACE                                                                  
*==============================================*                                
* STATOT - CALCULATE STATION TOTALS AND PRINT  *                                
*==============================================*                                
*                                                                               
STATOT   OC    STANUM,STANUM      IF NO TLCSTS NUMBER -                         
         BZ    STATX              DON'T PRINT A TOTAL STATION LINE              
         MVC   0(L'BWDSTA,R3),BWDSTA  STATION                                   
         CLI   0(R3),C'0'         IF CABLE STATION                              
         BL    *+12                                                             
         MVI   4(R3),C'/'          PRINT XXXX/XXX                               
         B     STA4                                                             
         CLI   3(R3),X'00'                                                      
         BNE   STA2                                                             
         MVI   3(R3),C'-'         3 CHARACTER STATION                           
         B     STA3                                                             
*                                                                               
STA2     MVI   4(R3),C'-'         4 CHARACTER STATION                           
STA3     MVC   5(1,R3),BWDSTA+4   MOVE IN MEDIA                                 
STA4     EDIT  STANUM,(5,8(R3)),DUB=APDUB,WRK=APWORK                            
         MVC   14(6,R3),=C'TLCSTS'                                              
*                                                                               
         CLI   SUPCST,C'Y'        SUPPRESS COST                                 
         BE    STA5                                                             
         EDIT  STACSTS,(13,26(R3)),2,DUB=APDUB,WRK=APWORK,FLOAT=$               
         MVC   TPCOST,STACSTS                                                   
STA5     LA    R8,STADEMOS                                                      
         OC    0(L'STADEMOS,R8),0(R8)                                           
         BZ    EXIT                                                             
         LA    R6,6                                                             
         LA    R3,102(R3)                                                       
         STCM  R3,15,SVP2         SAVE ADDRESS OF PRINT LINE                    
***  2 DECIMAL                                                                  
         LA    RE,LDEMHLD                                                       
         ST    RE,ALDEMHLD                                                      
***  2 DECIMAL                                                                  
*                                                                               
STAT6    XC    APFULL,APFULL       IN CASE ANYTHING IS LEFT OVER                
         CH    R6,=H'2'                                                         
         BNE   STAT7C                                                           
         TM    OPTSW,SIXDEMS      IF WE ARE PRINTING SIX DEMOS                  
         BZ    STATX              WE NEED TO PUT THE LAST TWO IN                
         ICM   R3,15,SVP2         THE CORRECT PLACE                             
         CLI   ASONOFF,ASOFF      POINT TO THE NEXT LINE AFTER DEMOS            
         BE    *+12                                                             
         LA    R3,DRLWIDTH(R3)                                                  
         B     STAT7                                                            
         LA    R3,DRVWIDTH(R3)                                                  
*                                                                               
STAT7    CLI   SUPCPPM,C'Y'       GO DIRECTLY ON THE NEXT LINE                  
         BE    STAT7C                                                           
         CLI   ASONOFF,ASOFF      BUT IF NOT SUPPRESSED                         
         BE    *+12               THEY WOULD GO ON THE FOLLOWING LINE           
         LA    R3,DRLWIDTH(R3)                                                  
         B     STAT7C                                                           
         LA    R3,DRVWIDTH(R3)                                                  
*                                                                               
STAT7C   ICM   R1,15,0(R8)                                                      
         ST    R1,APFULL           APFULL NOW READY TO BE DISPLAYED             
         C     R1,=F'99999'                                                     
         BH    STAT8                                                            
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    STAT7J               - NOPE                                      
         BRAS  RE,LDEMHLDR                                                      
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    STAT7J               - NOPE                                      
*                                                                               
******  ON THE TOTAL LINES, THEY ONLY WANT 1 DECIMAL, SO ROUND IT               
*****   APFULL IS ALREADY SETUP BY THIS POINT                                   
         BRAS  RE,ROUNDIT2                                                      
***      MVC   0(4,R8),APFULL      PRESERVE 2 DECIMAL VALUE IN R8               
         OI    0(R8),X'40'         LET'S TURN ON THE 2 DECIMAL FLAG             
***  2 DECIMAL  ***                                                             
STAT7J   EDIT  APFULL,(6,(R3)),1,DUB=APDUB,WRK=APWORK                           
         B     STAT8A                                                           
*                                                                               
STAT8    DS    0H                                                               
***  2 DECIMAL  ***                                                             
*****  APFULL IS ALREADY SETUP BY THIS POINT                                    
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    STAT8A0              - NOPE                                      
         BRAS  RE,LDEMHLDR                                                      
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    STAT8A0              - NOPE                                      
*                                                                               
*****       APFULL IS READY TO GO                                               
         BRAS  RE,ROUNDIT                                                       
         B     STAT8A2                                                          
***  2 DECIMAL  ***                                                             
STAT8A0  BRAS  RE,ROUNDIT2                                                      
STAT8A2  EDIT  APFULL,(6,(R3)),DUB=APDUB,WRK=APWORK                             
*                                                                               
STAT8A   CLI   SUPCPPM,C'Y'       SUPPRESS CPP                                  
         BE    STAT9                                                            
         MVC   TPRTG,1(R8)                                                      
         MVC   APWORK(1),0(R8)                                                  
         BRAS  RE,CALCPP           CALCPP USES APWORK                           
         MVC   0(1,R8),APWORK                                                   
         LR    RF,R3                                                            
         CLI   ASONOFF,ASOFF                                                    
         BE    *+12                                                             
         LA    RF,DRLWIDTH(RF)    POINT TO PRINT LINE 2                         
         B     STAT8B                                                           
         LA    RF,DRVWIDTH(RF)                                                  
*                                                                               
STAT8B   L     R1,APFULL                                                        
         C     R1,=F'99999'                                                     
         BH    STAT8D                                                           
         EDIT  APFULL,(6,(RF)),2,DUB=APDUB,WRK=APWORK                           
         B     STAT9                                                            
STAT8D   BRAS  RE,ROUNDIT         GET'S RID OF PENNIES                          
         EDIT  APFULL,(6,(RF)),DUB=APDUB,WRK=APWORK                             
*                                                                               
STAT9    LA    R3,7(R3)                                                         
         LA    R8,4(R8)                                                         
***  2 DECIMAL                                                                  
         L     RE,ALDEMHLD         NEED TO BUMP TO NEXT DEMO CAT                
         LA    RE,6(RE)                                                         
         ST    RE,ALDEMHLD                                                      
***  2 DECIMAL                                                                  
         BCT   R6,STAT6                                                         
STATX    B     EXIT                                                             
         EJECT                                                                  
*=====================================================*                         
* FCAMKT - FIRST ROUTINE FOR CAMPAIGN OR MARKET BREAK *                         
*=====================================================*                         
*                                                                               
FCAMKT   XC    MKTNUM,MKTNUM                                                    
         XC    STANUM,STANUM                                                    
         XC    STACSTS,STACSTS                                                  
         XC    MKTCSTS,MKTCSTS                                                  
         LA    R1,ALPHATAB        ALPHA TABLE FOR SEQUENCE NUMBERS              
         ST    R1,ADALPHA         SAVE ADDRESS                                  
         B     EXIT                                                             
         SPACE                                                                  
*=============================================*                                 
* MKTTOT - CALCULATE STATION TOTALS AND PRINT *                                 
*=============================================*                                 
*                                                                               
MKTTOT   OC    MKTNUM,MKTNUM                                                    
         BZ    MKT10                                                            
         EDIT  MKTNUM,(5,8(R3)),DUB=APDUB,WRK=APWORK                            
         MVC   14(6,R3),=C'TLCSTS'                                              
MKT10    CLI   SUPCST,C'Y'        IF SUPPRESSING COST - SUPPRESS IT             
         BE    MKTX                                                             
         EDIT  MKTCSTS,(13,26(R3)),2,DUB=APDUB,WRK=APWORK,FLOAT=$               
MKTX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* -- ON DAYPART FIRST - CLEAR COUNTER                                           
*                                                                               
FDPT     XC    DPTNUM,DPTNUM                                                    
         XC    DPTCSTS,DPTCSTS                                                  
         XC    DPTDEMOS,DPTDEMOS                                                
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
* DPTTOT - PRINT OUT NUMBER OF TELECASTS                                        
*                                                                               
DPTTOT   OC    DPTNUM,DPTNUM                                                    
         BZ    EXIT                                                             
         MVC   0(4,R3),=C'DPT='                                                 
         MVC   4(1,R3),BWDDPT                                                   
         EDIT  DPTNUM,(5,8(R3)),DUB=APDUB,WRK=APWORK                            
         MVC   14(6,R3),=C'TLCSTS'                                              
         XC    DPTNUM,DPTNUM                                                    
         CLI   SUPCST,C'Y'        SUPPRESS COST                                 
         BE    DPTT5                                                            
         EDIT  DPTCSTS,(13,26(R3)),2,DUB=APDUB,WRK=APWORK,FLOAT=$               
         MVC   TPCOST,DPTCSTS                                                   
DPTT5    LA    R8,DPTDEMOS                                                      
         OC    0(L'DPTDEMOS,R8),0(R8)                                           
         BZ    EXIT                                                             
         LA    R6,6                                                             
         LA    R3,102(R3)                                                       
         STCM  R3,15,SVP2         SAVE ADDRESS OF DEMO PRINT LINE               
***  2 DECIMAL                                                                  
         LA    RE,LDEMHLD                                                       
         ST    RE,ALDEMHLD                                                      
***  2 DECIMAL                                                                  
*                                                                               
DPTT6    XC    APFULL,APFULL       IN CASE ANYTHING IS LEFT OVER                
         CH    R6,=H'2'                                                         
         BNE   DPTT7C                                                           
         TM    OPTSW,SIXDEMS      IF PRINTING SIX DEMOS                         
         BZ    DPTTX              NOPE                                          
         ICM   R3,15,SVP2         YES - NEED TO POSITION THEM                   
         CLI   ASONOFF,ASOFF      POINT TO NEXT LINE AFTER DEMOS                
         BE    *+12                                                             
         LA    R3,DRLWIDTH(R3)                                                  
         B     DPTT7                                                            
         LA    R3,DRVWIDTH(R3)                                                  
*                                                                               
DPTT7    CLI   SUPCPPM,C'Y'       GO DIRECTLY ON THE NEXT LINE                  
         BE    DPTT7C                                                           
         CLI   ASONOFF,ASOFF      POINT TO NEXT LINE AFTER CPP/CPM              
         BE    *+12                                                             
         LA    R3,DRLWIDTH(R3)                                                  
         B     DPTT7C                                                           
         LA    R3,DRVWIDTH(R3)                                                  
*                                                                               
DPTT7C   ICM   R1,15,0(R8)                                                      
         ST    R1,APFULL           APFULL NOW READY TO BE DISPLAYED             
         C     R1,=F'99999'                                                     
         BH    DPTT7N                                                           
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    DPTT7J               - NOPE                                      
         BRAS  RE,LDEMHLDR                                                      
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    DPTT7J               - NOPE                                      
*                                                                               
******  ON THE TOTAL LINES, THEY ONLY WANT 1 DECIMAL, SO ROUND IT               
*****   APFULL IS ALREADY SETUP BY THIS POINT                                   
         MVC   APWORK(4),APFULL    PUT THIS BACK LATER FOR CPP                  
         BRAS  RE,ROUNDIT2                                                      
***      MVC   0(4,R8),APFULL      PRESERVE 2 DECIMAL VALUE                     
         OI    0(R8),X'40'         LET'S TURN ON THE 2 DECIMAL FLAG             
***  2 DECIMAL  ***                                                             
DPTT7J   EDIT  APFULL,(6,(R3)),1,DUB=APDUB,WRK=APWORK                           
         B     DPTT8A                                                           
*                                                                               
DPTT7N   DS    0H                                                               
***  2 DECIMAL  ***                                                             
*****   APFULL IS ALREADY SETUP BY THIS POINT                                   
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMALS?                         
         BZ    DPTT7S               - NOPE                                      
         BRAS  RE,LDEMHLDR                                                      
         TM    MASTFLG,LDEM2DOK    WE OK FOR 2 DECIMAL?                         
         BZ    DPTT7S               - NOPE                                      
*                                                                               
*****       APFULL IS READY TO GO                                               
         BRAS  RE,ROUNDIT                                                       
         B     DPTT7T                                                           
***  2 DECIMAL  ***                                                             
DPTT7S   BRAS  RE,ROUNDIT2                                                      
DPTT7T   EDIT  APFULL,(6,(R3)),DUB=APDUB,WRK=APWORK                             
         B     DPTT8A                                                           
*                                                                               
DPTT8A   CLI   SUPCPPM,C'Y'       SUPPRESS CPP                                  
         BE    DPTT9                                                            
*                                                                               
         MVC   TPRTG,1(R8)                                                      
         MVC   APWORK(1),0(R8)                                                  
         BRAS  RE,CALCPP                                                        
         MVC   0(1,R8),APWORK                                                   
         LR    RF,R3                                                            
         CLI   ASONOFF,ASOFF                                                    
         BE    *+12                                                             
         LA    RF,DRLWIDTH(RF)    POINT TO PRINT LINE 2                         
         B     DPTT8B                                                           
         LA    RF,DRVWIDTH(RF)                                                  
*                                                                               
DPTT8B   L     R1,APFULL                                                        
         C     R1,=F'99999'                                                     
         BH    DPTT8D                                                           
         EDIT  APFULL,(6,(RF)),2,DUB=APDUB,WRK=APWORK                           
         B     DPTT9                                                            
DPTT8D   BRAS  RE,ROUNDIT         GET'S RID OF PENNIES                          
         EDIT  APFULL,(6,(RF)),DUB=APDUB,WRK=APWORK                             
*                                                                               
DPTT9    LA    R3,7(R3)                                                         
         LA    R8,4(R8)                                                         
***  2 DECIMAL                                                                  
         L     RE,ALDEMHLD         NEED TO POINT TO NEXT DEMO CAT               
         LA    RE,6(RE)                                                         
         ST    RE,ALDEMHLD                                                      
***  2 DECIMAL                                                                  
         BCT   R6,DPTT6                                                         
DPTTX    B     EXIT                                                             
         SPACE                                                                  
*                                                                               
DPTOT    MVC   0(3,R3),QDPT                                                     
         MVC   3(4,R3),=C'-TOT'                                                 
         B     EXIT                                                             
*                                                                               
DPTOT1   MVC   0(3,R3),MQDPT                                                    
         MVC   3(4,R3),=C'-TOT'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* GTOT                                                                          
*                                                                               
GTOT     MVC   0(5,R3),=C'TOTAL'                                                
         XC    7(18,R3),7(R3)                                                   
         MVC   APFULL,TOTGPNT                                                   
         OC    APFULL,APFULL                                                    
         BZ    GTOT5                                                            
         BRAS  RE,ROUNDIT2                                                      
         EDIT  APFULL,(4,9(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                    
*                                                                               
GTOT5    MVC   APFULL,TOTGDOL                                                   
         OC    APFULL,APFULL                                                    
         BZ    GTOTX                                                            
         BRAS  RE,ROUNDIT                                                       
         CLC   =C'A2',INOFRM                                                    
         BNE   GTOT10             FOR AL2 FORMAT DOLLARS BUT NO CPP             
         CLI   GLOPTS+2,C'Y'                                                    
         BNE   GTOTX                                                            
         EDIT  APFULL,(8,21(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                   
         B     GTOTX                                                            
GTOT10   EDIT  APFULL,(8,14(R3)),DUB=APDUB,WRK=APWORK,FLOAT=-                   
* ---- CPP                                                                      
         XR    RE,RE                                                            
         ICM   RF,15,TOTGDOL      (RF) = DOLLARS                                
         BZ    EXIT                                                             
         ICM   R1,15,TOTGPNT      (R1) = POINTS                                 
         BZ    EXIT                                                             
         MH    RF,=H'10'                                                        
         DR    RE,R1                                                            
         EDIT  (RF),(8,23(R3)),2,DUB=APDUB,WRK=APWORK,FLOAT=-                   
GTOTX    B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
ALPHATAB DC    CL42'A1A2A3A4A5A6A7B1B2B3B4B5B6B7C1C2C3C4C5C6C7'                 
         DC    CL42'D1D2D3D4D5D6D7E1E2E3E4E5E6E7F1F2F3F4F5F6F7'                 
         DC    CL42'G1G2G3G4G5G6G7H1H2H3H4H5H6H7I1I2I3I4I5I6I7'                 
         DC    CL42'J1J2J3J4J5J6J7K1K2K3K4K5K6K7L1L2L3L4L5L6L7'                 
         DC    CL42'M1M2M3M4M5M6M7N1N2N3N4N5N6N7O1O2O3O4O5O6O7'                 
         DC    CL42'P1P2P3P4P5P6P7Q1Q2Q3Q4Q5Q6Q7R1R2R3R4R5R6R7'                 
         DC    CL42'S1S2S3S4S5S6S7T1T2T3T4T5T6T7U1U2U3U4U5U6U7'                 
         DC    CL42'V1V2V3V4V5V6V7W1W2W3W4W5W6W7X1X2X3X4X5X6X7'                 
         DC    CL42'Y1Y2Y3Y4Y5Y6Y7Z1Z2Z3Z4Z5Z6Z701020304050607'                 
         DC    CL42'111213141516172122232425262731323334353637'                 
         DC    CL42'414243444546475152535455565761626364656667'                 
         DC    CL42'717273747576778182838485868791929394959697'                 
         DC    CL2'**'                                                          
         SPACE                                                                  
*===================================================*                           
* CLDUMP -CLOSES REPORT & DIES (NO DOUBT ABOUT IT )**                           
*===================================================*                           
*                                                                               
CLDUMP   L     R9,AREP                                                          
         USING REPD,R9            R9=A(REPORT WORK AREA)                        
         MVI   REPACTN,REPACLO    CLOSE THE REPORT                              
         GOTO1 VREPORT,(R9)                                                     
         DROP  R9                                                               
         DC    H'0'                                                             
         EJECT                                                                  
*==================*                                                            
* LITERAL POOL     *                                                            
*==================*                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
ROUTLIST DS    0F                  ROUTINE ADDRESS LIST                         
         DC    C'IMED    ',A(IMED)                                              
         DC    C'OMED    ',A(OMED)                                              
         DC    C'IBYR    ',A(IBYR)                                              
         DC    C'OBYR    ',A(OBYR)                                              
         DC    C'ICAMP   ',A(ICAMP)                                             
         DC    C'OCAMP   ',A(OCAMP)                                             
         DC    C'OCMP    ',A(OCMP)                                              
         DC    C'IMKT    ',A(IMKT)                                              
         DC    C'OMKT    ',A(OMKT)                                              
         DC    C'ISTA    ',A(ISTA)                                              
         DC    C'OSTA    ',A(OSTA)                                              
         DC    C'ICBLSYS ',A(ICBLSYS)                                           
         DC    C'OCBLSYS ',A(OCBLSYS)                                           
         DC    C'IDPT    ',A(IDPT)                                              
         DC    C'ILEN    ',A(ILEN)                                              
         DC    C'IWORK   ',A(IWORK)                                             
         DC    C'OWORK   ',A(OWORK)                                             
         DC    C'ISPTWK  ',A(ISPTWK)                                            
         DC    C'OSPTWK  ',A(OSPTWK)                                            
         DC    C'IDEMO   ',A(IDEMO)                                             
         DC    C'ODEMO   ',A(ODEMO)                                             
         DC    C'IOVDEMO ',A(IOVDEMO)                                           
         DC    C'OOVDEMO ',A(OOVDEMO)                                           
         DC    C'IDPTLN  ',A(IDPTLN)                                            
         DC    C'ODPTLN  ',A(ODPTLN)                                            
         DC    C'ISUBDP  ',A(ISUBDP)                                            
         DC    C'ISUB1DP ',A(ISUB1DP)                                           
         DC    C'OSUB1DP ',A(OSUB1DP)                                           
         DC    C'ISUB2DP ',A(ISUB2DP)                                           
         DC    C'OWDPTLN ',A(OWDPTLN)                                           
         DC    C'IPNTS   ',A(IPNTS)                                             
         DC    C'OPNTS   ',A(OPNTS)                                             
         DC    C'IGPNTS  ',A(IGPNTS)                                            
         DC    C'OGPNTS  ',A(OGPNTS)                                            
         DC    C'IDOLLS  ',A(IDOLLS)                                            
         DC    C'ODOLLS  ',A(ODOLLS)                                            
         DC    C'IGDOLLS ',A(IGDOLLS)                                           
         DC    C'OGDOLLS ',A(OGDOLLS)                                           
         DC    C'IGCPP   ',A(IGCPP)                                             
         DC    C'OGCPP   ',A(OGCPP)                                             
         DC    C'IBPNTS  ',A(IBPNTS)                                            
         DC    C'OBPNTS  ',A(OBPNTS)                                            
         DC    C'IBDOLLS ',A(IBDOLLS)                                           
         DC    C'OBDOLLS ',A(OBDOLLS)                                           
         DC    C'IBCPP   ',A(IBCPP)                                             
         DC    C'OBCPP   ',A(OBCPP)                                             
         DC    C'IBCPP2  ',A(IBCPP2)                                            
         DC    C'OBCPP2  ',A(OBCPP2)                                            
         DC    C'ISPOTS  ',A(ISPOTS)                                            
         DC    C'OSPOTS  ',A(OSPOTS)                                            
         DC    C'IAVGPTS ',A(IAVGPTS)                                           
         DC    C'OAVGPTS ',A(OAVGPTS)                                           
         DC    C'IBVGPTS ',A(IBVGPTS)                                           
         DC    C'OBVGPTS ',A(OBVGPTS)                                           
         DC    C'IBVGDOL ',A(IBVGDOL)                                           
         DC    C'OBVGDOL ',A(OBVGDOL)                                           
         DC    C'IPTWK   ',A(IPTWK)                                             
         DC    C'OPTWK   ',A(OPTWK)                                             
         DC    C'IWGPNT  ',A(IWGPNT)                                            
         DC    C'OWGPNT  ',A(OWGPNT)                                            
         DC    C'IWGDOL  ',A(IWGDOL)                                            
         DC    C'OWGDOL  ',A(OWGDOL)                                            
         DC    C'IWGCPP  ',A(IWGCPP)                                            
         DC    C'OWGCPP  ',A(OWGCPP)                                            
         DC    C'IWEEK   ',A(IWEEK)                                             
         DC    C'OWEEK   ',A(OWEEK)                                             
         DC    C'IWBVGPT ',A(IWBVGPT)                                           
         DC    C'OWBVGPT ',A(OWBVGPT)                                           
         DC    C'IWBVGDL ',A(IWBVGDL)                                           
         DC    C'OWBVGDL ',A(OWBVGDL)                                           
         DC    C'IDCPPM  ',A(IDCPPM)                                            
         DC    C'ODCPPM  ',A(ODCPPM)                                            
         DC    C'MDPT    ',A(MDPT)                                              
         DC    C'MDPTO   ',A(MDPTO)                                             
         DC    C'SDPT    ',A(SDPT)                                              
         DC    C'ISDPTLN ',A(ISDPTLN)                                           
         DC    C'OSDPTLN ',A(OSDPTLN)                                           
         DC    C'FCAMKT  ',A(FCAMKT)                                            
         DC    C'FSTA    ',A(FSTA)                                              
         DC    C'FDPT    ',A(FDPT)                                              
         DC    C'MKTTOT  ',A(MKTTOT)                                            
         DC    C'STATOT  ',A(STATOT)                                            
         DC    C'DPTTOT  ',A(DPTTOT)                                            
         DC    C'DPTOT   ',A(DPTOT)                                             
         DC    C'DPTOT1  ',A(DPTOT1)                                            
         DC    C'GTOT    ',A(GTOT)                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*==================*                                                            
* LITERAL POOL     *                                                            
*==================*                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================*                   
* CALCPP - RETURNS IN APFULL CPP GIVEN TPCOST(4) & TPRTG(3) *                   
*===========================================================*                   
*                                                                               
CALCPP   NTR1  BASE=*,LABEL=*                                                   
         XC    APFULL,APFULL                                                    
         ICM   R1,15,TPCOST       COST                                          
         JZ    EXIT                                                             
         ICM   RE,7,TPRTG         RATING                                        
         JZ    EXIT                                                             
         SR    R0,R0                                                            
***  2 DECIMAL                                                                  
         C     RE,=F'99999'                                                     
         BH    CALCPP05                                                         
*                                                                               
**LCPP03 TM    GLINDS,GLTOTLIN     WE ON A TOTAL LINE?                          
**       BNZ   CALCPP10             - YUP, RATING IS ALREADY ROUNDED            
****                                  ..TO 1 DECIMAL, SO X 20 IS FINE           
         TM    APWORK,X'40'        IS IT 2 DECIMAL?                             
         BNZ   CALCPP05             - YUP                                       
         TM    APROFBTS,A00TWODC   WE ON 2 DECIMALS?                            
         BZ    CALCPP10             - NOPE                                      
*****  MASTLFG IS SETUP IN LDEMHLDR SUBROUTINE                                  
         TM    MASTFLG,LDEM2DOK    WE READY FOR 2 DECIMAL?                      
         BZ    CALCPP10             - NOPE                                      
         OI    APWORK,X'40'         TURN ON THE BIT                             
CALCPP05 M     R0,=F'200'                                                       
         B     CALCPP20                                                         
***  2 DECIMAL                                                                  
CALCPP10 M     R0,=F'20'                                                        
CALCPP20 DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         ST    R1,APFULL         SAVE CPP IN APFULL                             
         J     EXIT                                                             
         EJECT                                                                  
* -- ROUNDS TO THE HUNDREDTH'S PLACE                                            
ROUNDIT  NTR1  BASE=*,LABEL=*                                                   
         SR    RF,RF                                                            
         L     RE,APFULL                                                        
         CLI   CUDMED,C'C'         IS IT CANADA?                                
         BE    ROUNDITA             - YUP                                       
         TM    APFULL,X'80'        IS IT A BIG, BIG NUMBER?                     
         BZ    ROUNDITA             - NOPE                                      
         SRDL  RE,32                                                            
         D     RE,=F'100'                                                       
         CLM   RE,1,=X'50'         IS IT 50 OR BIGGER?                          
         BL    *+8                                                              
         AHI   RF,1                                                             
         ST    RF,APFULL                                                        
         J     EXIT                                                             
*                                                                               
ROUNDITA SRDA  RE,31                                                            
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'            FOR OVERFLOW                                 
         SRA   RF,1                                                             
         ST    RF,APFULL                                                        
         J     EXIT                                                             
         SPACE 2                                                                
*-- ROUNDS TO THE TENTH PLACE                                                   
ROUNDIT2 NTR1  BASE=*,LABEL=*                                                   
         SR    RF,RF                                                            
         L     RE,APFULL                                                        
         CLI   CUDMED,C'C'         IS IT CANADA?                                
         BE    ROUNDITB             - YUP                                       
         TM    APFULL,X'80'        IS IT A BIG, BIG NUMBER?                     
         BZ    ROUNDITB             - NOPE                                      
         SRDL  RE,32                                                            
         D     RE,=F'10'                                                        
         CLM   RE,1,=X'5'          IS IT 5 OR BIGGER?                           
         BL    *+8                                                              
         AHI   RF,1                                                             
         ST    RF,APFULL                                                        
         J     EXIT                                                             
*                                                                               
ROUNDITB SRDA  RE,31                                                            
         D     RE,=F'10'                                                        
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'            FOR OVERFLOW                                 
         SRA   RF,1                                                             
         ST    RF,APFULL                                                        
         J     EXIT                                                             
         EJECT                                                                  
*===========================================================*                   
* RDCOM - ROUTINE CALLED FROM OWORK - FINDS COMMENT ELE &   *                   
*         PUTS THEM ONTO 3RD,4TH,...  PRINT LINES           *                   
*===========================================================*                   
*                                                                               
RDCOM    NTR1  BASE=*,LABEL=*                                                   
         USING BWDRECD,R9                                                       
         SR    R0,R0                                                            
         LA    R1,BWDEL                                                         
RDCOM10  CLI   0(R1),0                                                          
         BE    RDCOMX                                                           
         CLI   0(R1),COMELCDQ     COMMENT ELEMENT                               
         BE    RDCOM20                                                          
RDCOM12  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     RDCOM10                                                          
*                                                                               
         USING COMEL,R1                                                         
RDCOM20  ZIC   RE,1(R1)                                                         
         SH    RE,=Y(COMCOM-COMEL)                                              
         LA    RF,COMCOM-1                                                      
         CLI   COMNUM,1                                                         
         BL    RDCOM22                                                          
         CLI   COMNUM,5                                                         
         BH    RDCOM22                                                          
         BCTR  RE,0                                                             
         LA    RF,1(RF)                                                         
*                                                                               
RDCOM22  CH    RE,=H'116'         IF OVER 117 CHARS LONG                        
         BNH   *+8                ONLY PRINT 117                                
         LA    RE,116                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RF)      R3 POINTS TO PRINT LINE                       
         CLI   ASONOFF,ASOFF                                                    
         BE    *+12                                                             
         LA    R3,DRLWIDTH(R3)    ONLINE - DROOL                                
         B     RDCOM12                                                          
         LA    R3,DRVWIDTH(R3)    OFFLINE - DRIVER                              
         B     RDCOM12                                                          
*                                                                               
RDCOMX   J     EXIT                                                             
         DROP  R1,R9                                                            
         EJECT                                                                  
*=========================================================*                     
* LDEMHLDR - SAVES THE SPOT FOR THE CURRENT DEMO CATEGORY *                     
*=========================================================*                     
LDEMHLDR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,ALDEMHLD                                                      
         J     LDEMSTRT                                                         
*=======================================================*                       
* LDEMHLD2 - ALWAYS LOOKS AT THE PRIMARY DEMO CATEGORY  *                       
*=======================================================*                       
LDEMHLD2 NTR1  BASE=*,LABEL=*                                                   
         LA    RE,LDEMHLD                                                       
*                                                                               
LDEMSTRT CLI   0(RE),C'R'          RATING?                                      
         JE    LDEMHDYS             - YUP                                       
         CLI   0(RE),C'E'          E-RATING?                                    
         JNE   LDEMHDNO             - IT'S IMPRESSION                           
*                                                                               
LDEMHDYS OI    MASTFLG,LDEM2DOK    2 DECIMALS OK!                               
         J     LDEMHDX                                                          
*                                                                               
LDEMHDNO NI    MASTFLG,X'FF'-LDEM2DOK MAKE SURE FLAG IS OFF                     
LDEMHDX  J     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
*======================================*                                        
* SPFREQ  - LOOKS AT SPOTABLE AND SEES *                                        
* WHICH SPOT APPEARED THE MOST - WHICH *                                        
* GETS RETURNED IN APBYTE              *                                        
*======================================*                                        
*                                                                               
SPFREQ   NTR1  BASE=*,LABEL=*                                                   
         LA    RF,SPFREQTB                                                      
         XC    0(30,RF),0(RF)                                                   
         MVI   0(RF),X'FF'        MARK END OF TABLE                             
         LA    RE,SPOTABLE                                                      
         LA    R0,14              # OF WEEKS MAX                                
FRQ10    CLI   0(RE),0                                                          
         BE    FRQ20                                                            
FRQ12    CLI   0(RF),X'FF'        ANYTHING IN TABLE?                            
         BNE   *+14                                                             
         MVC   0(1,RF),0(RE)      ADD NUMBER TO FREQUENCY TABLE                 
         B     FRQ15                                                            
*                                                                               
         CLC   0(1,RE),0(RF)      SEARCH FOR CURRENT ENTRY                      
         BE    *+12                                                             
         LA    RF,L'SPFREQTB(RF)  TRY NEXT ENTRY IN FREQUENCY TABLE             
         B     FRQ12                                                            
*                                                                               
FRQ15    SR    R0,R0                                                            
         IC    R0,1(RF)           INCREMENT FREQUENCY                           
         LA    R0,1(R0)                                                         
         STC   R0,1(RF)                                                         
         LA    RF,L'SPFREQTB(RF)                                                
         MVI   0(RF),X'FF'        MARK NEW END                                  
*                                                                               
FRQ20    LA    RE,1(RE)           INCREMENT SPOT # TABLE                        
         BCT   R0,FRQ10                                                         
         SPACE                                                                  
* #'S AND FREQUENCIES IN SPTPERWK - CHUGG THROUGH                               
* TO FIGURE OUT WHICH HAS THE HIGHEST FREQUENCY                                 
         SPACE                                                                  
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         MVI   APBYTE,0                                                         
*                                                                               
         LA    RF,SPFREQTB                                                      
         CLI   0(RF),X'FF'                                                      
         BE    FRQ30                                                            
         IC    RE,0(RF)           THE #                                         
         IC    R0,1(RF)           FIRST FREQUENCY                               
FRQ25    LA    RF,L'SPFREQTB(RF)                                                
         CLI   0(RF),X'FF'        END OF TABLE                                  
         BE    FRQ30                                                            
*                                                                               
         IC    R1,1(RF)           SECOND FREQUENCY                              
         CR    R0,R1                                                            
         BH    FRQ25                                                            
         IC    RE,0(RF)           REPLACE # - THIS ONES HIGHER                  
         IC    R0,1(RF)           REPLACE FREQUENCY                             
         B     FRQ25                                                            
*                                                                               
FRQ30    STC   RE,APBYTE                                                        
FRQX     J     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
*===============================================================                
* GETBEG - FINDS THE BEGINNING DAY OF THE WEEK THAT APDUB IS IN*                
*===============================================================                
*                                                                               
GETBEG   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VGETDAY,APPARM,APDUB,APFULL                                      
         ZIC   R1,APPARM                                                        
         ZIC   R0,ESTOWSDY                                                      
         CH    R0,=H'2'                                                         
         BNL   GETBEG10                                                         
         BCTR  R1,0                                                             
         LNR   R2,R1                                                            
         B     GETBEG20                                                         
*                                                                               
GETBEG10 SR    R1,R0                                                            
         BZ    GETBEGX                                                          
         BP    *+8                                                              
         LA    R1,7(R1)                                                         
         LNR   R2,R1                                                            
GETBEG20 GOTO1 VADDAY,APPARM,APDUB,APDUB,(R2)                                   
GETBEGX  J     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
*=======================================================*                       
* DATELIM - FINDS WEEKS WHERE FIRST & LAST SPOTS APPEAR *                       
* VALUES RETURNED SPER & EPER                           *                       
*=======================================================*                       
*                                                                               
DATELIM  NTR1  BASE=*,LABEL=*                                                   
         XC    SPER,SPER          START PERIOD DATE                             
         XC    EPER,EPER          END PERIOD DATE                               
         L     R8,ATWA            PACKED DATES                                  
         AHI   R8,CMPDATSP-TWAD                                                 
         LA    RE,53              LENGTH                                        
         LA    R1,SPOTABLE                                                      
DATE25   CLI   0(R1),0            ANY SPOT FOR THIS WEEK?                       
         BE    DATE40                                                           
         MVC   EPER,2(R8)         OVERWRITE END DATE TILL LAST ONE              
         OC    SPER,SPER          ONLY WANT FIRST START DATE                    
         BNZ   DATE40                                                           
         MVC   SPER,0(R8)                                                       
DATE40   LA    R1,1(R1)                                                         
         LA    R8,4(R8)                                                         
         BCT   RE,DATE25                                                        
DATEX    J     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
***********************************************************************         
* ROUTINE FOR IWEEK MOVED BECAUSE OF ADDRESSABILITY ERROR                       
***********************************************************************         
INPTWEEK NTR1  BASE=*,LABEL=*                                                   
         L     R1,ATWA            PACKED DATES                                  
         AHI   R1,CMPDATSP-TWAD                                                 
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,WEEKNO                                                      
         BNZ   IWEEK10                                                          
         L     RE,ATWA             FOR FIRST WEEK USE DISPLAY DATE              
         AHI   RE,CMPDATSD-TWAD                                                 
         ST    RE,APPARM                                                        
         MVI   APPARM,0                                                         
         GOTO1 VDATCON,APPARM,,(2,0(R2))                                        
         B     IWEEK20                                                          
*                                                                               
IWEEK10  MHI   RE,4                                                             
         AR    R1,RE                                                            
         MVC   0(2,R2),0(R1)                                                    
*                                                                               
IWEEK20  CLI   INOFRM,C'W'         WEEKLY IF DAILY CAMPAIGN                     
         BNE   IWEEK40                                                          
         GOTO1 VDATCON,APPARM,(2,0(R2)),(0,APWORK)                              
         GOTO1 VGETDAY,APPARM,APWORK,APWORK+6                                   
         CLC   APWORK+6(3),=C'   ' GETDAY SHOULD BE VALID                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   APPARM+1(1),ESTOWSDY                                             
         CLI   ESTOWSDY,0                                                       
         BNE   *+8                                                              
         MVI   APPARM+1,1                                                       
*                                                                               
         CLC   APPARM(1),APPARM+1  SAME START DAY?                              
         BE    IWEEKX              YES, WE'RE ALIGNED WITH A WEEK               
*                                                                               
         ZIC   RE,APPARM           DAY NUMBER OF THIS SPOT                      
         ZIC   RF,APPARM+1         WEEKS ALIGNED TO THIS DAY #                  
         SR    RE,RF                                                            
         BNM   IWEEK30                                                          
         AHI   RE,7                                                             
*                                                                               
IWEEK30  LNR   RE,RE                                                            
         ST    RE,APPARM+8                                                      
         GOTO1 VADDAY,APPARM,APWORK,APWORK+6,,0                                 
         L     RE,ATWA             FOR FIRST WEEK USE DISPLAY DATE              
         AHI   RE,CMPDATSD-TWAD                                                 
         CLC   APWORK+6(6),0(RE)   IF ROLLBACK DATE IS LESS CAMP START          
         BNL   *+10                                                             
         MVC   APWORK+6(6),0(RE)   THEN SET IT TO CAMP START                    
         GOTO1 VDATCON,APPARM,(0,APWORK+6),(2,0(R2))                            
         B     IWEEKX                                                           
*                                                                               
IWEEK40  CLI   DAILY,C'Y'                                                       
         BNE   IWEEKX                                                           
         GOTO1 VDATCON,APPARM,(2,0(R2)),(0,APWORK)                              
         ZIC   R8,DYSADD                                                        
         GOTO1 VADDAY,APPARM,APWORK,APWORK+6,(R8)                               
         GOTO1 VDATCON,APPARM,(0,APWORK+6),(2,0(R2))                            
IWEEKX   J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE FOR IWEEK MOVED BECAUSE OF ADDRESSABILITY ERROR                       
***********************************************************************         
OUPTWEEK NTR1  BASE=*,LABEL=*                                                   
         NI    MISCFLG1,X'FF'-MF1DONTP                                          
         CLI   CMPNWKS,14          CAMPAIGN HAS MORE THAN 14 WEEKS?             
         BNH   OWEEK9                                                           
         OC    INOFRM,INOFRM       ANY FORMAT?                                  
         BNZ   OWEEK9                                                           
         OC    INOSTDTE,INOSTDTE   ANY STDATE OPTION?                           
         BNZ   OWEEK9              NORMAL PROCESS                               
*                                                                               
         CLC   WEEKNO,RCPDSTDT     CURRENT WEEK >= DISPLACEMENT INTO?           
         BNL   OWEEK9              YES, OKAY TO PRINT                           
         OI    MISCFLG1,MF1DONTP   NO, DON'T PRINT                              
         B     OWEEKX                                                           
*                                                                               
OWEEK9   GOTO1 VDATCON,APPARM,(2,(R2)),(4,0(R3))                                
OWEEKX   ZIC   RF,WEEKNO                                                        
         AHI   RF,1                                                             
         STC   RF,WEEKNO                                                        
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* -- SETS UP WORK ENTRY FOR OPTION=DPT                                          
* -- BOTH (INCLUDING STA AND EXCLUDING STA)                                     
*                                                                               
***********************************************************************         
SETWORK  NTR1  BASE=*,LABEL=*                                                   
         USING BWDRECD,R9                                                       
         USING HEADRD,R2                                                        
         MVC   OPTFSEQ,BWDKELPO                                                 
         MVC   OPTSTA,BWDSTA      MOVE STATION                                  
         MVC   OPTDP,BWDDPT       MOVE IN DAYPART                               
         MVC   OPTLEN,BWDSLN      MOVE IN SPOT LENGTH                           
         MVI   OPTFDAY,0          SET PROPER SORTING SEQUENCE FOR DAYS          
         CLI   BWDDAYS,X'7C'       M-F                                          
         BE    SETW4                                                            
         MVI   OPTFDAY,1                                                        
         CLI   BWDDAYS,X'7F'       M-SU                                         
         BE    SETW4                                                            
         SR    RF,RF               START MONDAY AS 2                            
         ZIC   R0,BWDDAYS                                                       
         SRDL  R0,9                                                             
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   RF,*-10                                                          
         LPR   RF,RF                                                            
         STC   RF,OPTFDAY                                                       
*                                                                               
SETW4    MVC   OPTTIM,BWDKELTM    PACKED TIMES                                  
         MVC   OPTSEQ1,BWDKELSQ   SEQ #                                         
         CLI   COSTIND,2          MOVE IN APPROPRIATE COST                      
         BNE   SETW5                                                            
         MVC   OPTCOST,BWDCOST2                                                 
         MVI   OPTCIND,2                                                        
         B     SETW10                                                           
SETW5    CLI   COSTIND,3          COST3                                         
         BNE   SETW7                                                            
         MVC   OPTCOST,BWDCOST3                                                 
         MVI   OPTCIND,3                                                        
         B     SETW10                                                           
SETW7    MVC   OPTCOST,BWDCOST1  SO MUST BE COST1                               
         MVI   OPTCIND,1                                                        
*                               -----INFO FOR REREADING RECORD----              
SETW10   MVC   OPTDAY,BWDKELDY    DAYS                                          
         MVC   OPTSEQ2,BWDKSEQ    SEQUENCE NUMBER                               
         MVC   OPTSTCD,BWDKELST   STATION CODE                                  
         MVC   OPTPO,BWDKELPO     PACKAGE/ORBIT #                               
*                               -----CHECK STATION INCLUDED?------              
         CLI   GLARGS,C'N'        INCLUDE STATION HERE                          
         BNE   SETWX              YES                                           
         MVC   TEMPBLK(1),OPTFSEQ    MOVE SEQUENCE NUMBER                       
         MVC   TEMPBLK+1(L'NOPTLN-1),OPTDP   MOVE THE REST(EXCL. STA)           
         XC    0(L'OPTLN,R2),0(R2)                                              
         MVC   0(L'NOPTLN,R2),TEMPBLK                                           
SETWX    J     EXIT                                                             
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSWRK                                                       
         EJECT                                                                  
         PRINT ON                                                               
LOCALD   DSECT                                                                  
       ++INCLUDE SPNWSDRVD                                                      
         ORG   LOCALD+LDRVRLOC                                                  
*                                                                               
MYPOSO   DS    0CL3               SAVED PRINT POSITIONS                         
MYLTYP   DS    CL1                LINE TYPE                                     
MYLINE   DS    XL1                LINE NUMBERR                                  
MYCOL    DS    XL1                COLUMN NUMBER                                 
MYOLEN   DS    XL1                SAVED OUTPUT LENGTH                           
MISCFLG1 DS    XL1                                                              
MF1DONTP EQU   X'80'              DON'T PRINT THIS LINE                         
*                                                                               
ZSTA     DS    XL1                FLAG 9'S STATION DON'T PRINT                  
***  DOESN'T SEEM LIKE THIS FLAG IS USED SO I'LL USE IT                         
MASTFLG  DS    XL1                                                              
LDEM2DOK EQU   X'80'              FIRST TIME IN LDEMHLDR?                       
***                                  MHC  02/08/05                              
OVRIDE   DS    XL1                SAVED OVERRIDE FLAG                           
SVMDPT   DS    CL1                MASTER DAYPART                                
         DS    0F                                                               
SPER     DS    XL2                START DATE (BUY PERIOD PROGRAMMING)           
EPER     DS    XL2                END DATE (BUY PERIOD PROGRAMMING)             
DTLCST   DS    XL2                DETAIL SPOTS PER WEEK TOTAL                   
STANUM   DS    XL2                NUMBER OF STATION TLCSTS                      
MKTNUM   DS    XL2                NUMBER OF MARKET TLCSTS                       
DPTNUM   DS    XL2                NUMBER OF DAYPART TLCSTS                      
MQDPT    DS    CL3                SAVE MASTER DAYPART ON OUTPUT                 
TPRTG    DS    XL3                TEMPORARY RATING                              
TPCOST   DS    XL4                TEMPORARY COST                                
MASTRAT  DS    XL4                MASTER RATE SAVED VALUE                       
MASTCOST DS    XL4                MASTER COST SAVED VALUE                       
STACSTS  DS    XL4                STATION COST ON STATION TOTAL LINE            
MKTCSTS  DS    XL4                MARKET COST ON TOTAL LINE                     
DPTCSTS  DS    XL4                DAYPART COST ON TOTAL LINE                    
SVP2     DS    XL4                ADDRESS OF PRINT LINE 2                       
SVPDOL   DS    XL4                                                              
SVPPNT   DS    XL4                                                              
SVTPNT   DS    XL4                                                              
SVTDOL   DS    XL4                                                              
OVERS    DS    XL6                OVERRIDES                                     
SVIOKEY  DS    CL13               SAVE KEY                                      
TEMPBLK  DS    CL14               TEMP STORAGE FOR IWORK/OWORK ROUTINE          
STADEMOS DS    CL24               STATION TOTAL FOR RTGSXSPOTS                  
DPTDEMOS DS    CL24               DAYPART TOTAL FOR RTGSXSPOTS                  
SPFREQTB DS    15CL2              SPOT FREQUENCY TABLE                          
*                                                                               
OUTAREA  DS    0CL65              OUTPUT AREAS                                  
LABLAREA DS    CL15               LABEL AREA                                    
         DS    CL1                                                              
CODENNAM DS    0CL49                                                            
CODEAREA DS    CL12               CODE                                          
         DS    CL1                                                              
NAMEAREA DS    CL36               NAME                                          
*                                                                               
         DS    0D                                                               
ADALPHA  DS    A                  ADDRESS OF ALPHA TABLE                        
ADPTDEMO DS    A                  ADDRESS OF DAYPART DEMO TOTALS                
ASOVERS  DS    A                  ADDRESS OF DAYPART DEMO TOTALS                
ALDEMHLD DS    A                  ADDRESS OF CURRENT LDEMHLD POSITION           
         ORG   LOCALD+4096                                                      
*                                                                               
LOCALX   EQU *                                                                  
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         SPACE 3                                                                
       ++INCLUDE DRIVETABLE                                                     
         SPACE 3                                                                
       ++INCLUDE DROOLTABLE                                                     
         SPACE 3                                                                
*                                                                               
HEADRD   DSECT                                                                  
HDRLN    DS    0CL23                                                            
HDRFSEQ  DS    CL1                0=NONPKG,ELSE BWDKELSQ                        
HDRSTA   DS    CL8                STATION                                       
HDRFDAY  DS    CL1                FUDGED DAY FOR SORTING                        
HDRTIM   DS    CL2                PACKED TIMES                                  
         ORG   HDRSTA                                                           
DTHDRFDY DS    CL1                THIS IS ONLY FOR DAY/TIME                     
DTHDRTIM DS    CL2                RANK ACROSS STATIONS                          
DTHDRSTA DS    CL8                                                              
*                                                                               
HDRSEQ1  DS    CL1                SEQUENCE NUMBER                               
HDRCOST  DS    CL4                COST                                          
HDRCIND  DS    CL1                COST INDICATOR                                
HDRDAY   DS    CL1                REAL DAY                                      
HDRSEQ2  DS    CL2                SEQUENCE NUMBER                               
HDRSTCD  DS    CL1                STATION CODE                                  
HDRPO    DS    CL1                PACKAGE/ORBIT NUMBER                          
*                                                                               
         ORG   HDRLN                                                            
NHDRLN   DS    0CL15              FOR GROUPING BY STATION                       
NHDRFSEQ DS    CL1                0=NONPKG,ELSE BWDKELSQ                        
NHDRFDAY DS    CL1                FUDGED DAY FOR SORTING                        
NHDRTIM  DS    CL2                PACKED TIMES                                  
NHDRSEQ1 DS    CL1                SEQUENCE NUMBER                               
NHDRCOST DS    CL4                COST                                          
NHDRCIND DS    CL1                COST INDICATOR                                
NHDRDAY  DS    CL1                REAL DAY                                      
NHDRSEQ2 DS    CL2                SEQUENCE NUMBER                               
NHDRSTCD DS    CL1                STATION CODE                                  
NHDRPO   DS    CL1                PACKAGE/ORBIT NUMBER                          
*                                 IF OPTION IS DPT                              
         ORG   HDRLN                                                            
OPTLN    DS    0CL25              FOR GROUPING WITHOUT STATION                  
OPTFSEQ  DS    CL1                0=NONPKG,ELSE BWDKELSQ                        
OPTSTA   DS    CL8                STATION                                       
OPTDP    DS    CL1                DAYPART                                       
OPTLEN   DS    CL1                SPOT LENGTH                                   
OPTFDAY  DS    CL1                FUDGED DAY FOR SORTING                        
OPTTIM   DS    CL2                PACKED TIMES                                  
OPTSEQ1  DS    CL1                SEQUENCE NUMBER                               
OPTCOST  DS    CL4                COST                                          
OPTCIND  DS    CL1                COST INDICATOR                                
OPTDAY   DS    CL1                REAL DAY                                      
OPTSEQ2  DS    CL2                SEQUENCE NUMBER                               
OPTSTCD  DS    CL1                STATION CODE                                  
OPTPO    DS    CL1                PACKAGE/ORBIT NUMBER                          
         ORG   HDRLN                                                            
NOPTLN   DS    0CL17              FOR GROUPING BY STATION                       
NOPTFSEQ DS    CL1                0=NONPKG,ELSE BWDKELSQ                        
NOPTDPT  DS    CL1                DAYPART                                       
NOPTLEN  DS    CL1                SPOT LENGTH                                   
NOPTFDAY DS    CL1                FUDGED DAY FOR SORTING                        
NOPTTIM  DS    CL2                PACKED TIMES                                  
NOPTSEQ1 DS    CL1                SEQUENCE NUMBER                               
NOPTCOST DS    CL4                COST                                          
NOPTCIND DS    CL1                COST INDICATOR                                
NOPTDAY  DS    CL1                REAL DAY                                      
NOPTSEQ2 DS    CL2                SEQUENCE NUMBER                               
NOPTSTCD DS    CL1                STATION CODE                                  
NOPTPO   DS    CL1                PACKAGE/ORBIT NUMBER                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060SPNWS30   03/05/07'                                      
         END                                                                    
