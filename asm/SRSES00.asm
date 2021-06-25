*          DATA SET SRSES00    AT LEVEL 020 AS OF 01/05/21                      
*PHASE T14900A                                                                  
*INCLUDE FATWASVR                                                               
         TITLE '$SES - SET SESSION / SHOW SESSION INFO '                        
         PRINT NOGEN                                                            
SRSES    CSECT                                                                  
         NMOD1 WORKX-WORKD,**$SES**,RA,R9,RR=R4,CLEAR=YES                       
         USING WORKD,RC                                                         
         ST    RD,SAVERD           SAVE BASE RD                                 
         ST    R4,RELO                                                          
         MVI   STOLEN,X'FF'                                                     
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         L     R8,SRPARM1                                                       
         USING SYSFACD,R8          R8=A(SYSFAC LIST)                            
         L     R3,SRPARM6                                                       
         USING SRSESFFD,R3         R3=A(TWA)                                    
         ST    R3,ATWA                                                          
         L     R5,SRPARM3          R5=A(UTL ENTRY)                              
         USING UTLD,R5                                                          
*                                                                               
         MVC   ATIA,SRPARM2        SET A(TIA)                                   
         MVC   ATIOB,SRPARM8       SET A(TIOB)                                  
*                                                                               
         L     R6,VSSB             EXTRACT SSB INFO                             
*                                                                               
         MVC   SSMAX,=AL2(4)       OLD STYLE FIXES AT 4 SESSIONS                
         MVC   SSMXP,=AL2(5)                                                    
         TM    TSTATC,TSTCXSES                                                  
         BZ    *+16                                                             
         MVC   SSMAX,SSBSSMAX-SSBD(R6)                                          
         MVC   SSMXP,SSBSSMXP-SSBD(R6)                                          
*                                                                               
         MVC   SSDATE,SSBSDATE-SSBD(R6)                                         
         MVC   SSPGS,SSBSSPGS-SSBD(R6)                                          
         MVC   RECLEN,SSBTWAL-SSBD(R6)                                          
         MVC   ALANG,SSBALANG-SSBD(R6)                                          
         MVC   ACTRY,SSBACTRY-SSBD(R6)                                          
         MVC   AFID,SSBAFID-SSBD(R6)                                            
         MVC   SYNAME,SSBSYSN4-SSBD(R6)                                         
         MVC   LOCORE,SSBLOADR-SSBD(R6)                                         
         MVC   CHKDSP,=Y(CHKPTDSP)                                              
         L     R6,SSBTKADR-SSBD(R6)                                             
         USING TCBD,R6             R6=A(TCB)                                    
*                                                                               
         L     RE,SRPARM4          GET COMFACS ROUTINES                         
         USING COMFACSD,RE                                                      
         MVC   AGETTXT,CGETTXT                                                  
         MVC   ADATCON,CDATCON                                                  
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ATWASVR,VTWASVR     SET A(TWASVR) FROM SYSFACS                   
         ICM   R1,15,=V(TWASVR)                                                 
         BZ    *+10                                                             
         AR    R1,R4                                                            
         ST    R1,ATWASVR          TWASVR INCLUDED IN LINK                      
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
*TEST FOR WHICH MODE AND INITIALISE AS REQUIRED                       *         
*=SS AND =SS,X,INF COMMANDS REQUIRE EXTENDED DISPLAY                  *         
***********************************************************************         
         SPACE 1                                                                
INI001   CLC   SRVID+1(2),=C'SS'   TEST OLD STLYE COMMAND                       
         BNE   SS                                                               
         CLI   SRVID+3,C','                                                     
         BNE   INI002                                                           
         CLC   SRVID+5(4),=C',INF' TEST FOR CHKPT INFO REQUIRED                 
         BNE   SS                                                               
*                                                                               
INI002   SR    RE,RE               SET SOFT DISPS FOR EXTENDED DISPLAY          
         LA    RF,20                                                            
         SR    R0,R0                                                            
         ICM   R0,3,SSMAX                                                       
         BZ    INI003                                                           
         DR    RE,R0                                                            
         MH    RF,=H'80'                                                        
         STH   RF,CURDSP                                                        
         SH    RF,=H'80'                                                        
         STH   RF,TWADSP                                                        
*                                                                               
INI003   L     R1,=A(CTREC-WORKD)  OUT OF RANGE WORK                            
         AR    R1,RC                                                            
         ST    R1,ACTREC                                                        
         L     R1,=A(SAVECHK1-WORKD)                                            
         AR    R1,RC                                                            
         ST    R1,ASAVE1                                                        
         L     R1,=A(SAVECHK2-WORKD)                                            
         AR    R1,RC                                                            
         ST    R1,ASAVE2                                                        
         L     R1,=A(SAVECHK3-WORKD)                                            
         AR    R1,RC                                                            
         ST    R1,ASAVE3                                                        
         L     R1,=A(SAVECHK4-WORKD)                                            
         AR    R1,RC                                                            
         ST    R1,ASAVE4                                                        
         XC    ASAVEX,ASAVEX                                                    
*                                                                               
         L     RF,ATIOB                                                         
         MVC   PFKEY,TIOBAID-TIOBD(RF)                                          
         MVC   CURADR,TIOBCURS-TIOBD(RF)                                        
*                                                                               
         MVI   BYTE,0              SET SESSION TO 0                             
         MVI   ACTN,C'T'           SET ACTION TO TWA                            
         LA    RF,SRVID                                                         
         CLI   3(RF),C','          TEST FOR =SS BY ITSELF                       
         BNE   INI030                                                           
*                                                                               
INI010   CLI   4(RF),C'A'          MUST BE A-H                                  
         BL    INI020                                                           
         CLI   4(RF),C'H'                                                       
         BH    INI020                                                           
         SR    R1,R1               GET SESSION NUMBER                           
         IC    R1,4(RF)                                                         
         SH    R1,=H'0193'                                                      
         STC   R1,BYTE                                                          
         MVI   ACTN,C'D'           ACTION DISP                                  
         B     INI030                                                           
*                                                                               
INI020   CLI   4(RF),C'X'          TEST FOR X                                   
         BNE   INI030                                                           
         MVI   ACTN,C'X'                                                        
         B     INI030                                                           
*                                                                               
INI030   MVI   INFO,2                                                           
         BAS   RE,INIT                                                          
         BAS   RE,MAIN             GOTO MAIN PROGRAM                            
         B     INFOX                                                            
*                                                                               
XIT1     XIT1  ,                                                                
*                                                                               
XMOD1    L     RD,SAVERD           GO BACK TO BASE                              
         XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*CODE TO SUPPORT NEW TEMPSTR STYLE SERVICE FUNCTIONS                  *         
***********************************************************************         
         SPACE 1                                                                
SS       ST    R8,DMCB             SET A(SYSFACS) IN PARAM LIST                 
         XC    MSG,MSG                                                          
         MVC   SRFLD,SRVID         SAVE SERVICE REQUEST FIELD                   
         MVC   SAVESESS,TSESSION   SAVE CURRENT SESSION NUMBER                  
         SR    RE,RE                                                            
         IC    RE,TSSRSRV                                                       
         SRDL  RE,4                                                             
         SRL   RF,28                                                            
         STC   RE,TRANSESS         SAVE TRANSFER SESSION                        
         STC   RF,RSRVSESS         SAVE RESERVED SESSION                        
         SR    RE,RE                                                            
         IC    RE,TSSSWAP                                                       
         SRDL  RE,4                                                             
         SRL   RF,28                                                            
         STC   RE,PREVSESS         SAVE PREVIOUS SESSION                        
         STC   RF,LASTSESS         SAVE LAST SESSION                            
*                                                                               
SS1      MVI   ACTN,0              INIT ACTION AND SESSION                      
         MVI   SESS,X'0F'                                                       
         MVC   STEREO,TSTAT6       SAVE INITIAL STEREO SETTINGS                 
         MVC   STEREO8,TSTAT8                                                   
         NI    STEREO,TST6STRO+TST6STFU+TST6STSS                                
         NI    STEREO8,TST8STSS+TST8BINT                                        
         TM    TSTAT6,TST6STRO                                                  
         BZ    *+8                                                              
         OI    ACTN,X'80'          SET FLAG#1 ACTIVE BEFORE ACTION              
         TM    TSTAT6,TST6STFU                                                  
         BZ    *+8                                                              
         OI    ACTN,X'20'          SET FLAG#2 ACTIVE BEFORE ACTION              
         TM    TSTAT6,TST6STSS                                                  
         BZ    *+8                                                              
         OI    ACTN,X'08'          SET FLAG#3 ACTIVE BEFORE ACTION              
         TM    TSTAT8,TST8STSS                                                  
         BZ    *+8                                                              
         OI    ACTN,X'08'          SET FLAG#3 ACTIVE BEFORE ACTION              
*                                                                               
SS2      CLC   SRFLD+1(2),=C'SS'   TEST GENERIC SYNTAX                          
         BE    SSS                                                              
         SR    R1,R1                                                            
         LA    RE,SRSYNTAB                                                      
SS2A     ICM   R1,1,0(RE)          SEARCH SYNONYM TABLE                         
         BZ    SSERR10                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),SRFLD+1                                                  
         BE    SS2B                                                             
         LA    RE,L'SRSYNTAB(RE)                                                
         B     SS2A                                                             
SS2B     XC    SRFLD+5(12),SRFLD+5 REPLACE BY FORMAL SYNTAX                     
         MVC   SRFLD+1(4),4(RE)                                                 
         ICM   RF,15,8(RE)                                                      
         BZ    SSS                                                              
         A     RF,RELO                                                          
         BR    RF                  GO DIRECT TO VALIDATION ROUTINE              
*                                                                               
SSS      CLI   SRFLD+3,C','        =SS,S,XXX,S=ABC                              
         BNE   SSERR10                                                          
*                                                                               
SSSA     CLI   SRFLD+4,C'A'        SESSION A THRU H                             
         BL    SSSL                                                             
         CLI   SRFLD+4,C'H'                                                     
         BH    SSSL                                                             
*                                                                               
SSSA1    CLC   SRFLD+5(3),=C',XCT' XCTL RETURN FROM SEPARATE SESSION            
         BNE   SSSA2                                                            
         OI    XCTL,X'01'          SET FLAG TO SHOW =SS,W,XCT                   
         CLI   SRFLD+8,C'S'                                                     
         BNE   *+8                                                              
         OI    XCTL,X'02'          SET FLAG TO SHOW =SS,X,XCS                   
*                                                                               
         CLC   SSMAX,SSMXP         TEST IF EXTRA SESSION DEFINED                
         BE    SSSA2               NO                                           
         LH    R1,SSMXP            TEST IF RETURNING FROM EXTRA SESS            
         BCTR  R1,0                                                             
         CLM   R1,1,SAVESESS                                                    
         BNE   *+8                                                              
         OI    XCTL,X'80'          SET RETURNING FROM EXTRA SESS                
*                                                                               
SSSA2    MVC   SESS,SRFLD+4                                                     
         NI    SESS,X'0F'                                                       
         IC    R1,SESS                                                          
         BCTR  R1,0                                                             
         STC   R1,SESS                                                          
         B     SSSVAL                                                           
*                                                                               
SSSL     CLI   SRFLD+4,C'L'        LAST SESSION                                 
         BNE   SSSMNS                                                           
SSSL0    MVC   SESS,LASTSESS                                                    
         CLC   SESS,SSMAX+1                                                     
         BNL   SSERR5                                                           
         B     SSSVAL                                                           
*                                                                               
SSSMNS   CLI   SRFLD+4,C'-'        THIS SESSION -1                              
         BNE   SSSPLS                                                           
SSSMNS0  ZIC   R1,TSESSION                                                      
         LH    R0,SSMAX                                                         
SSSMNS1  SH    R1,=H'1'                                                         
         BNM   *+10                                                             
         LH    R1,SSMAX                                                         
         BCTR  R1,0                                                             
         STC   R1,SESS                                                          
         CLC   SESS,RSRVSESS       SKIP PAST RESERVED SESSION                   
         BNE   SSSVAL                                                           
         BCT   R0,SSSMNS1                                                       
         MVI   SESS,X'0F'                                                       
         B     SSSVAL                                                           
*                                                                               
SSSPLS   CLI   SRFLD+4,C'+'        THIS SESSION +1                              
         BNE   SSSP                                                             
SSSPLS0  ZIC   R1,TSESSION                                                      
         LH    R0,SSMAX                                                         
SSSPLS1  LA    R1,1(R1)                                                         
         CH    R1,SSMAX                                                         
         BL    *+6                                                              
         SR    R1,R1                                                            
         STC   R1,SESS                                                          
         CLC   SESS,RSRVSESS       SKIP PAST RESERVED SESSION                   
         BNE   SSSVAL                                                           
         BCT   R0,SSSPLS1                                                       
         MVI   SESS,X'0F'                                                       
         B     SSSVAL                                                           
*                                                                               
SSSP     CLI   SRFLD+4,C'P'        PREVIOUS SESSION                             
         BNE   SSSR                                                             
SSSP0    MVC   SESS,PREVSESS                                                    
         CLC   SESS,SSMAX+1                                                     
         BNL   SSERR6                                                           
         B     SSSVAL                                                           
*                                                                               
SSSR     CLI   SRFLD+4,C'R'        RESERVED SESSION                             
         BNE   SSST                                                             
SSSR0    MVC   SESS,RSRVSESS                                                    
         CLC   SESS,SSMAX+1                                                     
         BNL   SSERR7                                                           
         B     SSSVAL                                                           
*                                                                               
SSST     CLI   SRFLD+4,C'T'        TRANSFER SESSION                             
         BNE   SSSV                                                             
SSST0    MVC   SESS,TRANSESS                                                    
         CLC   SESS,SSMAX+1                                                     
         BNL   SSERR9                                                           
         B     SSSVAL                                                           
*                                                                               
SSSV     CLI   SRFLD+4,C'V'        AVAILABLE/UNCONDITIONAL SESSION              
         BE    *+12                                                             
         CLI   SRFLD+4,C'U'                                                     
         BNE   SSSW                                                             
*                                                                               
SSSV0    TM    ACTN,X'FE'          TEST STEREO ISSUES =SV                       
         BZ    SSSV1                                                            
         TM    STEREO,TST6STFU     AND THAT 2ND STEREO FLAG IS ON               
         BZ    SSSV1                                                            
         CLC   SRVID+1(2),=C'SV'   MAP TO =SS,*                                 
         BE    SSSNOP0                                                          
*                                                                               
SSSV1    MVC   NUM1SESS(4),=4X'FF' INITIALISE SESSION CHOOSE LIST               
         LH    R0,SSMAX                                                         
         SR    R1,R1               R1=NEW SESSION NUMBER                        
         IC    R1,SAVESESS                                                      
SSSV2    IC    RF,SSACBITS(R1)                                                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    TSSBITS,0           TEST SESSION AVAILABLE                       
         BO    SSSV3                                                            
         CLM   R1,1,RSRVSESS       TEST AVAILABLE SESSION RESERVED              
         BE    SSSV4                                                            
         STC   R1,SESS             ALWAYS USE FIRST AVAILABLE SESSION           
         B     SSSVAL                                                           
*                                                                               
SSSV3    EX    RF,*+8              TEST IF CANT USE SESSION                     
         B     *+8                                                              
         TM    TSSXBITS,0                                                       
         BO    SSSV4                                                            
         CLM   R1,1,SAVESESS       SESSION ACTIVE (ALREADY BEING USED)          
         BE    SSSV4                                                            
         CLM   R1,1,RSRVSESS       RESERVED SESSION                             
         BE    SSSV4                                                            
         CLI   NUM4SESS,X'FF'      4TH CHOICE IS ANY SESSION                    
         BNE   *+8                                                              
         STC   R1,NUM4SESS                                                      
         CLM   R1,1,LASTSESS       3RD CHOICE IS LAST SESSION                   
         BNE   *+12                                                             
         STC   R1,NUM3SESS                                                      
         B     SSSV4                                                            
         CLM   R1,1,PREVSESS       2ND CHOICE IS PREVIOUS SESSION               
         BNE   *+12                                                             
         STC   R1,NUM2SESS                                                      
         B     SSSV4                                                            
         CLI   NUM1SESS,X'FF'                                                   
         BNE   SSSV4                                                            
         STC   R1,NUM1SESS         1ST CHOICE IS NON LAST OR NON PREV           
*                                                                               
SSSV4    LA    R1,1(R1)            BUMP TO NEXT SESSION                         
         CH    R1,SSMAX                                                         
         BL    *+6                                                              
         SR    R1,R1                                                            
         BCT   R0,SSSV2                                                         
*                                                                               
SSSV5    CLI   SRFLD+4,C'U'        TEST IF UNCONDITIONAL SAVE                   
         B     SSSV6               *NOP* BE SSSV6                               
         CLI   SSMAX+1,2           TEST IF ONLY TWO SESSIONS                    
         BE    SSSV6                                                            
         CLC   TSYS(2),=X'0A0C'    TEST CONTROL/MAD                             
         BE    SSSV6                                                            
         B     SSERR3              ERROR NO AVAILABLE FREE SESSIONS             
*                                                                               
SSSV6    LA    RF,NUM1SESS         FIND BEST CHOICE ACTIVE SESSION              
         LA    R0,4                                                             
         CLI   0(RF),X'FF'                                                      
         BNE   SSSV7                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
         B     SSERR3              ERROR NO AVAILABLE FREE SESSIONS             
SSSV7    IC    R1,0(RF)                                                         
         STC   R1,SESS             SET NEW SESSION AND DISCONNECT               
         STC   R1,STOLEN           SET HAVE STOLEN AN ACTIVE SESSION            
         LR    R0,R1                                                            
         STC   R0,DMCB             SET SESSION NUMBER TO USE                    
         MVI   DMCB+4,X'80'        SET DISCONNECT FLAG                          
         GOTO1 ATWASVR,DMCB        CALL SAVE/RESTORE TO DISCONNECT              
         LR    R1,R0                                                            
         B     SSSVAL                                                           
*                                                                               
SSSW     CLI   SRFLD+4,C'W'        SWAP SESSION                                 
         BNE   SSSX                                                             
*                                                                               
SSSW0    TM    ACTN,X'FE'          TEST IF STEREO ISSUES =SW                    
         BZ    SSSW0A                                                           
*                                                                               
         CLC   LASTSESS,SSMXP+1    TEST IF LAST SESSION AVAILABLE               
         BNL   SSSNOP0                                                          
         CLC   SSMAX,SSMXP         TEST EXTRA SESSION DEFINED                   
         BE    SSSW00A             NO                                           
         LH    R1,SSMXP            TEST RETURNING FROM IT                       
         BCTR  R1,0                                                             
         CLM   R1,1,SAVESESS                                                    
         BE    SSSW0A              THEN NO NEED FOR PREV SESSION                
*                                                                               
SSSW00A  CLC   PREVSESS,SSMXP+1    TEST IF PREV SESSION AVAILABLE               
         BNL   SSSNOP0                                                          
*                                                                               
*>>      CLC   LASTSESS,SSMAX+1    TEST IF LAST SESSION AVAILABLE               
*        BNL   SSSNOP0                                                          
*        CLC   PREVSESS,SSMAX+1    TEST IF PREV SESSION AVAILABLE               
*>>      BNL   SSSNOP0                                                          
*                                                                               
SSSW0A   CLC   SRFLD+5(3),=C',XCT' XCTL RETURN FROM SEPARATE SESSION            
         BNE   SSSW1                                                            
         OI    XCTL,X'01'          SET FLAG TO SHOW =SS,W,XCT                   
         CLI   SRFLD+8,C'S'                                                     
         BNE   *+8                                                              
         OI    XCTL,X'02'          SET FLAG TO SHOW =SS,X,XCS                   
*                                                                               
         CLC   SSMAX,SSMXP         TEST EXTRA SESSION DEFINED                   
         BE    SSSW1               NO                                           
         LH    R1,SSMXP            TEST RETURNING FROM IT                       
         BCTR  R1,0                                                             
         CLM   R1,1,SAVESESS                                                    
         BNE   SSSW1                                                            
*                                                                               
         OI    XCTL,X'80'          SET RETURNING FROM EXTRA SESS                
         CLC   LASTSESS,SSMAX+1                                                 
         BNL   SSERR5                                                           
         MVC   SESS,LASTSESS                                                    
         B     SSSVAL                                                           
*                                                                               
SSSW1    CLC   LASTSESS,SSMAX+1    TEST IF LAST SESSION AVAILABLE               
         BNL   SSERR5                                                           
         CLC   PREVSESS,SSMAX+1    TEST IF PREV SESSION AVAILABLE               
         BNL   SSERR6                                                           
SSSW1A   CLC   SAVESESS,LASTSESS   SWAP TO PREV IF CURRENTLY IN LAST            
         BNE   SSSW2                                                            
         MVC   SESS,PREVSESS                                                    
         IC    RE,LASTSESS                                                      
         IC    RF,PREVSESS                                                      
         STC   RE,PREVSESS                                                      
         STC   RF,LASTSESS                                                      
         SLL   RF,28                                                            
         SLDL  RE,4                                                             
         STC   RE,TSSSWAP                                                       
         B     SSSVAL                                                           
SSSW2    CLC   SAVESESS,PREVSESS   SWAP TO LAST IF CURRENTLY IN PREV            
         BNE   SSSW3                                                            
         MVC   SESS,LASTSESS                                                    
         B     SSSVAL                                                           
SSSW3    MVC   SESS,LASTSESS       DEFAULT TO LAST SESSION                      
         B     SSSVAL                                                           
*                                                                               
SSSX     CLI   SRFLD+4,C'X'        MAXIMUM SESSION                              
         BNE   SSSNOP                                                           
         LH    R1,SSMAX                                                         
         BCTR  R1,0                                                             
         STC   R1,SESS                                                          
         B     SSSVAL                                                           
*                                                                               
SSSNOP   CLI   SRFLD+4,C'*'        THIS SESSION                                 
         BNE   SSERR1                                                           
SSSNOP0  MVC   SESS,SAVESESS                                                    
*                                                                               
SSSVAL   CLI   SRFLD+5,C','        TEST IF MORE PARAMS                          
         BNE   SSSVALX                                                          
         CLC   SRFLD+6(2),=C'S='   =SS,X,S=ABC ALLOWED TO SET STEREO            
         BNE   *+12                                                             
         LA    RE,SRFLD+8                                                       
         B     SSSVAL0                                                          
         CLI   SRFLD+9,C','                                                     
         BNE   SSSVAL2                                                          
         CLC   SRFLD+10(2),=C'S='  =SS,X,XXX,S=ABC ALSO FOR STEREO              
         BNE   SSSVAL2                                                          
         LA    RE,SRFLD+12                                                      
SSSVAL0  MVC   STIN1(3),0(RE)      SAVE STEREO INPUT CHRS                       
         CLI   STIN3,0                                                          
         BNE   *+8                                                              
         MVI   STIN3,C'*'                                                       
         CLI   STIN2,0                                                          
         BNE   *+8                                                              
         MVI   STIN2,C'*'                                                       
         CLI   STIN1,0                                                          
         BNE   *+8                                                              
         MVI   STIN1,C'*'                                                       
*                                                                               
SSSVAL1  CLI   STIN1,C'Y'          FIRST CHR TOGGLES TST6STRO                   
         BNE   *+12                                                             
         OI    STEREO,TST6STRO                                                  
         B     SSSVAL1A                                                         
         CLI   STIN1,C'N'                                                       
         BNE   *+12                                                             
         NI    STEREO,255-TST6STRO                                              
         B     *+12                                                             
         CLI   STIN1,C'*'                                                       
         BNE   SSERR10                                                          
SSSVAL1A CLI   STIN2,C'Y'          SECOND CHR TOGGLES TST6STFU                  
         BNE   *+12                                                             
         OI    STEREO,TST6STFU                                                  
         B     SSSVAL1B                                                         
         CLI   STIN2,C'N'                                                       
         BNE   *+12                                                             
         NI    STEREO,255-TST6STFU                                              
         B     SSSVAL1B                                                         
         CLI   STIN2,C'*'                                                       
         BNE   SSERR10                                                          
SSSVAL1B CLI   STIN3,C'Y'          THIRD CHR TOGGLES TST6STSS                   
         BNE   *+20                                                             
         OI    STEREO,TST6STSS                                                  
         OI    STEREO8,TST8STSS                                                 
         NI    STEREO8,X'FF'-TST8BINT                                           
         B     SSSVAL1D                                                         
         CLI   STIN3,C'N'                                                       
         BNE   *+16                                                             
         NI    STEREO,255-TST6STSS                                              
         NI    STEREO8,255-(TST8STSS+TST8BINT)                                  
         B     SSSVAL1D                                                         
*                                                                               
         CLI   STIN3,C'B'          BINARY TRANSFER ENABLE                       
         BNE   SSSVAL1C                                                         
         OI    STEREO,TST6STSS                                                  
         OI    STEREO8,TST8STSS                                                 
         TM    TSTAT9,TSTNVRSN                                                  
         BZ    *+8                                                              
         OI    STEREO8,TST8BINT                                                 
         B     SSSVAL1D                                                         
*                                                                               
SSSVAL1C CLI   STIN3,C'*'                                                       
         BNE   SSERR10                                                          
SSSVAL1D TM    STEREO,TST6STRO     TEST WHAT CHANGED                            
         BZ    *+8                                                              
         OI    ACTN,X'40'          SET FLAG#1 ACTIVE AFTER ACTION               
         TM    STEREO,TST6STFU                                                  
         BZ    *+8                                                              
         OI    ACTN,X'10'          SET FLAG#2 ACTIVE AFTER ACTION               
         TM    STEREO,TST6STSS                                                  
         BZ    *+8                                                              
         OI    ACTN,X'04'          SET FLAG#3 ACTIVE AFTER ACTION               
         TM    STEREO8,TST8STSS                                                 
         BZ    *+8                                                              
         OI    ACTN,X'04'          SET FLAG#3 ACTIVE AFTER ACTION               
SSSVAL1E OI    ACTN,X'02'          SET S= INPUT                                 
*                                                                               
SSSVAL2  CLC   SRFLD+6(2),=C'S='   ALREADY VALIDATED THIS                       
         BE    SSSVALX                                                          
*                                                                               
SSSVAL3  CLC   SRFLD+6(3),=C'RSV'  RESERVE A SESSION                            
         BNE   SSSVAL4                                                          
         TM    ACTN,X'FE'          ONLY STEREO CAN DO THIS                      
         BZ    SSERR10                                                          
         CLC   SESS,SSMAX+1        MUST BE VALID SESSION                        
         BNL   SSERR1                                                           
         SR    R1,R1                                                            
         IC    R1,SESS                                                          
         IC    R1,SSACBITS(R1)                                                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    TSSBITS,0                                                        
         BO    SSSVAL3A            *NOP* BO SSERR10 (MUST BE ACTIVE)            
SSSVAL3A NI    TSSRSRV,X'F0'                                                    
         OC    TSSRSRV,SESS                                                     
         MVC   RSRVSESS,SESS                                                    
         B     SSSVALX                                                          
*                                                                               
SSSVAL4  CLC   SRFLD+6(3),=C'RLS'  RELEASE A SESSION                            
         BNE   SSSVAL5                                                          
         TM    ACTN,X'FE'          ONLY STEREO CAN DO THIS                      
         BZ    SSERR10                                                          
         OI    TSSRSRV,X'0F'                                                    
         OI    RSRVSESS,X'0F'                                                   
         B     SSSVALX                                                          
*                                                                               
SSSVAL5  CLC   SRFLD+6(3),=C'TRA'  TRANSFER SESSION                             
         BNE   SSSVAL6                                                          
         TM    ACTN,X'FE'          ONLY STEREO CAN DO THIS                      
         BZ    SSERR10                                                          
         CLC   SESS,SSMAX+1        MUST BE VALID SESSION                        
         BNL   SSERR1                                                           
         SR    R1,R1                                                            
         IC    R1,SESS                                                          
         IC    R1,SSACBITS(R1)                                                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    TSSBITS,0                                                        
         BO    SSSVAL5A            *NOP* BO SSERR10 (MUST BE ACTIVE)            
SSSVAL5A NI    TSSRSRV,X'0F'                                                    
         PACK  BYTE,SESS                                                        
         OC    TSSRSRV,BYTE        SET TRANSFER SESSION IN HOB                  
         MVC   TRANSESS,SESS                                                    
         B     SSSVALX                                                          
*                                                                               
SSSVAL6  CLC   SRFLD+6(3),=C'UNT'  UNTRANSFER A SESSION                         
         BNE   SSSVAL7                                                          
         TM    ACTN,X'FE'          ONLY STEREO CAN DO THIS                      
         BZ    SSERR10                                                          
         OI    TSSRSRV,X'F0'                                                    
         OI    TRANSESS,X'0F'                                                   
         B     SSSVALX                                                          
*                                                                               
SSSVAL7  CLC   SRFLD+6(3),=C'USW'  MAKE SESSION UNSWAPABLE                      
         BNE   SSSVAL8                                                          
         TM    ACTN,X'FE'          ONLY STEREO CAN DO THIS                      
         BZ    SSERR10                                                          
         CLC   SESS,SSMAX+1        MUST BE VALID SESSION                        
         BNL   SSERR1                                                           
         SR    R1,R1                                                            
         IC    R1,SESS                                                          
         IC    R1,SSACBITS(R1)                                                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         OI    TSSXBITS,0                                                       
         B     SSSVALX                                                          
*                                                                               
SSSVAL8  CLC   SRFLD+6(3),=C'SWA'  MAKE SESSION SWAPABLE                        
         BNE   SSSVAL9                                                          
         TM    ACTN,X'FE'          ONLY STEREO CAN DO THIS                      
         BZ    SSERR10                                                          
         SR    R1,R1                                                            
         IC    R1,SESS                                                          
         IC    R1,SSNABITS(R1)                                                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         NI    TSSXBITS,0                                                       
         B     SSSVALX                                                          
*                                                                               
SSSVAL9  CLC   SRFLD+6(2),=C'XCT'  XCTL RETURN FROM SEPARATE SESSION            
         BNE   SSSVAL10                                                         
         OI    XCTL,X'01'          SET FLAG TO SHOW =SS,.,XCT                   
         CLI   SRFLD+8,C'S'                                                     
         BNE   *+8                                                              
         OI    XCTL,X'02'          SET FLAG TO SHOW =SS,X,XCS                   
         TM    XCTL,X'02'          TEST WANT TO SAVE SESSION                    
         BO    SSSVALX                                                          
         TM    XCTL,X'80'          TEST RETURN FROM EXTRA SESSION               
         NOP   SSSVALX                                                          
         MVC   DMCB(1),SAVESESS                                                 
         MVI   DMCB+4,X'80'        SET DISCONNECT FLAG                          
         GOTO1 ATWASVR,DMCB        CALL SAVE/RESTORE TO DISCONNECT              
         OI    XCTL,X'40'          SET SESSION DISCONNECTED                     
         B     SSSVALX                                                          
*                                                                               
SSSVAL10 CLC   SRFLD+6(3),=C'***'  NOP ACTION                                   
         BNE   SSERR10                                                          
*                                                                               
SSSVALX  CLC   SESS,RSRVSESS       IS THE SESSION RESERVED                      
         BNE   *+12                                                             
         TM    ACTN,X'FE'          ONLY STEREO CAN ACCESS THIS                  
         BZ    SSERR4                                                           
*                                                                               
         CLC   SESS,SSMAX+1        DOES THE SESSION EXIST                       
         BNL   SSERR1                                                           
*>>      BL    SS01                                                             
*                                                                               
*        CLC   SSMAX,SSMXP                                                      
*        BE    SSERR1                                                           
*        CLC   SESS,SSMXP+1        DOES THE SESSION EXIST                       
*        BNL   SSERR1                                                           
*        B     SSX                                                              
*>>      B     SS03A                                                            
*                                                                               
SS01     MVC   DMCB(1),SESS        SET REQUIRED SESSION NUMBER                  
         MVI   DMCB+4,0                                                         
         GOTO1 ATWASVR,DMCB        CALL SAVE/RESTORE ROUTINE                    
         MVC   ERRNUM,4(R1)                                                     
         CLI   ERRNUM,0            TEST IF NEW SESSION VALID                    
         BE    SS02                                                             
         CLI   ERRNUM,125          TEST IF NEW SESSION INVALID/TIMEOUT          
         BE    SS02                                                             
         CLI   ERRNUM,126          TEST IF NEW SESSION NOT CONNECTED            
         BE    SS02                                                             
         B     SSERRX                                                           
*                                                                               
SS02     SR    RE,RE               MOVE SAVE/RSTR SCREEN TO MY TWA              
         ICM   RE,7,DMCB+5                                                      
         LH    RF,RECLEN                                                        
         LR    R0,R3                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         SR    R1,R1                                                            
         IC    R1,64(R3)                                                        
         LA    R7,64(R3,R1)        R7=A(S/R FIELD HEADER)                       
         CLI   0(R7),X'19'         CHECK THAT IT LOOKS OK                       
         BNE   *+14                                                             
         CLC   2(2,R7),=X'003E'                                                 
         BE    SS02A                                                            
         SR    R7,R7                                                            
*                                                                               
SS02A    LR    R4,R3                                                            
         AH    R4,CHKDSP                                                        
         USING CHKPTD,R4           R4=A(TWA0 CHECKPOINT AREA)                   
*                                                                               
SS02B    NI    TSTAT6,255-TST6STRO-TST6STFU-TST6STSS                            
         TM    CHUTLST6,TST6STRO                                                
         BZ    *+8                                                              
         OI    TSTAT6,TST6STRO     RESTORE STEREO FLAGS FROM CHKPNT             
         TM    CHUTLST6,TST6STFU                                                
         BZ    *+8                                                              
         OI    TSTAT6,TST6STFU                                                  
         TM    CHUTLST6,TST6STSS                                                
         BZ    *+8                                                              
         OI    TSTAT6,TST6STSS                                                  
         TM    ACTN,X'02'          WAS S=ABC INPUT                              
         BZ    SS03                NO LEAVE AS IT WAS LAST TIME                 
SS02C    CLI   STIN1,C'*'          TEST ANY CHANGE TO 1ST STEREO FLAG           
         BE    SS02D                                                            
         NI    TSTAT6,255-TST6STRO                                              
         CLI   STIN1,C'Y'                                                       
         BNE   SS02D                                                            
         OI    TSTAT6,TST6STRO                                                  
SS02D    CLI   STIN2,C'*'          TEST ANY CHANGE TO 2ND STEREO FLAG           
         BE    SS02E                                                            
         NI    TSTAT6,255-TST6STFU                                              
         CLI   STIN2,C'Y'                                                       
         BNE   SS02E                                                            
         OI    TSTAT6,TST6STFU                                                  
SS02E    CLI   STIN3,C'*'          TEST ANY CHANGE TO 3RD STEREO FLAG           
         BE    SS03                                                             
         NI    TSTAT6,255-TST6STSS                                              
         NI    TSTAT8,255-TST8STSS-TST8BINT                                     
         CLI   STIN3,C'Y'                                                       
         BNE   SS02F                                                            
         OI    TSTAT6,TST6STSS                                                  
         OI    TSTAT8,TST8STSS                                                  
         B     SS03                                                             
SS02F    CLI   STIN3,C'B'                                                       
         BNE   SS03                                                             
         OI    TSTAT6,TST6STSS                                                  
         OI    TSTAT8,TST8STSS                                                  
         TM    TSTAT9,TSTNVRSN                                                  
         BZ    *+8                                                              
         OI    TSTAT8,TST8BINT                                                  
         B     SS03                                                             
*                                                                               
SS03     CLI   ERRNUM,125          FIX UTL FOR DISCONNECTED SESSION             
         BE    SS03A                                                            
         CLI   ERRNUM,126                                                       
         BNE   SS04                                                             
SS03A    MVC   SUSER,TUSER         SAVE USERID NUMBER                           
         MVC   SPASSWD,TPASSWD     SAVE PASSWORD NUMBER                         
         MVC   SAGY,TAGY           SAVE AGENCY ALPHA                            
         MVC   SSYS,TSYS                                                        
         MVC   SPRG,TPRG                                                        
         XC    TCTDATA,TCTDATA     SET TO SHOW DISCONNECTED                     
         XC    TPERSON,TPERSON                                                  
         NI    TSTAT7,255-(TST7PS0+TST7PS1+TST7PS2+TST7PSWN)                    
         MVI   TPASSEXP,0                                                       
         MVI   TSTATB,0                                                         
         XC    TAGYPER,TAGYPER                                                  
         XC    TUPDFAC,TUPDFAC                                                  
         XC    TTICKET,TTICKET                                                  
*                                                                               
         L     R1,TUTLXADR                                                      
         USING XAUTLD,R1                                                        
         SAM31                                                                  
         XC    TTICKETN,TTICKETN                                                
         SAM24                                                                  
         DROP  R1                                                               
*                                                                               
         XC    TAPRG,TAPRG                                                      
         MVC   TSESSION,SESS       SET NEW SESSION                              
         SR    R1,R1                                                            
         IC    R1,TSESSION                                                      
         IC    RF,SSNABITS(R1)                                                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         NI    TSSBITS,0           TURN OFF SESSION ACTIVE BIT                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         NI    TSSXBITS,0          TURN OFF CANT USE SESSION BIT                
         B     SS10                                                             
*                                                                               
SS04     MVC   SVTSVC,TSVCREQ      RESTORE CONNECT DATA                         
         MVC   TCTDATA,CHUTLSV                                                  
         MVC   TPERSON,CHUTLPER                                                 
         MVC   TSVCREQ,SVTSVC                                                   
*                                                                               
         MVC   SVFLAG,TFLAG                                                     
         MVC   SVTRCNT,TTRCNT                                                   
         MVC   TSVDATA1,CHUTLSV1                                                
         MVC   TFLAG,SVFLAG                                                     
         MVC   TTRCNT,SVTRCNT                                                   
*                                                                               
         MVC   TAGYSEC,CHUTLASC                                                 
         MVC   TACCS2,CHUTLAC2                                                  
*                                                                               
         MVC   TXPINFO,CHUTLXPI                                                 
*                                                                               
         NI    TSTAT6,255-TST6LOGN                                              
         TM    CHUTLST6,TST6LOGN                                                
         BZ    *+8                                                              
         OI    TSTAT6,TST6LOGN                                                  
*                                                                               
         NI    TSTAT7,255-(TST7PS0+TST7PS1+TST7PS2+TST7PSWN)                    
         TM    CHUTLST7,TST7PS0                                                 
         BZ    *+8                                                              
         OI    TSTAT7,TST7PS0                                                   
         TM    CHUTLST7,TST7PSWN                                                
         BZ    *+8                                                              
         OI    TSTAT7,TST7PSWN                                                  
         MVC   TPASSEXP,CHUTLPEX                                                
         MVC   TSTATB,CHUTLSTB                                                  
         MVC   TAGYPER,CHUTLAGP                                                 
         MVC   TUPDFAC,CHUTLUPF                                                 
         MVC   TTICKET,CHUTLTKT                                                 
*                                                                               
         L     R1,TUTLXADR                                                      
         USING XAUTLD,R1                                                        
         SAM31                                                                  
         MVC   TTICKETN,CHUTLTKN   TICKETN TO XAUTL                             
         SAM24                                                                  
         DROP  R1                                                               
*&&UK                                                                           
         NI    TSTAT9,255-TST9AGMS-TST9AGPR                                     
         TM    CHUTLST9,TST9AGMS                                                
         BZ    *+8                                                              
         OI    TSTAT9,TST9AGMS                                                  
         TM    CHUTLST9,TST9AGPR                                                
         BZ    *+8                                                              
         OI    TSTAT9,TST9AGPR                                                  
*&&                                                                             
*&&US                                                                           
         NI    TSTAT9,255-TST9SPTR                                              
         TM    CHUTLST9,TST9SPTR                                                
         BZ    *+8                                                              
         OI    TSTAT9,TST9SPTR                                                  
*&&                                                                             
SS05     CLI   TSYS,0              TEST IF CONNECTED SESSION                    
         BE    SS10                                                             
SS05A    L     R1,VSELIST          RESET TASYS AND TAPRG                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   TSYS,SESYS-SELISTD(R1)                                           
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         STCM  R1,7,TASYS                                                       
         STCM  R1,7,TARSYS                                                      
*                                                                               
         L     R1,SEPGMS-SELISTD(R1)                                            
         LH    RE,0(R1)            SET BXLE FOR PGMLST READ                     
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         MVI   FLAG,0                                                           
SS05B    CLC   TAGCTRY,PGMCTRY-PGMLSTD(R1)                                      
         BNE   *+8                                                              
         MVI   FLAG,1              ENTRY FOUND FOR THIS CTRY                    
         CLC   TPRG,PGMNUM-PGMLSTD(R1)                                          
         BNE   SS05C               NO MATCH ON PGM NUMBER                       
         CLI   PGMCTRY-PGMLSTD(R1),0                                            
         BNE   *+12                IF NOT CTRY=0 IT MUST MATCH                  
         CLI   FLAG,1              IF NO ENTRIES FOUND THEN OK                  
         BNE   SS05D                                                            
         CLC   TAGCTRY,PGMCTRY-PGMLSTD(R1)                                      
         BE    SS05D                                                            
SS05C    BXLE  R1,RE,SS05B                                                      
         SR    R1,R1                                                            
         OC    TAPRG,TAPRG         TEST P=XX INPUT ON CONNECT                   
         BNZ   SS05E                                                            
         DC    H'0'                                                             
*                                                                               
SS05D    L     RF,VSSB             SEE IF TAPRG IS DISPLACEMENT                 
         TM    SSBSTAT4-SSBD(RF),SSBPGDSP                                       
         BZ    SS05E                                                            
         XR    RF,RF                                                            
         ICM   RF,7,TARSYS                                                      
         ICM   RF,15,SEPGMS-SELISTD(RF)                                         
         SR    R1,RF                                                            
*                                                                               
SS05E    STCM  R1,7,TAPRG                                                       
*                                                                               
SS06     MVC   TCBLNSYS,CHLNSYS    RESTORE TCB SYS AND PRG                      
         MVC   TCBLNPRG,CHLNPRG                                                 
         MVC   TCBSRMSG,CHSRMSG                                                 
         MVC   TCBBILL,CHBILL      RESTORE BILLING REFERENCE                    
         MVC   TCBXTINF,CHXTINF    RESTORE TEMPEST                              
*                                                                               
SS07     SR    R0,R0               R0=N'SYSTEM SWITCH ENTRIES                   
         ICM   R0,1,CHTCBNUM                                                    
         BZ    SS08                                                             
         CHI   R0,TCBSWMAX         DO NOT RESTORE A BAD COUNT                   
         JH    *+2                                                              
         STC   R0,TCBSWNUM                                                      
         LA    RE,TCBSWTAB                                                      
         USING TCBSWTAB,RE         RE=A(TCB SYSTEM TABLE)                       
         LA    RF,CHTCBTAB                                                      
         USING CHTCBTAB,RF         RF=A(TWA CHKPT TABLE)                        
SS07A    MVC   TCBSWSYS,CHTCBSYS                                                
         MVC   TCBSWSOV,CHTCBSOV                                                
         MVC   TCBSWAGB,CHTCBAGB                                                
         MVC   TCBSWACS,CHTCBACS                                                
         LA    RE,TCBSWLEN(RE)                                                  
         LA    RF,CHTCBLEN(RF)                                                  
         BCT   R0,SS07A                                                         
         DROP  RE,RF                                                            
*                                                                               
SS08     MVC   TSESSION,SESS       SET NEW SESSION NUMBER                       
         SR    R1,R1                                                            
         IC    R1,TSESSION                                                      
         IC    R1,SSACBITS(R1)                                                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         OI    TSSBITS,0           SET SESSION ACTIVE BIT                       
         LTR   R7,R7                                                            
         BZ    SS09                                                             
         TM    TSTAT6,TST6STRO                                                  
         BO    *+14                                                             
         MVC   8(17,R7),CHSRMSG    RESTORE *USR/SYS/PRG/FS*                     
         B     SS09                                                             
         XC    8(17,R7),8(R7)      CLEAR FOR STEREO DATA                        
         MVI   8(R7),C'*'                                                       
*                                                                               
SS09     NI    TFLAG,255-TFLAGRTS  SET NO TEMPEST ALLOCATED                     
         OC    CHXTNDX,CHXTNDX                                                  
         BZ    SS09A                                                            
         TM    CHXTFLG,X'80'       TEMPEST DOES NOT BELONG TO SESSION           
         BO    SS09A                                                            
         OI    TFLAG,TFLAGRTS      SET RESERVED TEMPEST SPACE                   
*                                                                               
SS09A    MVC   CHUTLSV1+TFLAG-TSVDATA1,TFLAG                                    
         NI    TSTAT1,255-TSTATBIL                                              
         OC    CHBILL,CHBILL                                                    
         BZ    *+8                                                              
         OI    TSTAT1,TSTATBIL     SET BILLING ACTIVE                           
*                                                                               
SS10     MVC   TSVCREQ,$XMTALL     SET TO TRANSMIT SCREEN                       
         MVI   SRPARM1,X'FF'       SET VALUE TO FORCE CHECKPOINT/WRITE          
*&&US                                                                           
         L     R1,ATWA             SET TRANSMIT ALL IN TWA                      
         AHI   R1,64                                                            
         XR    RF,RF                                                            
SS10A    CLI   0(R1),X'00'                                                      
         BE    SS10B                                                            
         ICM   RF,1,0(R1)                                                       
         BZ    SS10B                                                            
         BXH   R1,RF,SS10A                                                      
SS10B    MVC   0(3,R1),=XL3'000101'                                             
*&&                                                                             
SS11     CLI   XCTL,0              TEST IF XCTL RETURN FROM SEP SESSION         
         BE    SSX                                                              
         B     SSX                 YES - DONT HAVE TO DO ANYTHING MORE          
*                                                                               
SSX      CLI   STOLEN,X'FF'        DID WE HAVE TO STEAL A SESSION               
         BE    SSXX                NO                                           
         SR    R1,R1                                                            
         SR    RF,RF                                                            
         IC    R1,STOLEN           YES SET SESSION STOLEN FLAG IN UTL           
         IC    RF,SSACBITS(R1)                                                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         OI    TNAHNAH,0                                                        
*                                                                               
SSXX     XMOD1 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*INITIALISATION FOR EXTENDED DISPLAY                                  *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    SRVP1,SRVP1         CLEAR FOR NOW                                
         XC    SRVP2,SRVP2                                                      
         XC    SRVP3,SRVP3                                                      
         XC    SRVP4,SRVP4                                                      
INITX    B     XIT1                                                             
         SPACE 1                                                                
***********************************************************************         
*MAIN CONTROL FOR EXTENDED DISPLAY                                    *         
***********************************************************************         
         SPACE 1                                                                
MAIN     NTR1                                                                   
         CLI   ACTN,C'T'                                                        
         BE    MAIN2                                                            
         CLI   ACTN,C'X'                                                        
         BE    MAIN1                                                            
         CLI   ACTN,C'D'                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,READTWA                                                       
         L     R1,ASAVE1           SAVE THE CHECKPOINT                          
         BAS   RE,SAVECHK                                                       
         L     R1,ASAVE1                                                        
         BAS   RE,DISPCHK          DISPLAY IT                                   
         B     MAINXXX                                                          
*                                                                               
MAIN1    MVI   BYTE,0              SAVE ALL THE CHECKPOINTS                     
         LA    R4,ASAVE1                                                        
MAIN010  BAS   RE,READTWA                                                       
         L     R1,0(R4)                                                         
         BAS   RE,SAVECHK                                                       
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         LA    R4,4(R4)                                                         
         CLC   BYTE,SSMAX+1        WAS THIS THE LAST SESSION                    
         BNE   MAIN010                                                          
*NOP     OC    0(4,R4),0(R4)                                                    
*NOP     BNZ   MAIN010                                                          
*                                                                               
         BAS   RE,DISPALL                                                       
         B     MAINXXX                                                          
*                                                                               
MAIN2    CLI   PFKEY,0             ENTER FOR DISPLAY                            
         BE    MAIN30                                                           
*                                                                               
         CLI   PFKEY,4             PF4=SWAP                                     
         BNE   MAIN20                                                           
         CLI   TSWAPLID,0          MUST HAVE A SYSTEM DEFINED                   
         BE    SSERR8                                                           
         BAS   RE,SWAPADV                                                       
         B     MAINXXX                                                          
*                                                                               
MAIN20   MVI   BYTE,X'C0'          PRESET SESSION                               
         SR    R1,R1                                                            
         ICM   R1,3,CURADR                                                      
         SH    R1,=H'160'          TEST TOP OF SCREEN                           
         BM    MAIN30                                                           
*                                                                               
MAIN21   IC    RF,BYTE             BUMP SESSION                                 
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         SH    R1,CURDSP           BUMP CURSOR POSN                             
         BNM   MAIN21                                                           
*                                                                               
MAIN22   CLI   PFKEY,2             PF2=CONNECT TO                               
         BNE   *+12                                                             
         BAS   RE,CONNECT          SET CONNECT BUFFER                           
         B     MAINXXX                                                          
*                                                                               
         CLI   PFKEY,3             PF2=DISCONNECT SESSION                       
         BNE   *+8                                                              
         BAS   RE,DISCONT          DISCONNECT SESSION                           
*                                                                               
MAIN30   BAS   RE,DISPTWA                                                       
         B     MAINXXX                                                          
*                                                                               
MAINXXX  B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
*GOBACKS - GENERATE SERVICE REQUESTS TO SWAP ADV SYSTEMS              *         
***********************************************************************         
         SPACE 1                                                                
SWAPADV  ST    RE,SAVERE                                                        
         ICM   R1,15,TBUFF                                                      
         MVI   0(R1),8             SET LEN                                      
         MVC   1(2,R1),SRVIDH+2    SET ADDR                                     
         MVC   3(5,R1),=C'=SWAP'   SET =SWAP                                    
         MVI   8(R1),6             SET LEN                                      
         MVC   9(2,R1),SRVIDH+2    SET ADDR                                     
         MVC   11(3,R1),=C'=SS'    SET =SESS                                    
         MVI   14(R1),0            SET END                                      
         B     GOBACKX                                                          
*                                                                               
CONNECT  ST    RE,SAVERE                                                        
         ICM   R1,15,TBUFF                                                      
         MVI   0(R1),8             SET LEN                                      
         MVC   1(2,R1),SRVIDH+2    SET ADDR                                     
         MVC   3(5,R1),=C'=SS,?'   SET =SS,?                                    
         MVC   7(1,R1),BYTE        SET SESSION                                  
         MVI   8(R1),0             SET END                                      
*                                                                               
GOBACKX  MVC   SRVID(8),=C'=GOBACK '                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*DISCONNECT SESSION                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISCONT  ST    RE,SAVERE                                                        
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         SH    R1,=H'193'          CONVERT TO SESSION NUM                       
         XC    DMCB,DMCB                                                        
         ST    R8,DMCB                                                          
         STC   R1,DMCB                                                          
         MVI   DMCB+4,X'80'        SET DISCONNECT FLAG                          
         GOTO1 ATWASVR,DMCB                                                     
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*ERROR AND INFO EXITS                                                 *         
***********************************************************************         
         SPACE 1                                                                
INFOX    LH    RE,INFO                                                          
         GOTO1 AGETTXT,DMCB,(RE),0,(C'I',0),(4,SYNAME),,X'00010000'             
         B     XMOD1                                                            
         SPACE 1                                                                
SSERR1   LA    R0,127              SESSION DOES NOT EXIST                       
         B     SSERRX                                                           
SSERR3   LA    R0,133              ALL SESSIONS CURRENTLY IN USE                
         B     SSERRX                                                           
SSERR4   LA    R0,134              SESSION IS RESERVED                          
         B     SSERRX                                                           
SSERR5   LA    R0,135              NO LAST SESSION AVAILABLE                    
         B     SSERRX                                                           
SSERR6   LA    R0,136              NO PREV SESSION AVAILABLE                    
         B     SSERRX                                                           
SSERR7   LA    R0,137              NO RSRV SESSION AVAILABLE                    
         B     SSERRX                                                           
SSERR8   LA    R0,138              NO SWAP SYSTEM DEFINED                       
         B     SSERRX                                                           
SSERR9   LA    R0,139              NO TRANFER SESSION DEFINED                   
         B     SSERRX                                                           
SSERR10  LA    R0,140              SYNTAX ERROR IN SESSION DEFINITION           
         B     SSERRX                                                           
*                                                                               
SSERRX   XC    DMCB(24),DMCB       R0=ERROR NUM                                 
         GOTO1 AGETTXT,DMCB,(R0)                                                
         B     XMOD1                                                            
         EJECT                                                                  
***********************************************************************         
* READ TWA0 INTO TIA BYTE=SESSION                                     *         
***********************************************************************         
         SPACE 1                                                                
READTWA  ST    RE,SAVERE                                                        
         SR    R1,R1               R1=SESSION                                   
         IC    R1,BYTE                                                          
         LA    RF,1                                                             
         SLL   RF,0(R1)            RF=SESSION BIT                               
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    TSSBITS,0           TEST FOR SESSION                             
         BO    READTW5                                                          
         CLI   ACTN,C'T'           TEST FOR ACTION TWA                          
         BNE   READTW5                                                          
*                                                                               
READTW1  L     RE,ATIA             LOAD S/R ROOT SCREEN INTO TIA                
         LH    RF,RECLEN                                                        
         LR    R0,RE                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR CHECKPOINT AREA ETC                    
         L     RE,ATIA                                                          
         LA    RE,64(RE)                                                        
         GOTO1 VCALLOV,DMCB,(RE),X'D70110FF'                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RE,DMCB             REMOVE LOGO FIELDS FROM SCREEN               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
READTW2  IC    R0,0(RE)            BUMP TO NEXT $CT SCREEN FIELD                
         AR    RE,R0                                                            
         LTR   R1,R1               TEST IF LOCATED S/R FIELD                    
         BNZ   READTW3                                                          
         L     R1,VSSB                                                          
         CLI   0(RE),X'19'         CHECK THAT S/R FIELD LOOKS OK                
         BNE   READTW3                                                          
         MVC   8(8,RE),=C'=CT,*  *'                                             
         XC    16(9,RE),16(RE)     SET S/R FIELD TO =CT,*FS*                    
         MVC   13(1,RE),SSBSYSN1-SSBD(R1)                                       
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,14(RE)                                                        
         OI    14(RE),X'80'        SET LOWER CASE SESSION LETTER                
READTW3  CLI   0(RE),0                                                          
         BE    READTWX                                                          
         CLI   0(RE),9             TEST IF LOCATED ONE BYTE TAB FIELD           
         BNE   READTW2                                                          
         MVC   9(3,RE),=X'000101'  TRUNCATE LOGO                                
         B     READTWX                                                          
*                                                                               
READTW5  CLC   BYTE,TSESSION       IF SAME SESSION                              
         BNE   *+10                                                             
         SR    R1,R1               READ PAGE 0                                  
         B     READTW6                                                          
         MH    R1,SSPGS            CONVERT TO REAL PAGE                         
         LA    R1,2(R1)                                                         
         LA    R1,X'80'(R1)                                                     
*                                                                               
READTW6  XC    DMCB(24),DMCB                                                    
         MVC   DMCB+10(2),TNUM      PP00TTTT                                    
         STC   R1,DMCB+8                                                        
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,,ATIA                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
READTWX  L     RE,SAVERE           RETURN                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*SAVE THE CHECKPOINT IN A SAVE AREA R1=ADDR BYTE=SES                  *         
***********************************************************************         
         SPACE 1                                                                
SAVECHK  ST    RE,SAVERE                                                        
         MVC   0(1,R1),BYTE        SAVE SESSION                                 
         L     RF,ATIA             R1=A(CHKPT)                                  
         AH    RF,=Y(CHKPTDSP)                                                  
         LR    R0,RF                                                            
         LA    RE,1(R1)            R1=A(SAVE AREA)                              
         LH    R1,=Y(CHKPTLEN)                                                  
         LR    RF,R1                                                            
         MVCL  RE,R0               SAVE THE CHKPT INFO                          
*                                                                               
         L     RE,SAVERE           RETURN                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*DISPLAY USEFUL CHKPT DATA INTO DISPAREA                              *         
***********************************************************************         
         SPACE 1                                                                
DISPCHK  NTR1                                                                   
         LR    R4,R1                                                            
         USING CHKPTD,R4                                                        
*                                                                               
         OC    CHTDDATA,CHTDDATA   TEST ANY CHECKPOINT DATA                     
         BZ    DISPCHKX                                                         
*                                                                               
         LA    R7,SRVHEADH                                                      
         USING LINED,R7                                                         
*                                                                               
         OI    LINEH+6,X'08'       HI INTESITY                                  
*                                                                               
         MVC   LINEHDR,BCSHDR      BROADCAST HEADER                             
         LA    R7,LINELEN(R7)                                                   
*                                                                               
         BAS   RE,DISPSES                                                       
         BAS   RE,DISPBC           BROADCAST DATA                               
*                                                                               
         MVC   LINEL,DISPWRK1                                                   
         LA    R7,LINELEN(R7)                                                   
         MVC   LINEL,DISPWRK2                                                   
         LA    R7,LINELEN(R7)                                                   
*                                                                               
         LA    R7,LINELEN(R7)                                                   
         OI    LINEH+6,X'08'       HI INTESITY                                  
         MVC   LINEHDR,SV1HDR      SV1 HEADER                                   
         LA    R7,LINELEN(R7)                                                   
*                                                                               
         BAS   RE,DISPUT1                                                       
         BAS   RE,DISPBMP                                                       
*                                                                               
         LA    R7,LINELEN(R7)                                                   
         OI    LINEH+6,X'08'       HI INTESITY                                  
         MVC   LINEHDR,SV2HDR      SV2 HEADER                                   
         LA    R7,LINELEN(R7)                                                   
*                                                                               
         BAS   RE,DISPUT2                                                       
         BAS   RE,DISPBMP                                                       
*                                                                               
         LA    R7,LINELEN(R7)                                                   
         OI    LINEH+6,X'08'       HI INTESITY                                  
         MVC   LINEHDR,SV3HDR      SV3 HEADER                                   
         LA    R7,LINELEN(R7)                                                   
*                                                                               
         BAS   RE,DISPUT3                                                       
         BAS   RE,DISPBMP                                                       
*                                                                               
         LA    R7,LINELEN(R7)                                                   
         OI    LINEH+6,X'08'       HI INTESITY                                  
         MVC   LINEHDR,SV4HDR      SV4 HEADER                                   
         LA    R7,LINELEN(R7)                                                   
*                                                                               
         BAS   RE,DISPUT4                                                       
         BAS   RE,DISPBMP                                                       
*                                                                               
DISPCHKX B     XIT1                                                             
*                                                                               
DISPBMP  EQU   *                                                                
         MVC   LINEL,DISPWRK1                                                   
         LA    R7,LINELEN(R7)                                                   
         BR    RE                                                               
DISPSES  IC    R1,0(R4)                                                         
         LA    R1,C'A'(R1)                                                      
         STC   R1,LINEL1                                                        
         LA    R4,1(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*DISPLAY ALL CHECKPOINT DATA                                          *         
***********************************************************************         
         SPACE 1                                                                
DISPALL  NTR1                                                                   
         USING CHKPTD,R4                                                        
*                                                                               
         LA    R7,SRVHEADH                                                      
         OI    LINEH+6,X'08'       HI INTESITY                                  
         MVC   LINEHDR,SV1HDR      SV1 HEADER                                   
         LA    R7,LINELEN(R7)                                                   
*                                                                               
         LA    R2,ASAVED           DISPLAY ALL CHKPT1 DATA                      
DISPA010 L     R4,0(R2)                                                         
         BAS   RE,DISPSES                                                       
         BAS   RE,DISPUT1                                                       
         BAS   RE,DISPBMP                                                       
         LA    R2,4(R2)                                                         
         OC    0(4,R2),0(R2)                                                    
         BNZ   DISPA010                                                         
*                                                                               
         OI    LINEH+6,X'08'       HI INTESITY                                  
         MVC   LINEHDR,SV2HDR      SV2 HEADER                                   
         LA    R7,LINELEN(R7)                                                   
*                                                                               
         LA    R2,ASAVED           DISPLAY ALL CHKPT2 DATA                      
DISPA020 L     R4,0(R2)                                                         
         BAS   RE,DISPSES                                                       
         BAS   RE,DISPUT2                                                       
         BAS   RE,DISPBMP                                                       
         LA    R2,4(R2)                                                         
         OC    0(4,R2),0(R2)                                                    
         BNZ   DISPA020                                                         
*                                                                               
         OI    LINEH+6,X'08'       HI INTESITY                                  
         MVC   LINEHDR,SV3HDR      SV3 HEADER                                   
         LA    R7,LINELEN(R7)                                                   
*                                                                               
         LA    R2,ASAVED           DISPLAY ALL CHKPT3 DATA                      
DISPA030 L     R4,0(R2)                                                         
         BAS   RE,DISPSES                                                       
         BAS   RE,DISPUT3                                                       
         BAS   RE,DISPBMP                                                       
         LA    R2,4(R2)                                                         
         OC    0(4,R2),0(R2)                                                    
         BNZ   DISPA030                                                         
*                                                                               
         OI    LINEH+6,X'08'       HI INTESITY                                  
         MVC   LINEHDR,SV4HDR      SV4 HEADER                                   
         LA    R7,LINELEN(R7)                                                   
*                                                                               
         LA    R2,ASAVED           DISPLAY ALL CHKPT3 DATA                      
DISPA040 L     R4,0(R2)                                                         
         BAS   RE,DISPSES                                                       
         BAS   RE,DISPUT4                                                       
         BAS   RE,DISPBMP                                                       
         LA    R2,4(R2)                                                         
         OC    0(4,R2),0(R2)                                                    
         BNZ   DISPA040                                                         
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
*DISPLAY ALL TWAS                                                     *         
***********************************************************************         
         SPACE 1                                                                
DISPTWA  NTR1                                                                   
         LA    R7,SRVHEADH         R7=START OF SCREEN                           
         MVI   BYTE,255            SESSION 0 FIRST BUMP                         
         MVC   HALF,=H'160'        1ST OFFSET IS 160 CHRS                       
         SR    R1,R1                                                            
         B     *+8                                                              
*                                                                               
DIST010  LH    R1,TWADSP           BUMP OFFSET TO NEXT BLOCK                    
         AH    R1,HALF                                                          
         MVC   0(86,R7),DUMMY      ADD A ROW OF DASHES                          
         LA    R1,1(R1)                                                         
         STCM  R1,3,2(R7)                                                       
         LA    R7,86(R7)                                                        
         LA    R1,79(R1)           NEXT LINE                                    
         STH   R1,HALF                                                          
*                                                                               
         SR    R1,R1               BUMP SESSION                                 
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
*                                                                               
         CLC   BYTE,SSMAX+1        WAS THIS THE LAST SESSION                    
         BNE   DIST015                                                          
*                                                                               
         MVC   0(86,R7),DUMMY      ADD A ROW OF PFKEYS                          
         NI    1(R7),255-X'08'     TURN OFF HI INTENSITY                        
         MVC   2(2,R7),=X'0731'                                                 
*                                                                               
         L     RE,AFID                                                          
DIST013  CLC   4(1,RE),TSWAPLID    FIND SWAP SYSTEM IN FACIDTAB                 
         BE    DIST014                                                          
         LA    RE,8(RE)                                                         
         CLI   4(RE),X'FF'                                                      
         BNE   DIST013                                                          
         LA    RE,=C'....'         JUST SHOW .... IF NOT FOUND                  
DIST014  MVC   FULL,0(RE)                                                       
         CLC   0(4,RE),=C'????'                                                 
         BNE   *+10                                                             
         MVC   FULL,=C'    '                                                    
         LA    RE,301                                                           
         GOTO1 AGETTXT,DMCB,(RE),(R7),(C'T',0),(4,FULL),,X'00010000'            
*                                                                               
         LA    R7,86(R7)           NEXT LINE                                    
         B     DIST040                                                          
*                                                                               
DIST015  BAS   RE,READTWA          GO READ TWA                                  
         L     R4,ATIA                                                          
         LH    R1,=Y(CHKPTDSP)     MAKE SURE SAME TERMINAL                      
         AR    R1,R4                                                            
         OC    CHUTLSYM-CHKPTD(8,R1),CHUTLSYM-CHKPTD(R1)                        
         BZ    DIST016                                                          
         CLC   TSYM,CHUTLSYM-CHKPTD(R1)                                         
         BE    DIST016                                                          
         DC    H'0'                                                             
DIST016  LA    R4,64(R4)           POINT TO FIRST TWA FIELD                     
*                                                                               
DIST020  CLC   2(2,R4),TWADSP      TEST 1ST 4 ROWS OF TWA                       
         BNL   DIST010                                                          
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R4)          LOAD AND TEST FOR END                        
         BZ    DIST010                                                          
         EX    R1,*+8              COPY FIELD                                   
         B     *+10                                                             
         MVC   0(0,R7),0(R4)                                                    
*                                                                               
         SR    RF,RF               FIX FIELD ADDRESS                            
         ICM   RF,3,2(R7)                                                       
         NI    6(R7),X'FF'-X'40'   TURN OFF CURSOR FLAG                         
         CH    RF,=H'62'           UNLESS SRVREQ FIELD                          
         BE    *+12                                                             
         MVI   1(R7),X'60'         SET TO PROTECTED LOW                         
         B     DIST030                                                          
*                                                                               
         MVI   1(R7),X'48'         SER SRV TO UNPROT HIGH                       
         CLC   BYTE,TSESSION                                                    
         BNE   *+8                                                              
         OI    6(R7),X'40'         SET CURSOR FLAG IF CURRENT SESS              
*                                                                               
DIST030  AH    RF,HALF             ADD OFFSET                                   
         STCM  RF,3,2(R7)                                                       
         AR    R4,R1               BUMP TO NEXT                                 
         AR    R7,R1                                                            
         B     DIST020                                                          
*                                                                               
DIST040  MVC   0(3,R7),=X'000101'  END OF SCREEN                                
*                                                                               
         MVC   INFO,=H'8'          SET MESSAGE 8                                
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
*DISPLAY BROADCAST DATA                                               *         
***********************************************************************         
         SPACE 1                                                                
DISPBC   NTR1                                                                   
         MVC   DISPWRK1,SPACES     LABEL & DATE                                 
         MVC   DISPWRK2,SPACES                                                  
         OC    CHBCDATE,CHBCDATE                                                
         BZ    DISPBC01                                                         
         GOTO1 ADATCON,DMCB,(6,CHBCDATE),(17,DISPWRK1+0)                        
*                                                                               
         MVC   FULL,CHBCTIME       THEN TIME                                    
         BAS   RE,TIMEOUT                                                       
         MVC   DISPWRK1+8(11),WORK1                                             
*                                                                               
DISPBC01 GOTO1 AHEXOUT,DMCB,CHBCCFLG,DISPWRK1+20,2                              
         GOTO1 (RF),(R1),CHBCPNDG,DISPWRK1+31,16                                
         GOTO1 (RF),(R1),CHBCSENT,DISPWRK2+31,16                                
*                                                                               
         LA    R1,DISPWRK1+25                                                   
         EDIT  (B2,CHBCCNUM),(5,0(R1)),ZERO=NOBLANK                             
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
*DISPLAY UTLSV1 DATA                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISPUT1  NTR1                                                                   
         MVC   DISPWRK1,SPACES     LABEL & DATE                                 
*                                                                               
         OC    CHTDDATA,CHTDDATA   TEST ANY CHECKPOINT DATA                     
         BZ    DISPUT11                                                         
         GOTO1 ADATCON,DMCB,(6,CHTDDATE),(17,DISPWRK1+0)                        
*                                                                               
         MVC   FULL,CHTDTIME       THEN TIME                                    
         BAS   RE,TIMEOUT                                                       
         MVC   DISPWRK1+8(11),WORK1                                             
*                                                                               
DISPUT11 MVC   DISPWRK1+20(3),CHLNSYS                                           
         MVC   DISPWRK1+24(3),CHLNPRG                                           
         MVC   DISPWRK1+28(12),CHBILL                                           
         MVC   DISPWRK1+41(17),CHSRMSG                                          
*                                                                               
         GOTO1 AHEXOUT,DMCB,CHUTLSV3,WORK1,L'CHUTLSV3                           
         MVC   DISPWRK1+59(2),WORK1+(CHUTLDDB-CHUTLSV3)*2                       
         MVC   DISPWRK1+62(2),WORK1+(CHUTLDDL-CHUTLSV3)*2                       
         MVC   DISPWRK1+65(2),WORK1+(CHUTLSSB-CHUTLSV3)*2                       
         MVC   DISPWRK1+68(2),WORK1+(CHUTLSSL-CHUTLSV3)*2                       
         MVC   DISPWRK1+71(2),WORK1+(CHUTLSSF-CHUTLSV3)*2                       
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
*DISPLAY UTLSV2 DATA                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISPUT2  NTR1                                                                   
         MVC   DISPWRK1,SPACES                                                  
*                                                                               
         GOTO1 AHEXOUT,DMCB,CHUTLSV,WORK1,28                                    
         MVC   DISPWRK1+0(2),WORK1+(CHUTLSYS-CHUTLSV)*2                         
         MVC   DISPWRK1+2(2),WORK1+(CHUTLPRG-CHUTLSV)*2                         
         MVC   DISPWRK1+5(2),WORK1+(CHUTLPRI-CHUTLSV)*2                         
         MVC   DISPWRK1+8(2),WORK1+(CHUTLOVS-CHUTLSV)*2                         
         MVC   DISPWRK1+11(4),WORK1+(CHUTLSVC-CHUTLSV)*2                        
         MVC   DISPWRK1+25(4),WORK1+(CHUTLAUT-CHUTLSV)*2                        
         MVC   DISPWRK1+30(8),WORK1+(CHUTLACC-CHUTLSV)*2                        
         MVC   DISPWRK1+39(2),WORK1+(CHUTLAGB-CHUTLSV)*2                        
         MVC   DISPWRK1+42(2),CHUTLAGY                                          
         MVC   DISPWRK1+45(2),WORK1+(CHUTLTST-CHUTLSV)*2                        
         MVC   DISPWRK1+48(4),WORK1+(CHUTLPSW-CHUTLSV)*2                        
         LA    R1,DISPWRK1+53                                                   
         EDIT  (B4,CHUTLSIN),(7,0(R1))                                          
*                                                                               
         MVC   FULL,CHUTLTIM       UTL TIME                                     
         BAS   RE,TIMEOUT                                                       
         MVC   DISPWRK1+61(11),WORK1                                            
*                                                                               
         MVC   HALF,CHUTLUSR       UTL USER                                     
         BAS   RE,GETUSER                                                       
         MVC   DISPWRK1+16(8),WORK1                                             
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
*DISPLAY UTLSV3 DATA                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISPUT3  NTR1                                                                   
         MVC   DISPWRK1,SPACES                                                  
*                                                                               
         MVC   DISPWRK1+0(8),=C'(------)'                                       
         SR    R1,R1                                                            
         ICM   R1,7,CHUTLASY       A(SELIST)                                    
         BZ    *+10                                                             
         MVC   DISPWRK1+1(6),0(R1)                                              
*                                                                               
         MVC   DISPWRK1+9(8),=C'(------)'                                       
         SR    R1,R1                                                            
         ICM   R1,7,CHUTLAPR       A(PGMLST)                                    
         C     R1,LOCORE                                                        
         BNH   *+10                                                             
         MVC   DISPWRK1+10(6),0(R1)                                             
*                                                                               
         MVC   DISPWRK1+18(8),=C'(------)'                                      
         SR    R1,R1                                                            
         ICM   R1,7,CHUTLASV       SR A(PGMLST)                                 
         C     R1,LOCORE                                                        
         BNH   *+10                                                             
         MVC   DISPWRK1+19(6),0(R1)                                             
*                                                                               
         GOTO1 AHEXOUT,DMCB,CHUTLFLG,DISPWRK1+27,1                              
         GOTO1 (RF),(R1),CHUTLSCN+0,DISPWRK1+35,9                               
         GOTO1 (RF),(R1),CHUTLCOS,DISPWRK1+54,1                                 
         GOTO1 (RF),(R1),CHUTLACO,DISPWRK1+61,1                                 
*                                                                               
         LA    R1,DISPWRK1+30                                                   
         EDIT  (B2,CHUTLTRC),(4,0(R1))                                          
*        LA    R1,DISPWRK1+33                                                   
*        EDIT  (B2,CHUTLSNE),(5,0(R1))                                          
         LA    R1,DISPWRK1+68                                                   
         EDIT  (B2,CHUTLSES),(2,0(R1))                                          
         LA    R1,DISPWRK1+71                                                   
         EDIT  (B2,CHUTLAGN),(4,0(R1))                                          
*                                                                               
         MVC   BYTE,CHUTLLAN                                                    
         BAS   RE,GETLANG                                                       
         MVC   DISPWRK1+57(3),WORK1                                             
*                                                                               
         MVC   BYTE,CHUTLACN                                                    
         BAS   RE,GETCTRY                                                       
         MVC   DISPWRK1+64(3),WORK1                                             
*                                                                               
*        MVC   DISPWRK1+56(3),CHUTLACU                                          
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
*DISPLAY UTLSV4 DATA                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISPUT4  NTR1                                                                   
         MVC   DISPWRK1,SPACES                                                  
*                                                                               
         LA    R1,DISPWRK1+0                                                    
         EDIT  (B2,CHUTLCFN),(6,0(R1))                                          
*                                                                               
         MVC   DISPWRK1+7(2),CHUTLASC                                           
*                                                                               
         GOTO1 AHEXOUT,DMCB,CHUTLSV2,WORK1,40                                   
         MVC   DISPWRK1+10(8),WORK1+(CHUTLCLR-CHUTLSV2)*2                       
         MVC   DISPWRK1+19(8),WORK1+(CHUTLAC2-CHUTLSV2)*2                       
         MVC   DISPWRK1+31(4),WORK1+(CHUTLPER-CHUTLSV2)*2                       
         MVC   DISPWRK1+36(2),WORK1+(CHUTLST6-CHUTLSV2)*2                       
         MVC   DISPWRK1+39(2),WORK1+(CHUTLSTA-CHUTLSV2)*2                       
         MVC   DISPWRK1+49(2),WORK1+(CHUTLTYP-CHUTLSV2)*2                       
         MVC   DISPWRK1+61(8),WORK1+(CHUTLCID-CHUTLSV2)*2                       
         MVC   DISPWRK1+70(6),WORK1+(CHUTLJOB-CHUTLSV2)*2                       
*                                                                               
         MVC   DISPWRK1+42(1),CHUTLOFF                                          
         MVC   DISPWRK1+52(8),CHUTLSYM                                          
*                                                                               
         MVC   BYTE,CHUTLCTR                                                    
         BAS   RE,GETCTRY                                                       
         MVC   DISPWRK1+45(3),WORK1                                             
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
*GET LANGUAGE NAME                                                    *         
***********************************************************************         
         SPACE 1                                                                
GETLANG  NTR1                                                                   
         L     R4,ALANG            MUST HAVE A(LANGTAB)                         
         BAS   RE,SETBXLE          SET BXLE                                     
         USING LANGTABD,R4                                                      
GETLAN0  CLC   LANGCODE,BYTE       TEST LANGUAGE CODE                           
         BE    GETLAN1                                                          
         BXLE  R4,R0,GETLAN0       NEXT                                         
         MVC   WORK1,SPACES                                                     
         B     XIT1                ERROR EXIT NOT FOUND                         
GETLAN1  MVC   WORK1,LANGSHR       SET NAME                                     
         B     XIT1                                                             
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
*GET COUNTRY NAME                                                     *         
***********************************************************************         
         SPACE 1                                                                
GETCTRY  NTR1                                                                   
         L     R4,ACTRY            MUST HAVE A(CTRYTAB)                         
         BAS   RE,SETBXLE          SET BXLE                                     
         USING CTRYTABD,R4                                                      
GETCTR0  CLC   CTRYCODE,BYTE       TEST COUNTRY CODE                            
         BE    GETCTR1                                                          
         BXLE  R4,R0,GETCTR0       NEXT                                         
         MVC   WORK1,SPACES                                                     
         B     XIT1                ERROR EXIT NOT FOUND                         
GETCTR1  MVC   WORK1,CTRYSHRN      SET NAME                                     
         B     XIT1                                                             
         DROP  R4                                                               
SETBXLE  LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        GET USERID FROM 2 CHR ID NUMBER                    *                   
*************************************************************                   
         SPACE 1                                                                
GETUSER  NTR1                                                                   
         L     R7,ACTREC           BUILD ID RECORD KEY                          
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),HALF                                                 
         NI    CTIKID+8,X'7F'      UNSET GENERIC FLAG                           
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R7),(R7)                            
         CLI   8(R1),0                                                          
         BNE   GETUSR12            NNNNNN IF REC NOT FOUND                      
         LA    R7,CTIDATA                                                       
         SR    RE,RE                                                            
GETUSR10 AR    R7,RE                                                            
         CLI   0(R7),X'02'         TEST ID ELEMENT                              
         BNE   *+14                                                             
         MVC   WORK1(8),2(R7)      GET ID NAME                                  
         B     GETUSRX                                                          
*                                                                               
         IC    RE,1(R7)            BUMP TO NEXT ELEMENT                         
         CLI   0(R7),0                                                          
         BNE   GETUSR10            BUT DROP THROUGH IF LAST                     
*                                                                               
GETUSR12 EDIT  (B2,HALF),(6,WORK1),FILL=0                                       
*                                                                               
GETUSRX  B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
*EDIT TUS IN FULL TO WORK HH:MM:SS.SS                                 *         
***********************************************************************         
         SPACE 1                                                                
TIMEOUT  ST    RE,SAVERE                                                        
         MVC   WORK1(11),=C'00:00:00.00'                                        
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,=F'360000'                                                    
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'6000'                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         EDIT  (RE),(2,WORK1+9),FILL=0    100/SEC                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*CONSTANTS AND LITERALS                                               *         
***********************************************************************         
         SPACE 1                                                                
DMREAD   DC    CL8'DMREAD'                                                      
TEMPSTR  DC    CL8'TEMPSTR'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
SPACES   DC    CL80' '                                                          
*                                                                               
$CT      DC    X'0110'                                                          
$XMTALL  DC    X'0126'                                                          
*                                                                               
SSACBITS DC    X'0102040810204080'                                              
SSNABITS DC    X'FEFDFBF7EFDFBF7F'                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
SRSYNTAB DS    0XL12                                                            
         DC    AL1(2),CL3'S+ ',CL4'SS,+',AL4(SSSPLS0)                           
         DC    AL1(2),CL3'S- ',CL4'SS,-',AL4(SSSMNS0)                           
         DC    AL1(3),CL3'SVU',CL4'SS,U',AL4(SSSV0)                             
         DC    AL1(2),CL3'SV ',CL4'SS,V',AL4(SSSV0)                             
         DC    AL1(2),CL3'SW ',CL4'SS,W',AL4(SSSW0)                             
         DC    AL1(2),CL3'RE ',CL4'SS,L',AL4(SSSL0)                             
         DC    AL1(2),CL3'CL ',CL4'SS,*',AL4(SSSNOP0)                           
         DC    AL1(2),CL3'SA ',CL4'SS,A',AL4(SSS)                               
         DC    AL1(2),CL3'SB ',CL4'SS,B',AL4(SSS)                               
         DC    AL1(2),CL3'SC ',CL4'SS,C',AL4(SSS)                               
         DC    AL1(2),CL3'SD ',CL4'SS,D',AL4(SSS)                               
         DC    AL1(2),CL3'SE ',CL4'SS,E',AL4(SSS)                               
         DC    AL1(2),CL3'SF ',CL4'SS,F',AL4(SSS)                               
         DC    AL1(2),CL3'SG ',CL4'SS,G',AL4(SSS)                               
         DC    AL1(0),CL3'   '                                                  
         EJECT                                                                  
***********************************************************************         
*SCREEN HEADLINE TEXT                                                 *         
***********************************************************************         
         SPACE 1                                                                
*              C'....+....1....+....2....+....3....+....4'                      
*              C'....+....5....+....6....+....7....+...'                        
BCSHDR   DC    C'S---Bcst Date/Time--- Flag Hinum -------'                      
         DC    C'-----Bit Mask------------             '                        
SV2HDR   DC    C'S-SyPg Pi Ov Srvq -UserId- Auth -LimAcc-'                      
         DC    C' Ag AG TS Pswd --Sin-- --Last Trn--   '                        
SV3HDR   DC    C'S-(SELIST) (PGMLST) (SRPGML) Fl Ttrc ---'                      
         DC    C'--------------- Ov Lan Op Ctr Se Agrp '                        
SV4HDR   DC    C'S-Number Sc -Colour- -Limac2- Sw Pers S6'                      
         DC    C' St Of Ctr Ty --Luid-- --CID--- -Job--'                        
SV1HDR   DC    C'S---Actv Date/Time--- Sys Prg --Billref-'                      
         DC    C'-- ----CT String---- A1 DD SS SL SF   '                        
DUMMY    DS    0CL86                                                            
         DC    X'5668000000000000'                                              
         DC    78C'-'                                                           
         EJECT                                                                  
***********************************************************************         
*WORKING STORAGE                                                      *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
ACTN     DS    X                                                                
DMCB     DS    6F                                                               
WORK     DS    XL64                                                             
WORK1    DS    XL128                                                            
*                                                                               
INFO     DS    H                                                                
ERROR    DS    H                                                                
*                                                                               
RELO     DS    A                                                                
ATWASVR  DS    A                                                                
AGETTXT  DS    A                                                                
ADATCON  DS    A                                                                
AHEXOUT  DS    A                                                                
ATIA     DS    A                                                                
ATIOB    DS    A                                                                
ATWA     DS    A                                                                
ACTRY    DS    A                                                                
AFID     DS    A                                                                
ALANG    DS    A                                                                
ACTREC   DS    A                                                                
SAVERE   DS    A                                                                
SAVERD   DS    A                                                                
*                                                                               
ASAVED   DS    0XL20                                                            
ASAVE1   DS    A                                                                
ASAVE2   DS    A                                                                
ASAVE3   DS    A                                                                
ASAVE4   DS    A                                                                
ASAVEX   DS    A                                                                
*                                                                               
RECLEN   DS    H                                                                
CHKDSP   DS    H                                                                
SSMAX    DS    H                   MAX NUM OF LOGICAL  SESSIONS                 
SSMXP    DS    H                   MAX NUM OF PHYSICAL SESSIONS                 
SSPGS    DS    H                                                                
SSDATE   DS    PL4                                                              
SYNAME   DS    CL4                                                              
LOCORE   DS    A                                                                
*                                                                               
CURDSP   DS    H                   20/SSMAX * 80                                
TWADSP   DS    H                   20/SSMAX * 80 - 80                           
*                                                                               
CURADR   DS    XL2                                                              
PFKEY    DS    X                                                                
*                                                                               
SUSER    DS    H                                                                
SPASSWD  DS    H                                                                
SAGY     DS    H                                                                
SSYS     DS    X                                                                
SPRG     DS    X                                                                
SVTSVC   DS    XL2                                                              
SVTRCNT  DS    XL2                                                              
SVFLAG   DS    XL1                                                              
SVSESS   DS    XL1                                                              
*                                                                               
SESS     DS    XL1                                                              
SAVESESS DS    XL1                                                              
RSRVSESS DS    XL1                                                              
TRANSESS DS    XL1                                                              
LASTSESS DS    XL1                                                              
PREVSESS DS    XL1                                                              
STOLEN   DS    XL1                                                              
XCTL     DS    XL1                                                              
STEREO   DS    XL1                                                              
STEREO8  DS    XL1                                                              
STIN1    DS    CL1                                                              
STIN2    DS    CL1                                                              
STIN3    DS    CL1                                                              
*                                                                               
NUM1SESS DS    X                                                                
NUM2SESS DS    X                                                                
NUM3SESS DS    X                                                                
NUM4SESS DS    X                                                                
*                                                                               
ERRNUM   DS    XL1                                                              
FLAG     DS    CL1                                                              
SRFLD    DS    CL17                                                             
MSG      DS    CL60                                                             
DISPWRK1 DS    CL78                                                             
DISPWRK2 DS    CL78                                                             
*                                                                               
SAVECHK1 DS    XL(CHKPTLEN+1)                                                   
SAVECHK2 DS    XL(CHKPTLEN+1)                                                   
SAVECHK3 DS    XL(CHKPTLEN+1)                                                   
SAVECHK4 DS    XL(CHKPTLEN+1)                                                   
*                                                                               
CTREC    DS    XL2048                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*OTHER DSECTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
LINED    DSECT                                                                  
LINEH    DS    XL8                                                              
LINEHDR  DS    0CL78                                                            
LINEL1   DS    CL2                                                              
LINEL    DS    CL76                                                             
LINELEN  EQU   *-LINED                                                          
         SPACE 1                                                                
SRSESFFD DSECT                                                                  
         DS    CL64                                                             
*SRSESFFD                                                                       
       ++INCLUDE SRSESFFD                                                       
         SPACE 2                                                                
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
         SPACE 2                                                                
*FACHKPT                                                                        
         PRINT OFF                                                              
       ++INCLUDE FACHKPT                                                        
         PRINT OFF                                                              
         SPACE 2                                                                
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT OFF                                                              
         SPACE 2                                                                
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT OFF                                                              
*SRDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE SRDDEQUS                                                       
         PRINT OFF                                                              
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SRSES00   01/05/21'                                      
         END                                                                    
