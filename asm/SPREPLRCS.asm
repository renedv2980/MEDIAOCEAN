*          DATA SET SPREPLRCS  AT LEVEL 083 AS OF 02/10/04                      
*PHASE SPLR02C,+0,NOAUTO                                                        
         TITLE 'SPREPLRCS CHANGE BBM STATION CALL LETTERS'                      
         PRINT NOGEN                                                            
SPLR02   CSECT                                                                  
         NMOD1 0,SPLR02,RR=R5                                                   
*                                                                               
         LA    R4,2048(RB)         R4 IS SECOND BASE REGISTER                   
         LA    R4,2048(R4)                                                      
         USING SPLR02,RB,R4                                                     
*                                                                               
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVC   FLTBOOK,=X'6320'                                                 
         MVC   FLTBOOKR,FLTBOOK                                                 
         XC    FLTBOOKR,=X'FFFF'                                                
         OPEN  (OUT,(OUTPUT))                                                   
         MVI   FORCEHED,C'Y'                                                    
         LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVC   DRCODE(3),=C'MCA'                                                
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         B     SPLM6                                                            
         SPACE 2                                                                
SPLM2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         CLI   8(R1),0             READ OK?                                     
         BNE   SPLR0               EOF                                          
SPLM6    CLC   DRCODE(3),=C'MCA'                                                
         BNE   SPLR0                                                            
         MVC   SVSPLKEY,SPLKEY                                                  
         MVC   PRVKEY,SPLKEY                                                    
         LA    R7,SVSPLKEY                                                      
         USING MLKEY,R7                                                         
         CLI   MLIND,0             IS THIS A STATION LIST KEY                   
         BNE   SPLM2                                                            
         CLC   MLBOOK,FLTBOOKR                                                  
         BH    SPLM2                                                            
         OC    MLKMKT,MLKMKT                                                    
         BNZ   SPLM2                                                            
         LA    RE,STAFIXT                                                       
MLLOOP   CLI   0(RE),X'FF'                                                      
         BE    SPLM2                                                            
         CLC   MLSTAT(4),0(RE)                                                  
         BE    *+12                                                             
         LA    RE,L'STAFIXT(RE)                                                 
         B     MLLOOP                                                           
         MVC   MLSTAT(4),4(RE)                                                  
         MVC   MLKMKT,MLRMKT                                                    
*                                                                               
SPLMM2   MVC   P(5),MLSTAT                                                      
         EDIT  (B2,MLRMKT),(4,P+6)                                              
         EDIT  (B2,MLKMKT),(4,P+10)                                             
         MVC   P+20(5),=C'MLKEY'                                                
         GOTO1 REPORT                                                           
         MVC   SVSPLKEY+20(4),=X'FFFF0000'                                      
         MVC   RECIOLN,=X'001B0000'                                             
         MVC   RECIO(24),SVSPLKEY                                               
         BAS   RE,PUTOUT                                                        
         DROP  R7                                                               
         B     SPLM2                                                            
*                                                                               
SPLR0    LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
         XC    CCOUNT,CCOUNT                                                    
         XC    SPLKEY,SPLKEY                                                    
         MVC   DRCODE(3),=C'MCA'                                                
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         B     SPLR6                                                            
         SPACE 2                                                                
SPLR2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         CLI   8(R1),0             READ OK?                                     
         BNE   SPLS0               EOF                                          
SPLR6    CLC   DRCODE(3),=C'MCA'                                                
         BNE   SPLS0                                                            
         MVC   SVSPLKEY,SPLKEY                                                  
         MVC   PRVKEY,SPLKEY                                                    
         LA    R7,SVSPLKEY                                                      
         USING BSKEY,R7                                                         
         CLI   BSIND,2             IS THIS A STATION LIST KEY                   
         BNE   SPLR2                                                            
         CLC   BSBOOK,FLTBOOKR                                                  
         BH    SPLR2                                                            
         OC    BSKMKT,BSKMKT                                                    
         BNZ   SPLR2                                                            
         LA    RE,STAFIXT                                                       
BSLOOP   CLI   0(RE),X'FF'                                                      
         BE    SPLR2                                                            
         CLC   BSSTAT(4),0(RE)                                                  
         BE    *+12                                                             
         LA    RE,L'STAFIXT(RE)                                                 
         B     BSLOOP                                                           
         MVC   BSSTAT(4),4(RE)                                                  
         MVC   BSKMKT,BSRMKT                                                    
*                                                                               
SPLRM2   MVC   P(5),BSSTAT                                                      
         EDIT  (B2,BSRMKT),(4,P+6)                                              
         EDIT  (B2,BSKMKT),(4,P+10)                                             
         MVC   P+20(5),=C'BSKEY'                                                
         GOTO1 REPORT                                                           
         MVC   SVSPLKEY+20(4),=X'FFFF0000'                                      
         MVC   RECIOLN,=X'001B0000'                                             
         MVC   RECIO(24),SVSPLKEY                                               
         BAS   RE,PUTOUT                                                        
         DROP  R7                                                               
         B     SPLR2                                                            
*                                                                               
         SPACE 2                                                                
SPLS0    LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
         XC    CCOUNT,CCOUNT                                                    
         XC    SPLKEY,SPLKEY                                                    
         MVC   DRCODE(3),=C'SCA'                                                
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         B     SPLS6                                                            
         SPACE 2                                                                
SPLS2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         CLI   8(R1),0             READ OK?                                     
         BNE   SPLD0               EOF                                          
SPLS6    CLC   DRCODE(3),=C'SCA'                                                
         BNE   SPLD0                                                            
         MVC   SVSPLKEY,SPLKEY                                                  
         MVC   PRVKEY,SPLKEY                                                    
         LA    R7,SVSPLKEY                                                      
         USING SBKEY,R7                                                         
         CLC   SBBOOK,FLTBOOK                                                   
         BL    SPLS2                                                            
         LA    RE,STAFIXT                                                       
STLOOP   CLI   0(RE),X'FF'                                                      
         BE    SPLS2                                                            
         CLC   SBSTAT(4),0(RE)                                                  
         BE    *+12                                                             
         LA    RE,L'STAFIXT(RE)                                                 
         B     STLOOP                                                           
         MVC   SBSTAT(4),4(RE)                                                  
         MVC   SBKMKT,SBRMKT                                                    
*                                                                               
SPLSM2   MVC   P(5),SBSTAT                                                      
         EDIT  (B2,SBRMKT),(4,P+6)                                              
         EDIT  (B2,SBKMKT),(4,P+10)                                             
         MVC   P+20(5),=C'SBKEY'                                                
         GOTO1 REPORT                                                           
         MVC   SVSPLKEY+20(4),=X'FFFF0000'                                      
         MVC   RECIOLN,=X'001B0000'                                             
         MVC   RECIO(24),SVSPLKEY                                               
         BAS   RE,PUTOUT                                                        
         DROP  R7                                                               
         B     SPLS2                                                            
*                                                                               
         EJECT                                                                  
SPLD0    LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVC   DRCODE(3),=C'RCA'                                                
         MVC   SVSPLKEY,SPLKEY                                                  
         XC    RCOUNT,RCOUNT                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         MVC   HLDSTA,DRSTAT                                                    
         B     SPLD6                                                            
         SPACE 1                                                                
SPLD2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         CLI   8(R1),0                                                          
         BNE   SPRR0                                                            
SPLD6    CLC   HLDSTA,DRSTAT                                                    
         BE    SPLD7                                                            
         OC    RCOUNT,RCOUNT                                                    
         BZ    SPLD7                                                            
         MVC   P(5),HLDSTA                                                      
         MVC   P+7(4),=CL4'GERM'                                                
         EDIT  (B4,RCOUNT),(6,P+20)                                             
         XC    RCOUNT,RCOUNT                                                    
         MVC   HLDSTA,DRSTAT                                                    
         GOTO1 REPORT                                                           
SPLD7    XC    CCOUNT,CCOUNT                                                    
         CLC   DRCODE(3),=C'RCA'                                                
         BNE   SPRR0                                                            
         CLC   DRBOOK,FLTBOOKR                                                  
         BH    SPLD2                                                            
         LA    RE,STAFIXT                                                       
SPLOOP   CLI   0(RE),X'FF'                                                      
         BE    SPLD2                                                            
         CLC   DRSTAT(4),0(RE)                                                  
         BE    *+12                                                             
         LA    RE,L'STAFIXT(RE)                                                 
         B     SPLOOP                                                           
*        CLI   DRBTYP,0                                                         
*        BE    SPLD2                                                            
*        CLI   DRBTYP,X'E0'                                                     
*        BE    SPLD2                                                            
         MVC   SVDA,DRNDXDA                                                     
         BAS   RE,SPLF0                                                         
         B     SPLD2                                                            
         SPACE 1                                                                
SPLF0    NTR1                                                                   
         MVC   RECIO(20),SPLKEY                                                 
         XC    RCOUNT,RCOUNT                                                    
         XC    DCOUNT,DCOUNT                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMFIL ',SVDA,RECIO                   
         B     SPLF2                                                            
SPLF1    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMFIL ',SVDA,RECIO                   
SPLF2    CLI   8(R1),0                                                          
         BNE   SPLFEX                                                           
         LA    R6,RECIO                                                         
         USING DRKEY,R6                                                         
         CLC   DRBOOK,FLTBOOKR                                                  
         BH    SPLFEX                                                           
         LA    R7,DRFRSTEL                                                      
         USING MARELEM,R7                                                       
         LA    RE,STAFIXT                                                       
DRLOOP   CLI   0(RE),X'FF'                                                      
         BE    SPLFEX                                                           
         CLC   DRSTAT(4),0(RE)                                                  
         BE    *+12                                                             
         LA    RE,L'STAFIXT(RE)                                                 
         B     DRLOOP                                                           
         MVC   DRSTAT(4),4(RE)                                                  
         MVC   DRKMKT(2),MARNO                                                  
*                                                                               
*                                                                               
SPLFM2   L     RF,CCOUNT                                                        
         C     RF,=F'0'                                                         
         BNE   SPLF5                                                            
         MVC   P(5),DRSTAT                                                      
         EDIT  (B2,MARNO),(4,P+6)                                               
         EDIT  (B2,DRKMKT),(4,P+10)                                             
         MVC   P+20(5),=C'DRKEY'                                                
         GOTO1 REPORT                                                           
*                                                                               
SPLF5    LA    R6,RECIO                                                         
         XC    RECIOLN,RECIOLN                                                  
         SR    RE,RE                                                            
         ICM   RE,3,DRRLEN                                                      
         LA    RE,4(RE)                                                         
         STCM  RE,3,RECIOLN                                                     
*                                                                               
         L     RE,RCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,RCOUNT                                                        
         BAS   RE,PUTOUT                                                        
         MVC   DRBTYP,BKTYPE                                                    
         B     SPLF1                                                            
*                                                                               
         DROP  R6,R7                                                            
SPLFEX   XIT1                                                                   
         EJECT                                                                  
PUTOUT   NTR1                                                                   
         LA    R5,RECIOLN                                                       
         L     R7,=A(OUT)                                                       
         PUT   (R7),(R5)                                                        
         L     RE,OCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OCOUNT                                                        
         L     RE,CCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,CCOUNT                                                        
         C     RE,=F'6'                                                         
         BH    PUTOUTX                                                          
         GOTO1 HEXOUT,DMCB,RECIOLN,P,60                                         
         GOTO1 REPORT                                                           
PUTOUTX  XIT1                                                                   
         EJECT                                                                  
SPRR0    DS    0H                                                               
EXIT1    CLOSE (OUT)                                                            
         EDIT  (B4,OCOUNT),(6,P)                                                
         GOTO1 REPORT                                                           
EXIT     XMOD1 1                                                                
         SPACE 2                                                                
SPLRRA DC      F'0'                                                             
SPLRRB DC      F'0'                                                             
SPLRRC DC      F'0'                                                             
OCOUNT DC      F'0'                                                             
CCOUNT DC      F'0'                                                             
RCOUNT DC      F'0'                                                             
DCOUNT DC      F'0'                                                             
         SPACE 2                                                                
         EJECT                                                                  
RELO     DC    F'0'                                                             
RANKCTR  DC    F'0'                                                             
SPILCTR  DC    F'0'                                                             
LOOPCTR  DC    F'0'                                                             
USTOTAL  DC    F'0'                                                             
INDXCTR  DC    F'0'                                                             
LASTRANK DC    F'0'                                                             
FLTBOOK  DS    CL2                                                              
FLTBOOKR DS    CL2                                                              
PRVSTAT  DS    CL5                                                              
PRVMKT   DS    CL2                                                              
PVSMS    DS    CL7                                                              
PRVKEY   DS    CL24                                                             
SPLKEY   DS    CL24                                                             
SVSPLKEY DS    CL24                                                             
SVDA     DS    CL4                                                              
HLDSTA   DS    CL5                                                              
HRNKLEN  DC    AL2(RANKLEN)                                                     
RPTYPE   DS    C                                                                
BKTYPE   DS    C                                                                
RECIOLN  DS    CL4                                                              
RECIO    DS    2000C                                                            
         LTORG                                                                  
         EJECT                                                                  
*     BOOK TYPES ARE ASSIGNED AS FOLLW                                          
*         H=HISPANIC                                                            
*         B=BLACK                                                               
*         T=TRADING AREA                                                        
*         M=METRO                                                               
*         N=NETWORK AFFILIATES ONLY                                             
*         S=SCANAMERICA / TMG SCANNER (ARB ONLY)                                
*         E=EXTENDED AREA                                                       
*                                                                               
* TABLE OF MARKETS                                                              
*                                                                               
MARKETS  DS    0XL06                                                            
*        DC    CL4'CHBX',AL2(5531)                                              
*        DC    CL4'CHMI',AL2(6119)                                              
*        DC    CL4'CFRE',AL2(7071)                                              
*        DC    CL4'CFAP',AL2(4199)                                              
*        DC    CL4'CBLN',AL2(5369)                                              
*        DC    CL4'CFPL',AL2(5369)                                              
*        DC    CL4'CIPA',AL2(7153)                                              
*        DC    CL4'CFSK',AL2(7109)                                              
*        DC    CL4'CJPM',AL2(4120)                                              
*        DC    CL4'CKRS',AL2(4120)                                              
*        DC    CL4'CIHF',AL2(2080)                                              
*        DC    CL4'CKKX',AL2(8069)                                              
*        DC    CL4'CFAC',AL2(8069)                                              
*        DC    CL4'CFTF',AL2(4101)                                              
*        DC    CL4'CFER',AL2(4061)                                              
*        DC    CL4'CJBR',AL2(4061)                                              
*        DC    CL4'WNED',AL2(5199)                                              
*<       DC    CL4'CICT',AL2(8069)                                              
*<       DC    CL4'CHRO',AL2(5069)                                              
*<<      DC    CL4'CJON',AL2(0009)                                              
*<       DC    CL4'CFER',AL2(4061)                                              
*<<      DC    CL4'CBAT',AL2(3011)                                              
*        DC    CL4'WPTZ',AL2(9760)                                              
*        DC    CL4'WVNY',AL2(9760)                                              
*        DC    CL4'WCAX',AL2(9760)                                              
*        DC    CL4'WWNY',AL2(9764)                                              
*        DC    CL4'WHEC',AL2(9765)                                              
*        DC    CL4'WUTV',AL2(9762)                                              
*        DC    CL4'KHQ ',AL2(9763)                                              
*<<      DC    CL4'CJBR',AL2(4061)                                              
*<<<     DC    CL4'KCPQ',AL2(9109)                                              
*        DC    CL4'KCTS',AL2(9109)                                              
*        DC    CL4'KOMO',AL2(9109)                                              
*        DC    CL4'KSTW',AL2(9109)                                              
*        DC    CL4'WKBD',AL2(5409)                                              
*        DC    CL4'WXON',AL2(5409)                                              
*        DC    CL4'KAYU',AL2(9763)                                              
*        DC    CL4'WWTI',AL2(5109)                                              
*        DC    CL4'WCFE',AL2(4479)                                              
*<<<     DC    CL4'WETK',AL2(4479)                                              
*        DC    CL4'CJON',AL2(0009)                                              
*        DC    CL4'KAYU',AL2(9763)                                              
*        DC    CL4'CJBN',AL2(5565)                                              
*        DC    CL4'CBC ',AL2(9802)                                              
*        DC    CL4'CKBI',AL2(9710)                                              
*        DC    CL4'CBAT',AL2(3011)                                              
*        DC    CL4'CECO',AL2(5145)                                              
*        DC    CL4'BBS ',AL2(9710)                                              
*                                                                               
*        DC    CL4'ACCE',AL2(8119)                                              
*        DC    CL4'CHWI',AL2(5409)                                              
******<MAR99                                                                    
*        DC    CL4'CIVT',AL2(9109)                                              
*        DC    CL4'CKEM',AL2(8119)                                              
*        DC    CL4'ACCE',AL2(8119)                                              
*        DC    CL4'CKAL',AL2(8069)                                              
*        DC    CL4'CKMI',AL2(4479)                                              
*        DC    CL4'CBAT',AL2(3011)                                              
*        DC    CL4'CJON',AL2(0009)                                              
*        DC    CL4'CBKT',AL2(7071)                                              
*        DC    CL4'CKCK',AL2(7071)                                              
*        DC    CL4'CFRE',AL2(7071)                                              
*****************MAY99                                                          
*        DC    CL4'CBC ',AL2(9802)                                              
*        DC    CL4'WFFF',AL2(4479)                                              
************* NOV/99                                                            
         DC    CL4'CTV ',AL2(9710)                                              
         DC    CL4'CKTV',AL2(4120)                                              
*                                                                               
MARKETSX DC    AL2(0)                                                           
*                                                                               
STAFIXT  DS    0CL8                                                             
         DC    C'BVAN',C'BRAV'        BRAVO                                     
         DC    C'CXVA',C'COME'        COMEDY                                    
         DC    C'MVAN',C'DSCY'        DISCOVERY                                 
         DC    C'HVAN',C'SHOW'        SHOWCASE                                  
         DC    C'HXVA',C'HIST'        HISTORY                                   
         DC    C'JVAN',C'HSN '        HEADLINE SPORTS NEWS                      
         DC    C'JXVA',C'SPNP'        SPORTSNET                                 
         DC    C'KVAN',C'FAIR'        FAIRCHILD                                 
         DC    C'LVAN',C'LIFE'        LIFE NETWORK                              
         DC    C'LXVA',C'TOON'        TELETOON                                  
         DC    C'MVAN',C'MUCH'        MUCH MUSIC                                
         DC    C'NVAN',C'CBNW'        CBC NEWSWORLD                             
         DC    C'NXVA',C'NWS '        CTV NEWS ONE                              
         DC    C'OVAN',C'WTN '        WOMEN'S TV NWK                            
         DC    C'PXVA',C'PRME'        PRIME                                     
         DC    C'QXVA',C'MMM '        MUCH MORE MUSIC                           
         DC    C'SVAN',C'TSN '        SPORTS NET                                
         DC    C'SXVA',C'SPAC'        SPACE                                     
         DC    C'UXVA',C'OLN '        OUTDOOR LIFE NET                          
         DC    C'VVAN',C'VTV '        VISION                                    
         DC    C'WVAN',C'WNW '        WEATHER NETWORK                           
         DC    C'YVAN',C'YTV '        YTV                                       
         DC    C'YXVA',C'CMT '        CMT                                       
*        DC    C'B017',C'CHNM'        CHMN                                      
         DC    C'B970',C'TVAN'        TVAN                                      
         DC    C'B996',C'TREE'        TREE                                      
         DC    C'B997',C'FOOD'        FOOD                                      
         DC    X'FFFF'                                                          
         EJECT                                                                  
OUT      DCB   DDNAME=OUT,DSORG=PS,RECFM=VB,LRECL=2000,                X        
               BLKSIZE=16000,MACRF=PM                                           
         LTORG                                                                  
         SPACE 2                                                                
RANKTAB  DS    10000C                                                           
SPILTAB  DS    100000C                                                          
*                                                                               
RANKTABD DSECT                                                                  
RMKT     DS    CL2                                                              
RNKUNV   DS    CL4                                                              
RNKPCT   DS    CL4                                                              
RANK     DS    CL2                                                              
SMARANK  DS    CL2                                                              
RMKTNAM  DS    CL30                                                             
RNKTABEN DS    0C                                                               
RANKLEN  EQU   RNKTABEN-RMKT                                                    
         SPACE 2                                                                
SPILTABD DSECT                                                                  
SMS      DS    0CL7                                                             
SMKT     DS    CL2                                                              
SSTA     DS    CL5                                                              
SBOOK    DS    CL2                                                              
SPILEND  DS    0C                                                               
SPILLEN  EQU   SPILEND-SMKT                                                     
         SPACE 2                                                                
HOMETABD DSECT                                                                  
HSTA     DS    CL5                                                              
HMKT     DS    CL2                                                              
HOMTABEN DS    0C                                                               
HOMELEN  EQU   HOMTABEN-HSTA                                                    
         SPACE 2                                                                
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1                                                              
PLMNUM   DS    CL4                                                              
         DS    CL3                                                              
PLMNAME  DS    CL30                                                             
         DS    CL3                                                              
PLMRANK  DS    CL3                                                              
         DS    CL3                                                              
PLSRANK  DS    CL3                                                              
         DS    CL2                                                              
PLUSPCT  DS    CL6                                                              
         DS    CL1                                                              
PLHMSUNV DS    CL11                                                             
         SPACE 2                                                                
PSLINE   DSECT                                                                  
         DS    CL3                                                              
PSSTA    DS    CL4                                                              
         DS    CL2                                                              
PSMKTNUM DS    CL4                                                              
         DS    CL2                                                              
PSMKTNAM DS    CL30                                                             
         DS    CL4                                                              
PSBOOKS  DS    CL80                                                             
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
*        PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083SPREPLRCS 02/10/04'                                      
         END                                                                    
