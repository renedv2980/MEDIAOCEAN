*          DATA SET SPNWS06    AT LEVEL 160 AS OF 02/26/07                      
*PHASE T20706C,*                                                                
         TITLE 'BWS06- BWS WORK OVERLAY FOR SID OPTION AND DEMO ACTION'         
T20706   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20706**,RA,R9,RR=RE                                           
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
******** LA    R1,0(R6)                                                         
******** AH    R1,=Y(SAVAREAX-SAVAREA)                                          
******** ST    R1,LARECTAB         USE END OF TWA FOR RECORD TABLE              
         L     R1,ATIA             USE TIA FOR RECORD TABLE                     
         ST    R1,LARECTAB                                                      
         ST    R6,LASAVE                                                        
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         MVC   APPARM,COMPARM      RESTORE APPARM                               
         LA    R2,IOKEY                                                         
         USING BWHRECD,R2                                                       
         LA    R3,APRECKEY                                                      
         USING BWDRECD,R3                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         DC    AL4(0)  VALKEY                                                   
         DC    AL4(0)  VALREC                                                   
         DC    AL4(0)  DISKEY                                                   
         DC    AL4(0)  DISREC                                                   
         DC    AL4(0)  DELREC                                                   
         DC    AL4(0)  RESREC                                                   
         B     VALPAR                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     VALSEL                                                           
         B     EXIT    FSTLST                                                   
         B     PROCLS                                                           
         B     FSTSCR                                                           
         B     LSTSCR                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     CLC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1  ,                                                                
*                                                                               
EXITR8   XIT1  REGS=(R8)                                                        
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
* OUTPUT : FVMSGNO NE FVFOK IF KEY IS INVALID                         *         
*          APRECKEY                                                   *         
*          APPARM FOR ROOT                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPAR   GOTO1 AVALPARM,BWSKY2H    VALIDATE SELECT PARAMETERS                   
         BE    VALP2                                                            
         TM    TWAFLAG,TWANODET    TEST FOR DETAIL RECORDS                      
         BZ    VALPX                                                            
         CLI   INOSID,0            NO-TEST FOR SID OPTION                       
         BE    VALPX                  NO-NO RECORDS TO LIST                     
         OC    CMPSCHEM,CMPSCHEM      YES-CHECK CAMPAIGN FOR NSID               
         BZ    ESCH                       SCHEME                                
*                                                                               
VALP2    MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    LFLAG,LNEWMIN                                                    
         CLI   SCPFKEY,255         TEST LFM ACTION JUST COMPLETED               
         BNE   VALP4                                                            
         TM    TWAACTIV,TWAACXFR   TEST FOR XFR WITHOUT LFM                     
         BZ    *+12                                                             
         NI    TWAACTIV,FF-TWAACXFR                                             
         B     VALP4                                                            
         OI    TWAACTIV,TWAACLFM                                                
         CLI   APPFKEY,PFK05       HONOR PFKEY 5                                
         BNE   VALP4                                                            
         MVI   SCPFKEY,PFK05                                                    
*                                                                               
VALP4    CLI   BSLN,0              TEST SPOT LENGTH FILTER                      
         BE    *+12                                                             
         CLI   INOSLN,0            YES-TEST SLN OPTION                          
         BNE   ESLNOP              YES-INVALID OPTION                           
*                                                                               
         BAS   RE,VALPFKEY         VALIDATE PF KEY                              
*                                                                               
         CLI   APACTN,ACTDEM       FOR DEMO ACTION --                           
         BNE   VALPX                                                            
         LA    RE,ESTDEMS          VALIDATE DEMO OPTION                         
         MVI   APBYTE,0            APBYTE=0 1ST ATTEMPT TO BUILD DEMOS          
         CLI   INODEM,0            TEST FOR DEMO OPTION PRESENT                 
         BE    VALP6               NO-USE FIRST DEMOS                           
         TM    INODEM,X'80'        YES-TEST FOR NEXT                            
         BZ    VALP18                                                           
         NI    INODEM,FF-X'80'     YES-                                         
         ZIC   RF,INODEM                                                        
         BCTR  RF,0                                                             
         MH    RF,=H'12'                                                        
         AR    RE,RF                                                            
         OC    0(3,RE),0(RE)                                                    
         BNZ   VALP6                                                            
         LA    RE,ESTDEMS                                                       
*                                                                               
VALP6    LA    RF,INODEM+1         MOVE DEMOS TO DEMO OPTION                    
         LA    R4,4                                                             
*                                                                               
VALP8    OC    0(3,RE),0(RE)       TEST FOR END OF DEMOS                        
         BZ    VALP16                                                           
         CLI   APBYTE,0            TEST FOR 1ST ATTEMPT                         
         BNE   VALP14                                                           
         OC    INORTG,INORTG       TEST FOR OVERRIDE TARGET DEMO                
         BZ    VALP10                                                           
         CLC   INORTG+1(2),1(RE)   YES - IS THIS THE TARGET DEMO ?              
         BE    VALP12                    YES - SKIP IT                          
         B     VALP14                                                           
*                                                                               
VALP10   LA    R0,ESTDEMS          NO - IS THIS THE PRIMARY DEMO ?              
         CR    R0,RE                    YES - SKIP IT                           
         BNE   VALP14                                                           
*                                                                               
VALP12   LA    RE,3(RE)                                                         
         B     VALP8                                                            
*                                                                               
VALP14   MVC   0(3,RF),0(RE)                                                    
         LA    RE,3(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R4,VALP8                                                         
*                                                                               
VALP16   MVI   0(RF),FF            END OF LIST                                  
         LNR   R4,R4                                                            
         LA    R4,4(R4)                                                         
         STC   R4,INODEM           NUMBER OF DEMOS                              
         LTR   R4,R4               TEST FOR ZERO                                
         BNZ   VALP24                                                           
         MVI   APBYTE,1            YES - 2ND ATTEMPT, INCLUDE ALL DEMOS         
         LA    RE,ESTDEMS                                                       
         B     VALP6                                                            
*                                                                               
VALP18   LA    R1,INODEM+1         VALIDATE DEMOS IN THE DEMO OPTION            
         ZIC   R0,INODEM                                                        
*                                                                               
VALP20   LA    RE,ESTDEMS                                                       
*                                                                               
VALP22   OC    0(3,RE),0(RE)                                                    
         BZ    EDEMOP                                                           
         CLC   1(2,RE),1(R1)                                                    
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     VALP22                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,VALP20                                                        
*                                                                               
VALP24   MVI   APPARM+4,NLSTLINS   NUMBER OF LIST LINES                         
         L     R1,ALSM                                                          
         USING LSMD,R1                                                          
         TM    LSMINDS,LSMISEL     IF USER NOW INVITED TO ENTER                 
         BZ    VALPX               SELECTIONS,                                  
         CLC   APRECKEY,LSMUKEY    AND THE KEY HAS NOT CHANGED --               
         BNE   VALPX                                                            
         DROP  R1                                                               
         LA    R0,NDEMLINS                                                      
         SR    R1,R1                                                            
         LA    RE,WRKL8H+DLINEDSP                                               
         LA    R4,WRKL1H                                                        
         USING WRKL1H,R4                                                        
*                                                                               
VALP26   LR    RF,RE               FOR ALL DEMO LINES,                          
         AH    RF,TWADMLNL                                                      
*                                                                               
VALP28   CR    RE,RF               SCAN THE FIELDS                              
         BE    VALP32                                                           
         BL    *+6                                                              
         DC    H'0'                                                             
         TM    FVATRB-FVIHDR(RE),FVAPROT    TEST FOR UNPROTECTED                
         BO    VALP30                                                           
         TM    FVIIND-FVIHDR(RE),FVIVAL     TEST FOR NOT PREV VALIDATED         
         BO    VALP30                                                           
         NI    WRKPRGH+FVIIND-FVIHDR,FF-FVIVAL TURN OFF PREV VALIDATED          
         B     VALP32                          BIT IN CORRESP LIST LINE         
*                                                                               
VALP30   ICM   R1,1,0(RE)          NEXT FIELD                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RE,R1                                                            
         B     VALP28                                                           
*                                                                               
VALP32   LR    RE,RF               NEXT DEMO LINE                               
         LA    R4,WRKL2H-WRKL1H(R4)                                             
         BCT   R0,VALP26                                                        
         DROP  R4                                                               
*                                                                               
         LA    R1,SAVOPTS          SAVE DEMO OPTION IN OPTION SAVE AREA         
         LA    R1,INODEM-INOPTS(R1)                                             
         MVC   0(L'INODEM,R1),INODEM                                            
         B     VALPX                                                            
*                                                                               
VALPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PF KEY                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALPFKEY NTR1  ,                                                                
         CLI   BSLN,0              ONLY FOR ALL SPOT LENGTHS,                   
         BNE   VPFKX                                                            
         CLI   INOSLN,0            AND NO SLN OPTION,                           
         BNE   VPFKX                                                            
         L     R8,ALSM             AND WHEN USER INVITED TO ENTER               
         USING LSMD,R8             SELECTIONS,                                  
         TM    LSMINDS,LSMISEL                                                  
         BZ    VPFKX                                                            
         CLC   APRECKEY,LSMUKEY    AND WHEN KEY HAS NOT CHANGED -               
         BNE   VPFKX                                                            
         LA    R1,SLNPFKS          TEST FOR SPOT LENGTH PF KEY                  
*                                                                               
VPFK2    CLI   0(R1),0                                                          
         BE    VPFKX                                                            
         CLC   APPFKEY,0(R1)                                                    
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     VPFK2                                                            
         MVC   APHALF,1(R1)        SAVE SPOT LENGTH                             
         MVI   APPFKEY,0           GET RID OF TRACE OF PF KEY                   
         LA    R1,WRKL1H                                                        
         LA    R2,WRKL2H-WRKL1H                                                 
         LA    R3,WRKFUTH+1                                                     
         SR    R4,R4                                                            
*                                                                               
VPFK4    GOTO1 AFVAL               FIND THE LINE THE CURSOR'S AT                
         CLC   ACCURS,FVABSA                                                    
         BL    VPFK6                                                            
         LA    R4,1(R4)                                                         
         BXLE  R1,R2,VPFK4                                                      
         GOTO1 AFVAL,WRKFUTH                                                    
         CLC   ACCURS,FVABSA                                                    
         BL    VPFK6                                                            
         B     VPFKX                                                            
*                                                                               
VPFK6    BCTR  R4,0                                                             
         LTR   R1,R4                                                            
         BM    VPFKX                                                            
         MH    R1,=Y(LSMRTABL)     TEST CURSOR'S ON A SID LINE                  
         LA    R1,LSMRTAB(R1)                                                   
         CLI   LSMRNUM-LSMRTAB(R1),RECSID                                       
         BNE   VPFKX                                                            
         MH    R4,=Y(WRKL2H-WRKL1H)     YES-                                    
         LA    R4,WRKL1H(R4)                                                    
         USING WRKL1H,R4                                                        
         XC    SVPROG,SVPROG                                                    
         TM    WRKPRGH+FVIIND-FVIHDR,FVIVAL  TEST PROGRAM FIELD CHANGED         
         BO    VPFK8                                                            
         CLC   WRKPRG(4),=C'SLN='  YES-TEST FOR 'SLN='                          
         BE    VPFKX               YES-IGNORE PF KEY THEN                       
         MVC   SVPROG,SPACES       NO-SAVE THE PROGRAM NAME                     
         MVC   SVPROG(L'WRKPRG),WRKPRG                                          
*                                                                               
VPFK8    MVC   WRKPRG,SPACES       MARK LINE FOR SPOT LENGTH TRANSFER           
         MVC   WRKPRG(4),=C'SLN='                                               
         MVC   WRKPRG+4(2),APHALF                                               
         NI    WRKPRGH+FVIIND-FVIHDR,255-FVIVAL                                 
         OI    TWAACTIV,TWAACPFK   INDICATE PF KEY SPOT LENGTH TRANSFER         
*                                                                               
VPFKX    B     EXIT                                                             
         DROP  R4,R8                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST LIST SCREEN HOOK                                              *         
***********************************************************************         
         SPACE 1                                                                
FSTSCR   CLI   INOSID,C'Y'         TEST FOR SID OPTION                          
         BNE   *+14                                                             
         MVC   WRKFUT(L'FUTLINE),FUTLINE    YES-FORMAT PF KEY LINE              
         OI    WRKFUTH+FVOIND-FVIHDR,FVOXMT                                     
*                                                                               
         TM    TWAACTIV,TWAACSCR   TEST HERE BECAUSE OF RE-SCROLLING            
         BZ    FSCR1               TO NOT FIRST SCREEN                          
         L     R1,ALSM             YES-MAKE SURE INITIAL KEY IS SET             
         USING LSMD,R1                 CORRECTLY                                
         XC    LSMLKEY,LSMLKEY                                                  
         LA    R3,LSMLKEY                                                       
         MVC   BWDKEY,DTLKEY                                                    
         MVI   BWDKEL,BWDELCDQ                                                  
         OC    QSTA,QSTA                                                        
         BZ    FSCR1                                                            
         MVC   BWDKELST,BSTACD                                                  
         DROP  R1                                                               
*                                                                               
FSCR1    CLI   APACTN,ACTDEM       FOR DEMO ACTION --                           
         BNE   FSCRX                                                            
         L     RE,AIOAREA3         CLEAR WORK AREA FOR TWA BUILD                
         LA    RF,2000                                                          
         XCEF                                                                   
         SR    R0,R0               BUILD TWABLD ELEMENT LIST                    
         L     R1,AIOAREA3                                                      
         LA    R4,TWABRHD1         RHS HEADLINE 1                               
         USING TWAELEMD,R4                                                      
         BAS   RE,TWABELMV                                                      
         LA    R4,TWABLHED         LHS HEADLINE                                 
         MVC   TWAELLNQ(L'LHSHEAD,R1),LHSHEAD                                   
         BAS   RE,TWABELMV                                                      
         LA    R4,TWABRHD2         RHS HEADLINE 2                               
         BAS   RE,TWABELMV                                                      
*                                                                               
******** ZIC   R3,INODEM           NUMBER OF DEMOS                              
         LA    R3,4                MAX NUMBER OF DEMOS                          
         LR    RE,R3                                                            
         MH    RE,=Y(DEMRTG2H-DEMRTG1H)                                         
         LA    RE,L'DEMSDTH+L'DEMSDT(RE)                                        
         STH   RE,TWADMLNL         STORE LENGTH OF TWA DEMO LINE                
         SLL   R3,1                                                             
         LA    R3,1(R3)                                                         
         STC   R3,APBYTE           R3 = NUMBER OF FIELDS PER LINE               
         LA    RF,NDEMLINS                                                      
*                                                                               
FSCR2    ZIC   R3,APBYTE                                                        
         LA    R4,TWABLINE                                                      
         BAS   RE,TWABELMV         MOVE ALL FIELDS OF DEMO LINE                 
         LA    R4,TWAELLNQ(R4)                                                  
         BCT   R3,*-8                                                           
         BCT   RF,FSCR2            FOR ALL DEMO LINES                           
*                                                                               
         L     R1,AIOAREA3                                                      
         GOTO1 ATWABLD             CALL TWABLD                                  
         MVI   TWASCRN,0           FORCE CLEAR BEFORE AND AFTER                 
*                                                                               
         LA    R4,WRKL8H+DHED1DSP+L'FVIHDR    FORMAT THE DEMO HEADINGS          
         LA    R8,WRKL8H+DHED2DSP+L'FVIHDR                                      
         LA    RF,INODEM+1                                                      
*                                                                               
FSCR4    CLI   0(RF),FF                                                         
         BE    FSCR10                                                           
         LA    R1,COMDNAMS                                                      
         LA    RE,ESTDEMS                                                       
*                                                                               
FSCR6    OC    0(3,RE),0(RE)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   1(2,RE),1(RF)                                                    
         BE    FSCR8                                                            
         LA    RE,3(RE)                                                         
         LA    R1,6(R1)                                                         
         B     FSCR6                                                            
*                                                                               
FSCR8    MVC   0(3,R4),=C'---'                                                  
         MVC   3(6,R4),0(R1)                                                    
         CLI   8(R4),C' '                                                       
         BH    *+8                                                              
         MVI   8(R4),C'-'                                                       
         MVC   9(3,R4),=C'---'                                                  
         MVC   2(3,R8),=C'Rtg'                                                  
         MVC   9(3,R8),=C'Cpp'                                                  
         CLI   0(R1),C'R'                                                       
         BE    FSCR9                                                            
         CLI   CUDMED,C'C'         FOR CANADA, 'E' DEMOS ARE RATINGS            
         BNE   *+12                                                             
         CLI   0(R1),C'E'                                                       
         BE    FSCR9                                                            
         MVC   1(4,R8),=C'Imps'                                                 
         MVI   11(R8),C'm'                                                      
*                                                                               
FSCR9    LA    R4,13(R4)                                                        
         LA    R8,13(R8)                                                        
         LA    RF,3(RF)                                                         
         B     FSCR4                                                            
*                                                                               
FSCR10   OI    WRKL8H+DHED1DSP+6,FVOXMT                                         
         OI    WRKL8H+DHED2DSP+6,FVOXMT                                         
         B     FSCRX                                                            
*                                                                               
FSCRX    B     EXIT                                                             
         SPACE 2                                                                
TWABELMV MVC   0(TWAELLNQ,R1),0(R4)   MOVE TWABLD ELEMENT TO WORK AREA          
         IC    R0,TWAELLN                                                       
         AR    R1,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR LIST SCREEN HOOK                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVC   SCPFKEY,APPFKEY                                                  
         TM    TWAACTIV,TWAACSCR   TEST RE-SCROLL                               
         BZ    *+12                                                             
         MVI   SCPFKEY,PFK04       YES-FORCE START FROM 1ST SCREEN              
         B     LSCRX                                                            
         TM    TWAACTIV,TWAACLFM   TEST ANY LFM ACTION                          
         BZ    *+12                                                             
         NI    TWAACTIV,FF-TWAACLFM YES                                         
         B     LSCR2                                                            
         TM    TWAACTIV,TWAACLIN   TEST FOR ANY LINE CHANGE ACTION              
         BZ    LSCRX                                                            
         NI    TWAACTIV,FF-TWAACLIN                                             
         CLI   APPFKEY,0           YES - TEST FOR NO PFKEY                      
         BNE   LSCRX                                                            
*                                                                               
LSCR2    MVI   SCPFKEY,PFK05       FORCE STAY ON CURRENT SCREEN                 
*                                                                               
LSCRX    MVI   APMODE,APMPFKS      PF KEY SET                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
* INPUT  : APINDS EQ APILFLST FIRST TIME / FIRST SCREEN               *         
*                    APILNLST FISRT TIME / NOT FIRST SCREEN           *         
*                    OTHER    NEXT TIME(S)                            *         
*          APRECKEY                                                   *         
* OUTPUT : APRECKEY                                                   *         
*          APMODE  = APMEOFS FOR END-OF-LIST                          *         
*          APRECID+0(1) = RECORD IDENTIFICATION BYTE                  *         
*          APRECID+1(2) = DAYPART/LENGTH (FOR BWS RECORDS)            *         
*          APRECNUM = 0 (FOR REGULAR BWS RECORDS)                     *         
*          APRECNUM = RECPKG (FOR PACKAGE RECORDS)                    *         
*          APRECNUM = RECORB (FOR ORBIT RECORDS)                      *         
*          APRECNUM = RECSID (FOR NSID RECORDS)                       *         
*          APRECDA  = STATION (FOR BWS RECORDS)                                 
***********************************************************************         
         SPACE 1                                                                
GETSEL   CLI   APINDS,APILFLST     TEST FOR VERY FIRST TIME                     
         BNE   GETS6                                                            
         OI    LFLAG,LFSTLINE      FLAG FIRST LINE                              
         NI    TWAACTIV,TWAACSCR+TWAACPFK  RESET SCREEN ACTIVITY                
         SR    R8,R8               R8 = RECORD COUNT                            
         L     RE,LARECTAB                                                      
         ST    RE,LANXTREC         SAVE A(NEXT SLOT IN RECORD TABLE)            
******** LH    RF,=Y(CHKPTGLD)     RECORD TABLE IS AREA FROM SAVAREAX           
******** SH    RF,=Y(SAVAREAX-TWAD)  TO START OF FACPAK TABLES                  
******** LA    R1,0(RE,RF)                                                      
         LH    RF,=Y(14*1024)      ALLOCATE 14K TO RECORD TABLE                 
         LA    R1,0(RE,RF)                                                      
         ST    R1,LARCTABX         SAVE A(END OF RECORD TABLE)                  
         XCEF  ,                   CLEAR THE TABLE                              
         TM    TWAFLAG,TWANODET    TEST FOR ANY DETAIL RECORDS                  
         BNZ   GETS4                                                            
         MVC   IOKEY(13),APRECKEY  YES-                                         
         LA    R1,MINHI2           START READING DETAIL RECORDS                 
         LA    R2,BWDKELST-BWDKEY-1                                             
         OC    QSTA,QSTA                                                        
         BZ    *+8                                                              
         LA    R2,L'BWDKELST(R2)                                                
         B     GETS2+4                                                          
*                                                                               
GETS2    LA    R1,MINSEQ2          READ NEXT DETAIL RECORD                      
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)   TEST REACHED END                           
         B     GETS4                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   GETS4                                                            
         L     R3,AIOAREA2         ADDRESS THE RECORD                           
*                                                                               
         OC    QSTA,QSTA           TEST STATION FILTER                          
         BZ    *+14                                                             
         CLC   QSTA,BWDSTA         YES-MATCH THE STATION                        
         BNE   GETS2                                                            
         OC    QCABLE,QCABLE       TEST CABLE FILTER                            
         BZ    *+14                                                             
         CLC   QCABLE,BWDSTA       YES-MATCH CABLE SYSTEM                       
         BNE   GETS2                                                            
         CLI   BDPT,0              TEST DAYPART FILTER                          
         BE    *+14                                                             
         CLC   BWDDPT,BDPT         YES-MATCH THE DAYPART                        
         BNE   GETS2                                                            
         CLI   BSLN,0              TEST SPOT LENGTH FILTER                      
         BE    *+14                                                             
         CLC   BWDSLN,BSLN         YES-MATCH THE LENGTH                         
         BNE   GETS2                                                            
*                                                                               
         CLI   BWDKELPO,0          TEST PACKAGE/ORBIT                           
         BE    *+12                                                             
         CLI   BWDKELSQ,0          YES-TEST SLAVE                               
         BNE   GETS2               YES-IGNORE                                   
*                                                                               
         BAS   RE,BLDREC           BUILD RECORD TABLE ENTRY                     
         B     GETS2               NEXT RECORD                                  
*                                                                               
GETS4    CLI   INOSID,0            TEST FOR SID OPTION                          
         BE    *+12                                                             
         BAS   RE,READNSID         YES-READ NSID RECORDS                        
         BNE   GETSX                                                            
         LTR   R8,R8               TEST ANY RECORDS                             
         BZ    GETS99                                                           
         LA    R4,LRECTABL         YES - SORT TABLE                             
         LA    R1,LRECKEYL                                                      
         ST    R1,APPARM+12                                                     
*                                                                               
         GOTO1 VXSORT,APPARM,(0,LARECTAB),(R8),(R4),,0                          
*                                                                               
         GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(4,0),LARECTAB  SAVE                
         XC    TWARDSPL,TWARDSPL   FIRST RECORD                                 
         TM    TWAACTIV,TWAACSCR   TEST RE-SCROLL                               
         BZ    *+14                                                             
         NI    TWAACTIV,255-TWAACSCR    YES-                                    
         MVC   TWARDSPL,TWASDSPL        SCROLL TO CURRENT SCREEN                
         NI    TWAACTIV,255-TWAACPFK    MAKE SURE PFKEY SLN XFR OFF NOW         
         LH    RF,TWARDSPL                                                      
         STH   RF,TWASDSPL         SCREEN DISPLACEMENT                          
         B     GETS10                                                           
*                                                                               
GETS6    CLI   APINDS,APILNLST     TEST FIRST LINE / NOT FIRST SCREEN           
         BNE   GETS8                           YES - READ THE RECORD            
         GOTO1 VDMGR,APPARM,DMREAD,TEMPSTR,(4,0),LARECTAB     TABLE             
         LH    RF,TWARDSPL         CURRENT RECORD                               
         STH   RF,TWASDSPL         FIRST RECORD OF SCREEN                       
         MVI   TWAACTIV,0          NO SCREEN ACTIVITY YET                       
         OI    LFLAG,LFSTLINE      FLAG FIRST LINE                              
         B     GETS10                                                           
*                                                                               
GETS8    LH    RF,TWARDSPL         NOT FIRST LINE - GET NEXT RECORD             
         LA    RF,LRECTABL(RF)                                                  
         STH   RF,TWARDSPL                                                      
*                                                                               
GETS10   L     R4,LARECTAB         POINT TO RECORD TABLE ENTRY                  
         AR    R4,RF                                                            
         USING LRECTABD,R4                                                      
         OC    0(LRECKEYL,R4),0(R4)  TEST FOR END                               
         BZ    GETS99                                                           
         MVI   APRECNUM,0          SET VALUES FOR ROOT                          
         MVC   APRECID,RTRECID     RECORD ID                                    
         CLI   CUDMED,C'C'         CANADIAN INDICATOR                           
         BNE   *+8                                                              
         OI    APRECID,RICANAD                                                  
         TM    RTINDS,RTIPKG       TEST PACKAGE/ORBIT RECORD                    
         BZ    *+8                                                              
         MVI   APRECNUM,RECPKG     YES - SET APRECNUM                           
         TM    RTINDS,RTIORB                                                    
         BZ    *+8                                                              
         MVI   APRECNUM,RECORB                                                  
*                                                                               
         TM    RTRECID,RINSID      TEST NSID                                    
         BZ    GETS12                                                           
         MVI   APRECNUM,RECSID     YES-SET APRECNUM                             
         LA    RF,APRECKEY         SET KEY UP WITH RECORD DETAILS               
         USING NSIDKEYD,RF                                                      
         MVC   NSPER,RTPER                                                      
         MVC   NSDPT,RTDPT2                                                     
         MVC   NSSLN,RTSLN2                                                     
         MVC   NSPRG,RTPRG                                                      
         MVC   NSDAY,RTDAY                                                      
         MVC   NSTIME,RTTIME                                                    
         MVC   NSMKT,MKTNOCLT                                                   
         MVC   NSSTA,RTRECID+1                                                  
         MVC   NSAGYMD,BAGYMD                                                   
         MVC   NSYEAR,RTYEAR                                                    
         B     GETSX                                                            
         DROP  RF                                                               
*                                                                               
GETS12   LA    R3,APRECKEY         BWS RECORD                                   
         MVC   BWDKEY,DTLKEY                                                    
         MVI   BWDKEL,BWDELCDQ                                                  
         MVC   BWDKEL+1(6),RTELKEY                                              
         MVC   IOKEY(13),APRECKEY                                               
         GOTO1 AMIN,MINRD2         READ THE RECORD                              
         BNE   EPF4                                                             
         L     R3,AIOAREA2         SET MORE VALUES FOR ROOT                     
         MVC   APRECDA,BWDSTA      APRECDA=STATION                              
         CLI   BWDSTA,C'0'         TEST CABLE                                   
         BL    *+10                                                             
         MVC   APRECKEY+40(3),BWDSTA+5  YES-STORE CABLE NET IN KEY+40           
         MVC   APRECID+1(1),BWDDPT APRECID+1(2)=DAYPART/LENGTH                  
         MVC   APRECID+2(1),BWDSLN                                              
         B     GETSX                                                            
         DROP  R4                                                               
*                                                                               
GETS99   MVI   APMODE,APMEOFS      END OF LIST                                  
         B     GETSX                                                            
*                                                                               
GETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ NSID RECORDS                                                   *         
* R8 = RECORD COUNT                                                   *         
* FVMSGNO SET ON OUTPUT                                               *         
***********************************************************************         
         SPACE 1                                                                
READNSID NTR1                                                                   
         LA    RF,SRBLKLN          CLEAR RANSID BLOCK                           
         XCEF  SRBLK,(RF)                                                       
         LA    R3,CMPPERS                                                       
         B     *+14                                                             
*                                                                               
NSID2    OC    0(4,R3),0(R3)       READ FOR ALL REQUIRED PERIODS                
         BZ    NSIDX                                                            
         MVC   SRASIR,AIOAREA3                                                  
         MVC   SRACOM,ACOM                                                      
         MVC   SRACLPAC,VCLPACK                                                 
         MVC   SRAMSUNP,VMSUNPK                                                 
         MVC   SRADYUNP,VDAYUNPK                                                
         MVC   SRAUNTIM,VUNTIME                                                 
         MVC   SRSELSCH,CMPSCHEM                                                
         MVC   SRSELAM,BAGYMD                                                   
         MVC   SRSELAGY,CUAALF                                                  
         MVC   SRSELMED,QMED                                                    
         MVC   SRSELSLN,BSLN       SPOT LENGTH                                  
*                                                                               
*****    MVC   SRSELCTY,APROF7                                                  
         MVI   SRSELCTY,C'U'                                                    
         TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BZ    *+8                                                              
         MVI   SRSELCTY,C'C'                                                    
*                                                                               
         CLI   BSLN,0                                                           
         BNE   *+10                                                             
         MVC   SRSELSLN,INOSLN                                                  
         CLI   SRSELSLN,0                                                       
         BNE   *+8                                                              
         MVI   SRSELSLN,30                                                      
         MVC   SRSELYR,INOSID+1    YEAR (IF ANY)                                
         MVC   SRSELPER,0(R3)      PERIOD                                       
         MVC   SRSELMKT,MKTNOCLT   NON CLIENT SPECIFIC MARKET CODE              
         MVC   SRSELSTA,BSTA       STATION (MAYBE 0)                            
         MVC   SRSELPRG(L'INOPRG),INOPRG                                        
         MVC   SRSELDPT(1),BDPT    DAYPART (MAYBE 0)                            
         CLI   BDPT,0                                                           
         BE    NSID4                                                            
         CLI   CMPDPOPT,C'S'       TEST SUBDPTS SCHEDULED SEPERATELY            
         BE    NSID4               YES-NO SUBDPTS                               
         MVC   SRSELDPT+1(L'SRSELDPT-1),DPTSUBS                                 
         MVC   SRSELDP2,DPTSUBS+L'SRSELDPT-1                                    
*                                                                               
NSID4    GOTO1 VRANSID,APPARM,SRBLK     READ A DETAIL RECORD                    
         CLI   SRERROR,SRNOSCH                                                  
         BE    ESCH                                                             
         CLI   SRERROR,SRNOPER                                                  
         BE    EPER                                                             
         CLI   SRERROR,SRNOERR                                                  
         BNE   NSID36                                                           
         CLI   SRMODE,SRONEREC                                                  
         BNE   NSID36                                                           
         CLC   SRACTTIM+2(2),=C'CC'     IGNORE 'TILL CONCLUSION'                
         BE    NSID4                                                            
*                                                                               
         OC    QCABLE,QCABLE       TEST CBALE FILTER                            
         BZ    *+14                                                             
         CLC   QCABLE(4),SRERSTA   YES-MATCH CABLE SYSTEM                       
         BNE   NSID4                                                            
*                                                                               
         CLI   BDPT,0              TEST ALL DAYPARTS                            
         BNE   NSID5                                                            
         MVC   APBYTE,BSLN                                                      
         GOTO1 AGETDPT,SRACTDPT    YES-VALIDATE NSID DAYPART                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK) INVALID                                      
         B     NSID4                                                            
         MVC   BSLN,APBYTE                                                      
         MVI   BDPT,0                                                           
         MVI   DPTTYPE,0                                                        
         MVI   DPTMAS,0                                                         
         XC    DPTSUBS,DPTSUBS                                                  
*                                                                               
NSID5    TM    CMPIND,CMPICMPU     TEST COMPETITION USER                        
         BZ    NSID10                                                           
         L     R2,SRASIR           YES - LOOK FOR BWS TRANSFER ELEMENT          
         USING SIRRECD,R2                                                       
         SR    R0,R0                                                            
         LA    R1,EOKELEM                                                       
*                                                                               
NSID6    CLI   0(R1),0                                                          
         BE    NSID4               NOT FOUND - READ NEXT RECORD                 
         CLI   0(R1),EBWCODEQ                                                   
         BNE   NSID8                                                            
         USING EBWELEM,R1                                                       
         CLC   EBWCLT,BCLT         MATCH CLT/PRD/EST                            
         BNE   NSID8                                                            
         CLC   EBWPRD,BPRD                                                      
         BNE   NSID8                                                            
         CLC   EBWEST,BEST                                                      
         BE    NSID10              MATCH - RECORD IS OK                         
*                                                                               
NSID8    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     NSID6                                                            
         DROP  R1                                                               
*                                                                               
NSID10   L     R4,LARECTAB                                                      
         USING LRECTABD,R4                                                      
*                                                                               
NSID12   OC    0(LRECKEYL,R4),0(R4)   CHECK DAY/TIMES/STA NOT ALREADY           
         BZ    NSID14                 IN TABLE                                  
         CLC   SRACTDAY,RTDAY                                                   
         BNE   NSID13                                                           
         CLC   SRACTTIM,RTTIME                                                  
         BNE   NSID13                                                           
         CLC   SRACTSTA,RTRECID+1                                               
         BE    NSID4                                                            
*                                                                               
NSID13   LA    R4,LRECTABL(R4)                                                  
         C     R4,LARCTABX                                                      
         BL    NSID12                                                           
         CLI   BDPT,0              RECORD TABLE OVERFLOW                        
         BE    EFLTDPT                                                          
         B     ETMR                                                             
*                                                                               
NSID14   ST    R4,LANXTREC         SAVE A(RECORD TABLE ENTRY)                   
         XC    LSLNLST,LSLNLST                                                  
         CLI   BSLN,0              TEST NO SPOT LENGTH FILTER                   
         BNE   NSID20                                                           
         CLI   SRACTSLN,0          AND THERE ARE EFFECTIVE COSTS                
         BE    NSID20                                                           
         CLI   INOSLN,0            AND SLN OPTION NOT SET                       
         BNE   NSID20                                                           
         TM    INOXFI,INOXFINC     AND WANT TO TRANSFER COSTS                   
         BO    NSID20                                                           
         L     R2,SRASIR           YES-FIND THE EFFECTIVE COST ELEMENTS         
         USING SIRRECD,R2                                                       
         LA    R5,LSLNLST                                                       
         SR    R0,R0                                                            
         LA    R1,EOKELEM                                                       
*                                                                               
NSID16   CLI   0(R1),0                                                          
         BE    NSID20                                                           
         CLI   0(R1),EECCODEQ                                                   
         BNE   NSID18                                                           
         USING EECELEM,R1                                                       
         CLC   EECSLN,SRACTSLN     FOUND-TEST THIS IS SLN ALREADY READ          
         BE    NSID18              YES                                          
         MVC   0(1,R5),EECSLN      NO-BUILD LIST OF EXTRA SPOT LENGTHS          
         LA    R5,1(R5)                                                         
         DROP  R1                                                               
*                                                                               
NSID18   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     NSID16                                                           
         DROP  R2                                                               
*                                                                               
NSID20   SR    R5,R5                                                            
         LA    R6,L'INOSLN                                                      
         CLI   BSLN,0              TEST SPOT LENGTH FILTER                      
         BNE   *+16                                                             
         CLI   INOSLN,0            NO-TEST SLN OPTION SET                       
         BE    *+8                                                              
         LA    R5,INOSLN           YES-R5=A(CURRENT SPOT LENGTH)                
         TM    CMPIND,CMPICMPU     TEST COMPETITION USER                        
         BZ    NSID24                                                           
*                                                                               
NSID22   XC    APRECKEY,APRECKEY   PREPARE APRECKEY FOR BWS REC BUILD           
         LA    R2,APRECKEY                                                      
         USING NSIDKEYD,R2                                                      
         MVC   NSPER,0(R3)                                                      
         MVC   NSDPT,SRACTDPT                                                   
         MVC   NSPRG,SRACTPRG                                                   
         MVC   NSDAY,SRACTDAY                                                   
         MVC   NSTIME,SRACTTIM                                                  
         MVC   NSSTA,SRACTSTA                                                   
         MVC   NSMKT,SRACTMKT                                                   
         MVC   NSAGYMD,BAGYMD                                                   
         MVC   NSYEAR,SRSELYR                                                   
         MVC   NSDPT,SRACTDPT                                                   
         MVC   NSSLN,SRSELSLN                                                   
         LTR   R5,R5                                                            
         BZ    *+10                                                             
         MVC   NSSLN,0(R5)                                                      
         GOTO1 ADTLBLD             BUILD BWS DETAIL RECORD                      
         BNE   NSIDX                                                            
         XC    COMATWAL,COMATWAL                                                
         BAS   RE,XFRADD           ADD IT                                       
         BNE   NSIDX                                                            
         BAS   RE,BLDREC           ENTER BWS RECORD IN TABLE                    
         B     NSID28                                                           
*                                  NOT COMPETITION USER -                       
NSID24   GOTO1 GETDAY,SRACTDAY     ENTER DETAIL RECORD IN TABLE                 
         CLI   INORNK,INORNKT      TEST RANK IN DAY/TIME SEQ                    
         BE    *+16                                                             
         MVC   RTDPT,SRACTDPT      NO-RANK IN DPT/LEN SEQ                       
         MVC   RTSLN,SRSELSLN                                                   
         MVC   RTDAYCOD,LDAY                                                    
         MVC   RTDAY,SRACTDAY                                                   
         MVC   RTTIME,SRACTTIM                                                  
         MVC   RTSTA,SRERSTAN                                                   
         CLI   RTSTA,C'0'                                                       
         BNL   *+10                                                             
         MVC   RTSTA+5(3),SPACES                                                
         MVC   RTDPT2,SRACTDPT                                                  
         MVC   RTSLN2,SRSELSLN                                                  
         MVC   RTPRG,SRACTPRG      PROGRAM TYPE                                 
         MVC   RTPER,0(R3)         PERIOD                                       
         OI    RTRECID,RINSID      NSID INDICATOR                               
         MVC   RTRECID+1(3),SRACTSTA    STATION                                 
         MVC   RTYEAR,SRSELYR                                                   
         LTR   R5,R5                                                            
         BZ    NSID27                                                           
*                                                                               
NSID26   CLI   INORNK,INORNKT                                                   
         BE    *+10                                                             
         MVC   RTSLN,0(R5)                                                      
         MVC   RTSLN2,0(R5)                                                     
*                                                                               
NSID27   MVC   APWORK(LRECTABL),0(R4)                                           
         LA    R4,LRECTABL(R4)                                                  
         XC    0(LRECKEYL,R4),0(R4)                                             
         LA    R8,1(R8)            INCREMENT RECORD COUNTER                     
*                                                                               
NSID28   CLI   LSLNLST,0           TEST MORE SPOT LENGTHS                       
         BE    NSID30                                                           
         LTR   R5,R5               YES                                          
         BNZ   *+12                                                             
         LA    R5,LSLNLST                                                       
         B     NSID33                                                           
         LA    R5,1(R5)                                                         
         B     NSID32                                                           
*                                                                               
NSID30   LTR   R5,R5                                                            
         BZ    NSID34                                                           
         LA    R5,1(R5)                                                         
         BCT   R6,NSID32                                                        
         B     NSID34                                                           
*                                                                               
NSID32   CLI   0(R5),0                                                          
         BE    NSID34                                                           
*                                                                               
NSID33   TM    CMPIND,CMPICMPU                                                  
         BO    NSID22                                                           
         MVC   0(LRECTABL,R4),APWORK                                            
         B     NSID26                                                           
         DROP  R2                                                               
*                                                                               
NSID34   L     R5,ATWA             RESTORE TWA0 REGISTERS                       
         L     R6,LASAVE                                                        
         B     NSID4               READ NEXT RECORD                             
*                                                                               
NSID36   LA    R3,4(R3)            NEXT PERIOD                                  
         B     NSID2                                                            
*                                                                               
NSIDX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXITR8          RETURN R8                                        
         EJECT                                                                  
***********************************************************************         
* BUILD ENTRY IN RECORD TABLE FOR BWS DETAIL RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
BLDREC   NTR1  ,                                                                
         L     R4,LANXTREC                                                      
         USING LRECTABD,R4                                                      
         XC    0(LRECTABL,R4),0(R4)                                             
         L     R3,AIOAREA2                                                      
         CLI   INORNK,INORNKT      TEST RANK IN DAY/TIME SEQ                    
         BE    *+16                                                             
         MVC   RTDPT,BWDDPT        NO-RANK IN DPT/LEN SEQ                       
         MVC   RTSLN,BWDSLN                                                     
         LA    R1,BWDDAYS                                                       
         BAS   RE,GETDAY                                                        
         MVC   RTDAYCOD,LDAY       DAY CODE FOR SORTING                         
         MVC   RTDAY,BWDDAYS       DAYS                                         
         MVC   RTTIME,BWDTIMES     TIMES                                        
         MVC   RTSTA,BWDSTA        STATION                                      
         MVC   RTDPT2,BWDDPT       DPTLEN FOR DAY/TIME/STA/DPTLEN SEQ           
         MVC   RTSLN2,BWDSLN                                                    
         MVI   RTRECID,0                                                        
         MVC   RTELKEY,BWDKELST    ELEM KEY STA/PKG-ORB/DAYS/TIMES/SEQ          
         MVC   RTRECID+1(3),BSTA   SET THE STATION                              
         OC    BSTA,BSTA                                                        
         BNZ   BLDR1                                                            
         GOTO1 VMSPACK,APPARM,QMKT,BWDSTA,APDUB                                 
         MVC   RTRECID+1(3),APDUB+2                                             
*                                                                               
BLDR1    TM    BWDINDS,BWDIPKG     PACKAGE/ORBIT INDICATORS                     
         BZ    *+8                                                              
         OI    RTINDS,RTIPKG                                                    
         TM    BWDINDS,BWDIORB                                                  
         BZ    *+8                                                              
         OI    RTINDS,RTIORB                                                    
*                                                                               
BLDR2    MVC   APWORK(LRECTABL),0(R4)                                           
         LA    R4,LRECTABL(R4)                                                  
         ST    R4,LANXTREC                                                      
         XC    0(LRECTABL,R4),0(R4)                                             
         LA    R8,1(R8)            AUGMENT RECORD COUNT                         
*                                                                               
         OC    BWDEFDT2,BWDEFDT2   TEST EFFECTIVE DATES                         
         BZ    BLDRX               NO                                           
         LA    R4,APWORK                                                        
         TM    RTRECID,RIEFFDT3    YES-TEST PROCESSED EFFECTIVE COST 3          
         BO    BLDRX               YES-DONE WITH THIS RECORD                    
         TM    RTRECID,RIEFFDT2    TEST JUST PROCESSED EFFECTIVE COST 2         
         BO    *+12                                                             
         OI    RTRECID,RIEFFDT2    NO-PROCESS EFFECTIVE COST 2                  
         B     BLDR4                                                            
         OC    BWDEFDT3,BWDEFDT3   YES-TEST EFFECTIVE DATE 3                    
         BZ    BLDRX                                                            
         NI    RTRECID,FF-RIEFFDT2 YES-PROCESS EFFETCIVE COST 3                 
         OI    RTRECID,RIEFFDT3                                                 
*                                                                               
BLDR4    L     R4,LANXTREC         GENERATE RECORDS FOR EFFECTIVE COSTS         
         MVC   0(LRECTABL,R4),APWORK                                            
         B     BLDR2                                                            
*                                                                               
BLDRX    B     EXITR8              RETURN R8                                    
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT RECORD                                          *         
* INPUT : APPARM+0(4) = A(TWA DISPLAY LINE)                           *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   MVC   COMATWAL,APPARM     A(TWA LINE)                                  
         NI    LFLAG,FF-LOVRCOST                                                
         L     R8,APPARM                                                        
         USING WRKL1H,R8                                                        
         CLI   APRECNUM,RECSID     TEST NSID RECORD                             
         BE    DISS2                                                            
         CLI   APMODE,APMDISS2     NO-TEST POST RECORD CHANGE DISPLAY           
         BNE   DISS1                                                            
         LA    R3,APRECKEY                                                      
         CLI   BWDKTYP,BWDKTYPQ    YES-ONLY FOR BWS DETAIL RECORDS              
         BNE   DISSX                                                            
         CLI   BWDKSUB,BWDKSUBQ                                                 
         BNE   DISSX                                                            
         MVC   IOKEY(13),APRECKEY  GET DETAIL RECORD                            
         GOTO1 AMIN,MINRD2                                                      
         BE    DISS1                                                            
         MVC   FVMSGNO,=AL2(FVFOK) NOT FOUND-MUST'VE JUST BEEN DELETED          
         B     DISSX                                                            
*                                                                               
DISS1    L     R3,AIOAREA2                                                      
         GOTO1 ADISPSEL            DISPLAY                                      
         BNE   DISSX                                                            
         MVI   LRECIND,0                                                        
         TM    BWDINDS,BWDIXFR     TEST FOR TRANSFERRED FROM SID                
         BZ    *+8                                                              
         MVI   LRECIND,C'+'        YES-SHOW IT                                  
         B     DISS48                                                           
*                                                                               
DISS2    LA    R4,APRECKEY                                                      
         USING NSIDKEYD,R4                                                      
         LA    RF,SRBLKLN          CLEAR RANSID BLOCK                           
         XCEF  SRBLK,(RF)                                                       
         MVC   SRASIR,AIOAREA3                                                  
         MVC   SRACOM,ACOM                                                      
         MVC   SRACLPAC,VCLPACK                                                 
         MVC   SRAMSUNP,VMSUNPK                                                 
         MVC   SRADYUNP,VDAYUNPK                                                
         MVC   SRAUNTIM,VUNTIME                                                 
         MVC   SRSELSCH,CMPSCHEM                                                
         MVC   SRSELAM,BAGYMD                                                   
         MVC   SRSELAGY,CUAALF                                                  
         MVC   SRSELMED,QMED                                                    
         MVC   SRSELSLN,NSSLN                                                   
*                                                                               
*****    MVC   SRSELCTY,APROF7                                                  
         MVI   SRSELCTY,C'U'                                                    
         TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BZ    *+8                                                              
         MVI   SRSELCTY,C'C'                                                    
*                                                                               
         CLI   SRSELSLN,0                                                       
         BNE   *+8                                                              
         MVI   SRSELSLN,30                                                      
         MVC   SRSELYR,NSYEAR                                                   
         MVC   SRSELPER,NSPER                                                   
         MVC   SRSELMKT,MKTNOCLT   NON CLIENT SPECIFIC MKT CODE                 
         MVC   SRSELSTA,NSSTA                                                   
         MVC   SRSELDPT(1),NSDPT                                                
         MVC   SRSELPRG(1),NSPRG                                                
         MVC   SRSELDAY,NSDAY                                                   
         MVC   SRSELTIM,NSTIME                                                  
         OC    SRSELTIM+2(2),SRSELTIM+2                                         
         BNZ   *+10                                                             
         MVC   SRSELTIM+2(2),SRSELTIM                                           
*                                                                               
DISS4    GOTO1 VRANSID,APPARM,SRBLK     READ THE DETAIL RECORD                  
         CLI   SRERROR,SRNOPER          NO PERIOD FOUND                         
         BE    DISS99                   THEN WE HAVE AN ERR, DON'T DIE          
         CLI   SRERROR,SRNOSCH          NO VALID SCHEME IN CAM RECORD           
         BE    DISS98                   WE HAVE AN ERROR, DON'T DIE             
         CLI   SRERROR,SRNOERR                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SRMODE,SRONEREC                                                  
         BNE   DISS99                                                           
         CLC   SRACTSTA,NSSTA           CHECK CORRECT STA/DAY/TIME              
         BNE   DISS4                                                            
         CLC   SRACTDAY,NSDAY                                                   
         BNE   DISS4                                                            
         CLC   SRACTTIM,NSTIME                                                  
         BNE   DISS4                                                            
         DROP  R4                                                               
*                                                                               
         MVI   LRECIND,C'S'        SHOW UP SID RECORD                           
         XC    LUPGRD,LUPGRD       INITIALIZE UPGRADE VALUES                    
         XC    LUPFIL,LUPFIL                                                    
         XC    LUPFRBK,LUPFRBK                                                  
         MVI   LUPFRBKT,0                                                       
         XC    LUPFRBKL,LUPFRBKL                                                
         XC    LUPDAY,LUPDAY                                                    
         XC    LUPTIM,LUPTIM                                                    
         XC    LUPPUT,LUPPUT                                                    
         XC    LUPSHR,LUPSHR                                                    
         XC    LAOVREL,LAOVREL                                                  
         MVC   LUPSTA,SRERSTA                                                   
         OC    CMPUP,CMPUP         TEST FOR DEFAULT CAMPAIGN UPGRADE            
         BZ    DISS8                                                            
         MVC   LUPFIL,CMPUF        YES - SAVE UPGRADE VALUES                    
         MVC   LUPGRD,CMPUP                                                     
         MVC   LUPFRBK,CMPFB                                                    
         TM    CMPFB+1,BTY2CHAR                                                 
         BNO   DISS6G                                                           
         CLI   CMPFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LUPFRBKT(1),CMPFBTP                                              
DISS6G   MVC   LUPFRBKL(6),CMPFBLST                                             
         MVC   LUPFRBKL,CMPFBLST                                                
         MVC   LUPPUT,CMPUPUT                                                   
         MVC   LUPSHR,CMPUSHR                                                   
*                                                                               
DISS8    LA    R4,WRKLST           START FORMATTING DISPLAY LINE                
         USING LIST1D,R4                                                        
         MVC   LISTSTA,SRERSTA     STATION                                      
         MVC   WRKPRG,SRACTPRO     PROGRAM                                      
         OI    WRKPRGH+6,FVOXMT                                                 
         MVC   QTIMES,SRERTIM      TIMES                                        
         MVC   LACTDAY,SRACTDAY    SAVE DAYS                                    
         MVC   LACTTIME,SRACTTIM   SAVE TIMES                                   
         MVC   LACTCOST,SRACTCS1   SAVE COST                                    
*                                                                               
         L     R2,SRASIR           LOOK FOR BWS COST OVERRIDE ELEMENT           
         USING SIRRECD,R2                                                       
         SR    R0,R0                                                            
         LA    R3,EOKELEM                                                       
*                                                                               
DISS10   CLI   0(R3),0                                                          
         BE    DISS16                                                           
         CLI   0(R3),EBCCODEQ                                                   
         BNE   DISS14                                                           
         USING EBCELEM,R3                                                       
         CLC   EBCCLT,BCLT         FOUND - MATCH CLT/SLN                        
         BNE   DISS14                                                           
         CLC   EBCSLN,SRSELSLN                                                  
         BNE   DISS14                                                           
         OC    EBCEFF,EBCEFF                                                    
         BZ    DISS12                                                           
         TM    CMPOPTS,CAMOWKS     TEST NON-CONTIGUOUS FLIGHT WEEKS             
         BZ    DISS11                                                           
***      CLC   EBCEFF,CMPDATSP     YES                                          
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         CLC   EBCEFF,0(R5)        YES                                          
         BH    DISS14                                                           
         B     DISS12                                                           
*                                                                               
DISS11   GOTO1 VDATCON,APPARM,(2,EBCEFF),(3,APFULL)                             
         CLC   APFULL(3),CMPST                                                  
         BH    DISS14                                                           
*                                                                               
DISS12   MVC   LACTCOST,EBCCOST    SAVE OVERRIDE COST                           
         OI    LFLAG,LOVRCOST                                                   
         B     DISS16                                                           
*                                                                               
DISS14   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DISS10                                                           
         DROP  R2,R3                                                            
*                                                                               
DISS16   CLC   SRACTSLN,SRSELSLN   TEST SPOT LENGTH = SELECTED SLN              
         BE    DISS18                                                           
         CLI   SRACTSLN,0          NO-TEST SPOT LENGTH = 0                      
         BE    DISS18                                                           
         TM    LFLAG,LOVRCOST      NO-TEST COST OVERRIDE                        
         BO    DISS18                                                           
         CLI   CLTBWPRO+13,C'N'    NO-TEST OPTION TO NOT ADJUST < 30SEC         
         BNE   *+12                                                             
         CLI   SRSELSLN,30         YES-TEST SPOT LEN LESS THAN 30               
         BL    DISS18                                                           
         GOTO1 ACOSTEQU,APPARM,(SRACTSLN,LACTCOST),(SRSELSLN,LACTCOST)          
*                                                                               
DISS18   GOTO1 ANETCOST,LACTCOST   NET DOWN THE COST IF NECESSARY               
*                                                                               
         TM    CMPOPTS,CAMONSU     TEST NO SID UPGRADE                          
         BO    DISS24                                                           
         MVC   LAOVREL,SROVELEM    SAVE A(FIRST DEMO OVERRIDE ELEM)             
         CLI   SRUPWHER,C'D'       CHECK UPGRADE FROM DETAIL                    
         BE    *+14                                                             
         OC    CMPUP,CMPUP                                                      
         BNZ   DISS24                                                           
         MVC   LUPFIL,SRUPFILE     SAVE UPGRADE VALUES                          
         MVC   LUPGRD,SRUPEXP                                                   
         MVC   LUPFRBK,SRUPBOOK                                                 
****  PROBABLY NEEDS NEW CODE HERE, IGNORE FOR NOW                              
         MVC   LUPFRBKL,SRUPEXBK                                                
         MVC   LUPDAY,SRUPDAY                                                   
         MVC   LUPTIM,SRUPTIM                                                   
         OC    SRUPSTA,SRUPSTA                                                  
         BZ    DISS24                                                           
         MVC   LUPSTA,SRUPSTA      STATION OVERRIDE                             
*                                                                               
DISS24   OI    WRKLSTH+6,FVOXMT                                                 
         MVC   APHALF,BDPT         (SAVE DPT AND SLN)                           
         GOTO1 AGETDAY,LACTDAY     DAYS                                         
         MVC   WRKDAY,QDAYS                                                     
         OI    WRKDAYH+6,FVOXMT                                                 
         GOTO1 AGETTIM,LACTTIME    TIMES                                        
         MVC   WRKTIM,QTIMES                                                    
         OI    WRKTIMH+6,FVOXMT                                                 
         MVC   BDPT(2),APHALF                                                   
         XC    EBLOCK,EBLOCK       COST                                         
         MVI   EBTIN,C'B'                                                       
         TM    INOXFI,INOXFINC     TEST FOR NO COST                             
         BO    DISS30                                                           
         MVI   EBDECS,2                                                         
         MVI   EBFLOAT,C'$'                                                     
         LA    RF,WRKCST                                                        
         ST    RF,EBAOUT                                                        
         MVI   EBLOUT,L'WRKCST                                                  
         LA    RF,LACTCOST                                                      
         ST    RF,EBAIN                                                         
         MVI   EBLIN,4                                                          
         SR    RE,RE                                                            
         ICM   RF,15,0(RF)                                                      
         D     RE,=F'100'                                                       
         LTR   RE,RE                                                            
         BZ    DISS26                                                           
*                                                                               
         LA    RE,L'WRKCST-4                                                    
         LTR   RE,RE                                                            
         BNP   DISS26                                                           
         LA    R1,1                                                             
         MH    R1,=H'10'                                                        
         BCT   RE,*-4                                                           
         BCTR  R1,0                                                             
         CR    RF,R1                                                            
         BNH   DISS28                                                           
*                                                                               
DISS26   MVI   EBSCIN,X'82'        SCALE PENNIES TO DOLLARS                     
         MVI   EBDECS,0                                                         
*                                                                               
DISS28   GOTO1 VEDITOR,APPARM,EBLOCK   FORMAT THE COST                          
         OI    WRKCSTH+6,FVOXMT                                                 
         TM    LFLAG,LOVRCOST          IF IT'S A NSID OVERRIDE COST,            
         BZ    DISS30                  GIVE IT A *                              
         L     R1,EBAOUT                                                        
         CLI   0(R1),C' '                                                       
         BH    DISS29                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *-8                                                              
         BCTR  R1,0                                                             
DISS29   MVI   0(R1),C'*'                                                       
*                                                                               
DISS30   LA    R0,LNDEMOS          BUILD DEMO LIST FOR SPDEMUP                  
         LA    R1,LDEMS                                                         
         LA    RE,ESTDEMS                                                       
*                                                                               
DISS31   OC    0(3,RE),0(RE)                                                    
         BZ    DISS32                                                           
         MVC   0(3,R1),0(RE)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,DISS31                                                        
*                                                                               
DISS32   MVI   0(R1),X'FF'                                                      
         BAS   RE,DEMUP            DO UPGRADE TO GET RATING                     
         LA    R4,LDEMVALS         FORMAT THE RATING                            
         LA    RF,LDEMS                                                         
         XC    APDUB,APDUB                                                      
         MVI   EBFLOAT,0                                                        
*                                                                               
DISS33   CLI   0(RF),FF            GET THE RATING AND IMPRESSION                
         BE    DISS36                                                           
         CLC   1(2,RF),SVDEMO1+1                                                
         BNE   DISS34                                                           
         ST    R4,APDUB            APDUB(4) = A(TARGET RTG/IMPRESSION)          
         CLI   0(RF),EDOCODEQ      TEST FOR OVERRIDE                            
         BNE   DISS34                                                           
         MVI   EBFLOAT,C'*'                                                     
*                                                                               
DISS34   CLC   1(2,RF),SVDEMO2+1                                                
         BNE   *+8                                                              
         ST    R4,APDUB+4          APDUB+4(4)=A(TARGET IMPRESSION/RTG)          
         LA    RF,3(RF)                                                         
         LA    R4,4(R4)                                                         
         B     DISS33                                                           
*                                                                               
DISS36   MVC   EBAIN,APDUB                                                      
         MVI   EBLIN,4                                                          
         LA    RE,WRKRAT                                                        
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'WRKRAT                                                  
         MVI   EBDECS,1                                                         
***  2 DECIMAL                                                                  
         L     RE,APDUB            APDUB CONTAINS AN ADDRESS                    
         TM    0(RE),X'40'         WE NEED 2 DECIMAL?                           
**       TM    APDUB,X'40'         WE NEED 2 DECIMAL?                           
         BNO   DISS36E                                                          
         MVI   EBDECS,2                                                         
         NI    0(RE),X'FF'-X'40'   TAKE OFF 2D BIT                              
         ST    RE,APDUB                                                         
**       NI    APDUB,X'FF'-X'40'                                                
***  2 DECIMAL                                                                  
DISS36E  MVI   EBSCIN,0                                                         
         CLI   SVDEMO1+1,C'I'      TEST FOR IMPRESSION                          
         BNE   *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    WRKRATH+6,FVOXMT                                                 
*                                                                               
         TM    INOXFI,INOXFINC     TEST FOR NO COST                             
         BO    DISS38              YES - SKIP CPP/CPM                           
         ICM   RE,15,LACTCOST      FORMAT CPP                                   
         BZ    DISS38                                                           
         ST    RE,COMCPPC                                                       
         LA    RE,LCOSTPP                                                       
         ST    RE,EBAIN                                                         
         MVI   EBFLOAT,C'$'                                                     
         MVI   EBDECS,2                                                         
         L     R8,COMATWAL                                                      
         L     R1,APDUB                                                         
         ICM   RE,15,0(R1)                                                      
         BZ    DISS38                                                           
         ST    RE,COMCPPD                                                       
         BAS   RE,CALCCPP                                                       
         LA    RE,WRKCPP                                                        
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'WRKCPP                                                  
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    WRKCPPH+6,FVOXMT                                                 
*                                                                               
DISS38   MVC   APBYTE,BDPT                                                      
         CLI   BDPT,0              TEST DAYPART FILTER                          
         BNE   DISS40                                                           
         CLI   CMPDPOPT,C'M'       NO-TEST SUBDAYPARTS UNDER MASTER             
         BNE   DISS40                                                           
         MVC   APHALF,BDPT         YES-GET DAYPART                              
         GOTO1 AGETDPT,SRACTDPT                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDPT(2),APHALF                                                   
         CLI   DPTTYPE,C'S'        TEST IT'S A SUBDAYPART                       
         BNE   DISS40                                                           
         MVC   APBYTE,DPTMAS       YES-SAVE THE MASTER DAYPART                  
*                                                                               
DISS40   TM    SVINDS,SVIDPLN      TEST DPTLEN IN HEADING, NOT CPM              
         BO    DISS42              YES                                          
         TM    INOXFI,INOXFINC     NO-TEST FOR NO COST                          
         BO    DISS44              YES-SKIP CPM                                 
         OC    LACTCOST,LACTCOST   TEST COST=0                                  
         BZ    DISS44              YES                                          
         OC    SVDEMO2,SVDEMO2     NO-FORMAT CPM                                
         BZ    DISS44                                                           
         L     R1,APDUB+4                                                       
         ICM   RE,15,0(R1)                                                      
         BZ    DISS44                                                           
         ST    RE,COMCPPD                                                       
         BAS   RE,CALCCPP                                                       
         LA    RE,WRKDLM                                                        
         LA    RE,LISTCPM-LIST2D(RE)                                            
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'LISTCPM                                                 
         CLC   LCOSTPP,=F'10000'                                                
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    WRKDLMH+6,FVOXMT                                                 
         B     DISS44                                                           
*                                                                               
DISS42   LA    RE,WRKDLM           FORMAT DAYPART/LENGTH                        
         LA    RE,LISTCPM-LIST2D(RE)                                            
         MVC   0(1,RE),SRACTDPT                                                 
         CLI   APBYTE,0            TEST MASTER DAYPART                          
         BE    *+10                                                             
         MVC   0(1,RE),APBYTE      YES                                          
         SR    RF,RF                                                            
         ICM   RF,1,SRSELSLN                                                    
         BNZ   *+8                                                              
         LA    RF,30                                                            
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  1(3,RE),APDUB                                                    
         CLI   1(RE),C'0'                                                       
         BNE   DISS44                                                           
         MVC   1(2,RE),2(RE)                                                    
         MVI   3(RE),C' '                                                       
*                                                                               
DISS44   CLI   BDPT,0              TEST DAYPART FILTER                          
         BNE   *+16                YES                                          
         CLI   APBYTE,0            NO-TEST MASTER/SUB DAYPARTS                  
         BNE   DISS46              YES-DISPLAY SUBDPT                           
         B     DISS47                                                           
         CLC   SRACTDPT,BDPT       DPT FILTER-TEST DAYPARTS MATCH               
         BE    DISS47              YES                                          
*                                                                               
DISS46   LA    RE,WRKDLM           DISPLAY SUB-DAYPART                          
         USING LIST2D,RE                                                        
         MVC   LISTMISC(4),=C'DPT='                                             
         MVC   LISTMISC+4(1),SRACTDPT                                           
         OI    WRKDLMH+6,FVOXMT                                                 
         DROP  RE                                                               
*                                                                               
DISS47   CLI   SRERSTAN,C'0'       TEST CABLE STATION                           
         BL    DISS48                                                           
         LA    RE,WRKDLM           YES-DISPLAY NETWORK                          
         USING LIST2D,RE                                                        
         MVC   LISTMISC,SPACES                                                  
         MVC   LISTMISC(3),SRERSTAN+5                                           
         OI    WRKDLMH+6,FVOXMT                                                 
         DROP  RE                                                               
*                                                                               
DISS48   CLI   LRECIND,0           TEST FOR RECORD INDICATOR                    
         BE    DISS50                                                           
         L     R8,COMATWAL                                                      
         LA    R4,WRKLST                                                        
         USING LIST1D,R4                                                        
         MVC   LISTLINE(1),LRECIND                                              
         DROP  R8                                                               
*                                                                               
DISS50   CLI   APACTN,ACTDEM       FOR DEMO ACTION --                           
         BNE   DISSX                                                            
         CLI   APMODE,APMDISS2     TEST POST RECORD CHANGE DISPLAY MODE         
         BNE   DISS51                                                           
         LA    R1,SAVOPTS          YES-RESTORE DEMO OPTION                      
         LA    R1,INODEM-INOPTS(R1)                                             
         MVC   INODEM,0(R1)                                                     
         LA    R4,WRKL8H+DLINEDSP  FIND DEMO LINE ADDRESS                       
         L     R1,COMATWAL                                                      
         LA    RE,WRKL1H                                                        
         SR    R1,RE                                                            
         BNP   DISS56                                                           
         SR    R0,R0                                                            
         D     R0,=A(WRKL2H-WRKL1H)                                             
         AH    R4,TWADMLNL                                                      
         BCT   R1,*-4                                                           
         B     DISS56                                                           
*                                                                               
DISS51   TM    LFLAG,LFSTLINE      TEST FOR FIRST LINE                          
         BZ    DISS54                                                           
         NI    LFLAG,FF-LFSTLINE                                                
         LA    R4,WRKL8H+DLINEDSP  YES - CLEAR ALL THE DEMO LINES               
         ST    R4,LADEMLIN                                                      
         LR    R1,R4                                                            
         SR    RE,RE                                                            
*                                                                               
DISS52   ICM   RE,1,0(R1)          TEST END OF TWA                              
         BZ    DISS56                                                           
         SH    RE,=Y(FVIFLD-FVIHDR)                                             
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R1),L'FVIHDR(R1)  CLEAR THE FIELD                     
         OI    6(R1),FVOXMT                 TRANSMIT                            
         NI    FVATRB-FVIHDR(R1),FF-FVAHIGH LOW INTENSITY                       
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   *+8                          FOR UNPROTECTED,                    
         OI    FVIIND-FVIHDR(R1),FVIVAL     TURN ON PREV VALIDATED              
         IC    RE,0(R1)            NEXT TWA FIELD                               
         AR    R1,RE                                                            
         B     DISS52                                                           
*                                                                               
DISS54   L     R4,LADEMLIN         NOT FIRST LINE -                             
         AH    R4,TWADMLNL         POINT TO CURRENT LINE                        
         ST    R4,LADEMLIN                                                      
*                                                                               
DISS56   L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         MVC   LSTA,SRERSTAN       SET THE STATION                              
         CLI   LSTA,C'0'                                                        
         BNL   *+10                                                             
         MVC   LSTA+5(3),SPACES                                                 
         CLI   APRECNUM,RECSID                                                  
         BE    *+10                                                             
         MVC   LSTA,BWDSTA                                                      
         BAS   RE,DISDEM           DISPLAY DEMO LINE                            
         B     DISSX                                                            
*                                                                               
DISS98   MVC   FVMSGNO,=AL2(FVNOSCHM)    NO VALID SCHEME IN CAM RECORD          
         MVI   FVOMTYP,C'E'                                                     
         B     DISSX                                                            
*                                                                               
DISS99   MVC   FVMSGNO,=AL2(FVGENERR)    PROCESSING ERROR - CALL DDS            
         MVI   FVOMTYP,C'E'                                                     
*                                                                               
DISSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY DEMO LINE                                                   *         
* INPUT  : R4 = A(TWA DEMO LINE)                                      *         
*          IOAREA2 CONTAINS THE RECORD                                *         
*          LSTA = STATION                                             *         
***********************************************************************         
         SPACE 1                                                                
DISDEM   NTR1                                                                   
         USING DEMLINED,R4                                                      
         MVC   DEMSTA(5),LSTA      FORMAT THE DEMO LINE                         
         CLI   DEMSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   DEMSTA+4,C' '                                                    
         CLI   LSTA,C'0'           TEST CABLE STATION                           
         BL    *+10                                                             
         MVC   DEMSTA+4(3),LSTA+5  YES-PLACE NETWORK NEXT TO HEADEND            
*                                                                               
         CLI   APRECNUM,RECSID     TEST NSID RECORD                             
         BE    DISD4                                                            
         L     R3,AIOAREA2         NO-                                          
         USING BWDRECD,R3                                                       
         SR    R0,R0               FIND THE DEMO ELEMENT                        
         LA    R1,BWDEL                                                         
*                                                                               
DISD2    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         ST    R1,APFULL           APFULL=A(DEMO ELEMENT)                       
         CLI   0(R1),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     DISD2                                                            
         MVC   COMCPPC,COMDETCS    SET DETAIL RECORD COST                       
         B     DISD4+6                                                          
*                                                                               
DISD4    MVC   COMCPPC,LACTCOST    SET NSID RECORD COST                         
         CLI   APRECNUM,RECSID     TEST BWS RECORD                              
         BE    DISD8                                                            
         CLI   BWDKELPO,0          YES-TEST PACKAGE/ORBIT                       
         BE    DISD8                                                            
         TM    BWDINDS,BWDIORB     YES                                          
         BO    DISD6                                                            
         MVC   DEMDAY,=C'--P A C'       PACKAGE                                 
         MVC   DEMTIME,=C'K A G E--  '                                          
         B     DISD10                                                           
*                                                                               
DISD6    MVC   DEMDAY,=C'--- O R'       ORBIT                                   
         MVC   DEMTIME,=C'B I T ---  '                                          
         B     DISD10                                                           
*                                                                               
DISD8    MVC   DEMDAY,QDAYS        DAYS                                         
         MVC   DEMTIME,QTIMES      TIMES                                        
*                                                                               
DISD10   OI    DEMSDTH+6,FVOXMT                                                 
*                                                                               
         XC    EBLOCK,EBLOCK       FORMAT THE RATINGS                           
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         LA    RE,APWORK                                                        
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'DEMRTG1                                                 
         XC    APELEM,APELEM                                                    
         LA    R2,APELEM                                                        
         LA    R3,DEMRTG1H                                                      
         ZIC   R8,INODEM                                                        
         LA    R6,INODEM+1                                                      
*                                                                               
DISD12   MVI   EBFLOAT,0                                                        
         CLI   APRECNUM,RECSID     TEST FOR NSID RECORD                         
         BNE   DISD16                                                           
         LA    RE,LDEMS            YES-GET DEMO VALUE FROM DEMO VALUE           
         LA    RF,LDEMVALS             LIST                                     
*                                                                               
DISD14   CLI   0(RE),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   1(2,RE),1(R6)                                                    
         BE    *+16                                                             
         LA    RE,3(RE)                                                         
         LA    RF,4(RF)                                                         
         B     DISD14                                                           
         CLI   0(RE),EDOCODEQ      TEST FOR OVERRIDE                            
         BNE   DISD18                                                           
         MVI   EBFLOAT,C'*'                                                     
         B     DISD18                                                           
*                                                                               
DISD16   L     R1,APFULL           NO-GET DEMO VALUE FROM DEMO ELEM             
         USING DMOEL,R1                                                         
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,DMODEMO                                                       
         LA    RE,L'DMODEMO                                                     
         CLC   1(2,R1),1(R6)                                                    
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DISD20              DEMO NOT FOUND                               
         MVC   APDUB(4),4(R1)                                                   
         TM    APDUB,DMODEMOV      TEST FOR OVERRIDE                            
         BZ    *+12                                                             
         MVI   EBFLOAT,C'*'                                                     
         NI    APDUB,FF-DMODEMOV                                                
         LA    RF,APDUB                                                         
*                                                                               
DISD18   ST    RF,EBAIN                                                         
         OC    0(4,R2),0(RF)       SAVE DEMO VALUES IN APELEM                   
         BZ    DISD20                                                           
         MVI   EBDECS,1                                                         
***  2 DECIMAL                                                                  
         TM    APDUB,X'40'                                                      
         BNO   DISD18G                                                          
         MVI   EBDECS,2                                                         
         NI    APDUB,FF-X'40'      TAKE OFF 2 DECIMAL BIT                       
         MVI   EBSCIN,0                                                         
         CLI   EBFLOAT,C'*'                                                     
         BNE   *+14                                                             
         CLC   0(4,RF),=F'10000'                                                
         BNL   *+14                                                             
         CLC   0(4,RF),=F'100000'                                               
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
         B     DISD19                                                           
***  2 DECIMAL                                                                  
DISD18G  MVI   EBSCIN,0                                                         
         CLI   EBFLOAT,C'*'                                                     
         BNE   *+14                                                             
         CLC   0(4,RF),=F'1000'                                                 
         BNL   *+14                                                             
         CLC   0(4,RF),=F'10000'                                                
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
*                                                                               
DISD19   GOTO1 VEDITOR,APPARM,EBLOCK                                            
         MVC   L'DEMRTG1H(L'DEMRTG1,R3),APWORK                                  
         OI    6(R3),FVOXMT                                                     
*                                                                               
DISD20   LA    R3,DEMRCPPL(R3)                                                  
         LA    R2,4(R2)                                                         
         LA    R6,3(R6)                                                         
         BCT   R8,DISD12                                                        
*                                                                               
         TM    INOXFI,INOXFINC     TEST FOR NO COST                             
         BO    DISD26                                                           
         OC    COMCPPC,COMCPPC     TEST FOR COST                                
         BZ    DISD26                                                           
         LA    RE,LCOSTPP          YES - FORMAT THE CPP'S                       
         ST    RE,EBAIN                                                         
         MVI   EBLOUT,L'DEMCPP1                                                 
         MVI   EBFLOAT,0                                                        
         LA    R3,DEMCPP1H                                                      
         ZIC   R8,INODEM                                                        
         LA    R6,APELEM                                                        
*                                                                               
DISD22   ICM   RE,15,0(R6)                                                      
         BZ    DISD24                                                           
         ST    RE,COMCPPD                                                       
         BAS   RE,CALCCPP                                                       
         MVI   EBDECS,2                                                         
         MVI   EBSCIN,0                                                         
         CLC   LCOSTPP,=F'100000'                                               
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         MVC   L'DEMCPP1H(L'DEMCPP1,R3),APWORK                                  
         OI    6(R3),FVOXMT                                                     
*                                                                               
DISD24   LA    R3,DEMRCPPL(R3)                                                  
         LA    R6,4(R6)                                                         
         BCT   R8,DISD22                                                        
*                                                                               
DISD26   B     DISDX                                                            
*                                                                               
DISDX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXECUTE A DEMO UPGRADE                                   *         
* INPUT  : LDEMS = DEMO LIST                                          *         
* OUTPUT : LDEMVALS = LIST OF DEMO VALUES                             *         
***********************************************************************         
         SPACE 1                                                                
DEMUP    NTR1  ,                                                                
         XC    LDEMVALS(4*LNDEMOS),LDEMVALS                                     
         OC    LUPGRD,LUPGRD       TEST FOR UPGRADE EXPRESSION                  
         BZ    DMUPX               NO - EXIT                                    
         LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         MVC   SPUPAREC,AIOAREA4                                                
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
         MVC   SPUPSTA,LUPSTA                                                   
         MVC   SPUPDAY,LACTDAY                                                  
         MVC   SPUPTIM,LACTTIME                                                 
         MVC   SPUPFIL,LUPFIL                                                   
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,LUPFRBK                                                  
         MVC   SPUPFBKL,LUPFRBKL                                                
         OC    SPUPFBK,SPUPFBK                                                  
         BNZ   *+16                                                             
         MVC   SPUPFBK,ESTBOOK                                                  
         XC    SPUPFBKL,SPUPFBKL                                                
         MVC   SPUPAOVR,LAOVREL                                                 
         MVC   SPUPUDAY,LUPDAY                                                  
         MVC   SPUPUTIM,LUPTIM                                                  
         MVC   SPUPTYPE(L'LUPGRD),LUPGRD                                        
         MVC   SPUPBTYP,STABKTYP                                                
*                                                                               
         CLI   QBOOKTYP,0                                                       
         BE    *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP                                                
*                                                                               
         TM    SPUPFBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    DEMUP20                                                          
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   DEMUP16              - NOPE                                      
         CLI   LUPFRBKT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPUPBTYP,LUPFRBKT    - YUP                                       
         B     DEMUP18                                                          
*                                                                               
DEMUP16  GOTO1 AGETBKTY,APPARM,(C'B',SPUPFBK+1),SPUPBTYP                        
DEMUP18  NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
DEMUP20  CLI   CUDMED,C'C'         TEST CANADA                                  
         BNE   *+16                                                             
         TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
         BZ    *+8                                                              
         OI    SPUPOPTS,SPOANGFR   YES                                          
*                                                                               
         CLI   G1WPROF+5,C'I'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
*                                                                               
         CLI   G1WPROF+7,C'Y'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         CLI   LUPPUT,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   LUPPUT,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   LUPSHR,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   LUPSHR,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
*                                                                               
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   DEMUP30                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
DEMUP30  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS    CALL SPDEMUP          
*                                                                               
DMUPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST/SELECT DATA                                           *         
* INPUT  : APPARM+0(4) = A(TWA DISPLAY LINE)                          *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   MVC   COMATWAL,APPARM                                                  
         MVC   LSVRECID,APRECID                                                 
         MVC   LSVRECKY,APRECKEY                                                
         CLI   APRECNUM,RECSID     TEST FOR NSID RECORD                         
         BNE   VALS2                                                            
         GOTO1 ADTLBLD             YES-BUILD DETAIL RECORD                      
         BNE   VALSX                                                            
         XC    APRECID,APRECID                                                  
         B     VALS4                                                            
*                                                                               
VALS2    MVC   IOKEY(13),APRECKEY  NO-GET BWS DETAIL RECORD                     
         GOTO1 AMIN,MINRD2                                                      
         BE    VALS4                                                            
         MVC   FVADDR,COMATWAL                                                  
         B     VALSX                                                            
*                                                                               
VALS4    GOTO1 AVALSEL1            MAKE CHANGES TO RECORD                       
         BNE   VALSX                                                            
         CLI   APACTN,ACTDEM       FOR DEMO ACTION --                           
         BNE   VALS10                                                           
         L     RF,COMATWAL         SEE IF ANY CHANGES ON CORRESPONDING          
         LA    RE,WRKL1H           DEMO LINE                                    
         SR    RF,RE                                                            
         SR    RE,RE                                                            
         LA    R1,WRKL2H-WRKL1H                                                 
         DR    RE,R1                                                            
         LTR   RE,RE                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R4,WRKL8H+DLINEDSP                                               
         LH    R1,TWADMLNL                                                      
         MR    RE,R1                                                            
         AR    R4,RF                                                            
         USING DEMLINED,R4                                                      
         ZIC   R0,INODEM                                                        
         LA    R1,DEMRTG1H                                                      
         LA    R8,INODEM+1                                                      
         XC    LFULL,LFULL                                                      
         XC    LUPGRD,LUPGRD                                                    
         MVC   LSVRTG,COMCPPD      SAVE DEMO VALUE FOR CPP CALC                 
         MVC   LSVCOST,COMCPPC     SAVE COST FOR CPP CALC                       
*                                                                               
VALS6    TM    FVIIND-FVIHDR(R1),FVIVAL                                         
         BO    VALS8                                                            
         BAS   RE,VALDEM          VALIDATE DEMO AND CHANGE DEMO ELEMENT         
         BNE   VALSX                                                            
         OI    FVATRB-FVIHDR(R1),FVAHIGH            HIGH INT RATING             
         OI    DEMRTGL+FVATRB-FVIHDR(R1),FVAHIGH    HIGH INT CPP                
         OI    FVIIND-FVIHDR(R1),FVIVAL             PREV VALIDATED BIT          
*                                                                               
VALS8    LA    R1,DEMRCPPL(R1)                                                  
         LA    R8,3(R8)                                                         
         BCT   R0,VALS6                                                         
*                                                                               
         MVC   COMCPPD,LSVRTG      RESTORE DEMO VALUE                           
         MVC   COMCPPC,LSVCOST     RESTORE COST                                 
*                                                                               
VALS10   CLI   APRECNUM,RECSID     FOR NSID RECORD -                            
         BNE   VALS15                                                           
         TM    INOIND,INOINOT      TEST NO TRANSFER                             
         BO    *+12                                                             
         TM    COMCHG,LDEMO+LCOST+LPROG+LDAYTIM   TEST CHANGE                   
         BNZ   VALS11                                                           
         MVC   APRECID,LSVRECID    NO-DON'T TRANSFER                            
         TM    INOIND,INOINOT      TEST NO TRANSFER OPTION                      
         BZ    VALS18              NO-DONE                                      
*                                                                               
VALS11   CLI   COMSLNS,0           TEST MULTIPLE SLNS FOR TRANSFER              
         BE    VALS14                                                           
         LA    R6,COMSLNS          YES-                                         
         LA    R2,L'COMSLNS                                                     
*                                                                               
VALS12   CLI   0(R6),0                                                          
         BE    VALS18                                                           
         L     R3,AIOAREA2                                                      
         CLC   BWDSLN,0(R6)        TEST THIS SLN ALREADY IN THE RECORD          
         BE    VALS14              YES                                          
         MVC   BWDSLN,0(R6)        NO-MOVE THE SPOT LENGTH                      
         TM    INOXFI,INOXFINC     TEST NO COST TRANSFER                        
         BO    VALS13                                                           
         MVC   APRECKEY,LSVRECKY                                                
         LA    R1,APRECKEY                                                      
         USING NSIDKEYD,R1                                                      
         MVC   NSSLN,0(R6)                                                      
         DROP  R1                                                               
****     LA    RF,L'IOAREA1        SAVE THE DETAIL RECORD                       
         LHI   RF,L'IOAREA1        SAVE THE DETAIL RECORD                       
         L     RE,AIOAREA2                                                      
         L     R0,AIOAREA3                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         MVC   APBYTE,TWAFLAG      PROTECT 'NO STATION' FLAG                    
         NI    APBYTE,TWANOSTA                                                  
         GOTO1 ADTLBLD             BUILD DETAIL RECORD WITH SPOT LENGTH         
         BNE   VALSX                                                            
         OC    TWAFLAG,APBYTE                                                   
         L     R1,AIOAREA2                                                      
         L     R3,AIOAREA3                                                      
         MVC   BWDCOST1,BWDCOST1-BWDRECD(R1) AND COSTS                          
         MVC   BWDCOST2,BWDCOST2-BWDRECD(R1) AND EFFECTIVE COSTS/DATES          
         MVC   BWDCOST3,BWDCOST3-BWDRECD(R1)                                    
         MVC   BWDEFDT2,BWDEFDT2-BWDRECD(R1)                                    
         MVC   BWDEFDT3,BWDEFDT3-BWDRECD(R1)                                    
****     LA    RF,L'IOAREA1        MOVE RECORD BACK                             
         LHI   RF,L'IOAREA1        MOVE RECORD BACK                             
         L     RE,AIOAREA3                                                      
         L     R0,AIOAREA2                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         L     R3,AIOAREA2                                                      
*                                                                               
VALS13   TM    COMCHG,LCOST        TEST COST OVERRIDE ON SCREEN                 
         BZ    VALS14                                                           
         MVC   BWDCOST1,COMCPPC    YES-USE COST OVERRIDE                        
         CLI   COMSLNS+1,0         TEST MORE THAN ONE SLN                       
         BE    VALS14              NO-USE THE OVERRIDE                          
         CLI   0(R6),30            YES-TEST SLN=30                              
         BE    VALS14                                                           
*                                  NO-EQUIV COST AGAINST 30 SEC                 
         GOTO1 ACOSTEQU,APPARM,(30,COMCPPC),(0(R6),BWDCOST1)                    
*                                                                               
VALS14   TM    INOIND,INOINOT      TEST NO TRANSFER OPTION SET                  
         BO    VALS16              YES                                          
         L     R3,AIOAREA2                                                      
         TM    COMCHG,LDAYTIM      NO-TEST DAY/TIME CHANGE                      
         BZ    *+16                                                             
         MVC   BWDKELDY,BWDDAYS    YES-MAKE SURE KEY REFLECTS RECORD            
         MVC   BWDKELTM,BWDTIMCD       DAYS/TIMES                               
         BAS   RE,XFRADD           TRANSFER - ADD RECORD                        
         BNE   VALSX                                                            
         B     VALS16                                                           
*                                                                               
VALS15   GOTO1 AVALSEL2            PUT CHANGED BWS RECORD                       
         L     R3,AIOAREA2                                                      
*                                                                               
VALS16   L     R8,COMATWAL         REDISPLAY THE RECORD                         
         GOTO1 ADISPSEL                                                         
         BNE   VALSX                                                            
         CLI   APACTN,ACTDEM       FOR DEMO ACTION --                           
         BNE   *+14                                                             
         MVC   LSTA,BWDSTA                                                      
         BAS   RE,DISDEM           REDISPLAY DEMO LINE                          
*                                                                               
         CLI   COMSLNS,0           TEST MULTIPLE SLN NSID TRANSFER              
         BE    VALS18                                                           
         LA    R6,1(R6)            YES-NEXT SPOT LENGTH                         
         BCT   R2,VALS12                                                        
*                                                                               
VALS18   CLI   COMSLNS+1,0         TEST MORE THAT ONE SLN FOR SLN=              
         BE    *+8                                                              
         OI    TWAACTIV,TWAACSCR   YES-THEN RE-SCROLL TO CURRENT SCREEN         
*                                                                               
         OI    TWAACTIV,TWAACLIN                                                
         B     VALSX                                                            
*                                                                               
VALSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE DEMO OVERRIDE AND CHANGE THE DEMO ELEMENT       *         
* INPUT  : R1 = A(FIELD HEADER)                                       *         
*          R8 = A(DEMO)                                               *         
*          LFULL = A(DEMO ELEMENT) OR 0 FOR FIRST TIME                *         
***********************************************************************         
         SPACE 1                                                                
VALDEM   NTR1                                                                   
         ST    R1,APFULL                                                        
         L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         NI    LFLAG,255-LDEMUP                                                 
         GOTO1 AFVAL               VALIDATE THE FIELD                           
         BH    VDEMX                                                            
         BE    VDEM10                                                           
         MVC   LDEMS(3),0(R8)      MISSING-DO UPGRADE                           
         MVI   LDEMS+3,FF                                                       
         OC    LUPGRD,LUPGRD                                                    
         BNZ   VDEM8                                                            
         MVC   LUPSTA,BWDSTA                                                    
         MVC   LACTDAY,BWDDAYS                                                  
         MVC   LACTTIME,BWDTIMES                                                
         XC    LUPDAY,LUPDAY                                                    
         XC    LUPTIM,LUPTIM                                                    
         XC    LAOVREL,LAOVREL                                                  
         OC    CMPUP,CMPUP                                                      
         BZ    VDEM2                                                            
         MVC   LUPFIL,CMPUF                                                     
         MVC   LUPFRBK,CMPFB                                                    
         TM    CMPFB+1,BTY2CHAR                                                 
         BNO   VDEM1G                                                           
         CLI   CMPFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LUPFRBKT(1),CMPFBTP                                              
VDEM1G   MVC   LUPFRBKL(6),CMPFBLST                                             
         MVC   LUPFRBKL,CMPFBLST                                                
         MVC   LUPGRD,CMPUP                                                     
         MVC   LUPPUT,CMPUPUT                                                   
         MVC   LUPSHR,CMPUSHR                                                   
*                                                                               
VDEM2    SR    R0,R0                                                            
         LA    R4,BWDEL                                                         
         XC    APDUB,APDUB                                                      
*                                                                               
VDEM4    CLI   0(R4),0                                                          
         BE    VDEM6                                                            
         LA    R1,APDUB                                                         
         CLI   0(R4),UPGELCDQ                                                   
         BE    *+16                                                             
         LA    R1,4(R1)                                                         
         CLI   0(R4),ODTELCDQ                                                   
         BNE   *+8                                                              
         ST    R4,0(R1)                                                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VDEM4                                                            
*                                                                               
VDEM6    ICM   R4,15,APDUB                                                      
         BZ    VDEM8                                                            
         USING UPGEL,R4                                                         
         MVC   LUPFIL,UPGFILE                                                   
         MVC   LUPFRBK,UPGFRBK                                                  
         TM    UPGFRBK+1,BTY2CHAR                                               
         BNO   VDEM7G                                                           
         CLI   UPGELLN,UPGELLNQ                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   UPGFRBKT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LUPFRBKT,UPGFRBKT                                                
VDEM7G   CLI   UPGELLN,51                                                       
         BL    *+10                                                             
         MVC   LUPFRBKL,UPGFRBKL                                                
         MVC   LUPGRD,UPGRADE                                                   
         MVC   LUPPUT,BWDUPUT                                                   
         MVC   LUPSHR,BWDUSHR                                                   
         ICM   R4,15,APDUB+4                                                    
         BZ    VDEM8                                                            
         USING ODTEL,R4                                                         
         MVC   LUPDAY,ODTDAY                                                    
         MVC   LUPTIM,ODTTIME                                                   
*                                                                               
VDEM8    BAS   RE,DEMUP                                                         
         MVC   APPARM+4(4),LDEMVALS                                             
         OI    LFLAG,LDEMUP                                                     
         B     VDEM12                                                           
*                                                                               
VDEM10   ZIC   RE,FVILEN                                                        
         ST    RE,APPARM+4                                                      
         GOTO1 VCASHVAL,APPARM,(1,FVIFLD)    VALIDATE RATING                    
         CLI   APPARM,FF                                                        
         BE    VDEM99                                                           
*                                                                               
VDEM12   ICM   R4,15,LFULL                                                      
         BNZ   VDEM16                                                           
         SR    R0,R0               FIND THE DEMO ELEMENT                        
         LA    R4,BWDEL                                                         
*                                                                               
VDEM14   CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VDEM14                                                           
         USING DMOEL,R4                                                         
         ST    R4,LFULL            LFULL = A(DEMO ELEMENT)                      
*                                                                               
VDEM16   LA    RE,L'DMODEMO        FIND THE DEMO IN THE DEMO ELEMENT            
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         LR    R1,R4                                                            
         LA    R4,DMODEMO                                                       
         CLC   1(2,R4),1(R8)       POINT R4 TO CORRECT DEMO                     
         BE    VDEM18                                                           
         BXLE  R4,RE,*-10                                                       
         XC    APELEM,APELEM       DEMO NOT FOUND IN ELEMENT                    
         ZIC   R5,1(R1)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R1)     SAVE CURRENT ELEMENT                         
         GOTO1 ADELELS,BWDRECD     DELETE IT FROM RECORD                        
         LA    R5,1+L'DMODEMO(R5)  LENGTHEN THE ELEMENT BY ONE DEMO             
         STC   R5,APELEM+1                                                      
         GOTO1 AADDELS,BWDRECD     ADD BACK THE ELEMENT                         
         MVC   0(3,R4),0(R8)       MOVE NEW DEMO INTO THE ELEMENT               
*                                                                               
VDEM18   CLC   5(3,R4),APPARM+5    TEST FOR CHANGE                              
         BE    VDEM90                                                           
         XC    LDEMOLD,LDEMOLD     YES - SAVE OLD RATING                        
***      MVC   LDEMOLD+1(3),5(R4)                                               
         MVC   LDEMOLD,4(R4)                                                    
         MVC   4(4,R4),APPARM+4          MOVE IN NEW RATING                     
         TM    LFLAG,LDEMUP              TEST UPGRADE USED                      
         BO    *+8                                                              
         OI    4(R4),DMODEMOV            NO-INDICATE OVERRIDE                   
*                                                                               
         TM    INFIND,INFINOAD     TEST OPTION TO NOT AUTO ADJUST DEMOS         
         BO    VDEM20                                                           
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT  NO-TEST FOR AUTO             
         BZ    VDEM20                              ADJUSTMENTS                  
**       OC    LDEMOLD,LDEMOLD     YES - TEST OLD DEMO VALUE = 0                
**       BZ    VDEM98                    YES - ERROR                            
         BAS   RE,DEMADJ                 NO  - ADJUST                           
         BNE   VDEM98              OLD DEMO VALUE = 0                           
*                                                                               
VDEM20   OI    COMCHG,LDEMO        INDICATE DEMO CHANGE                         
         MVC   COMCPPD,APPARM+4    MOVE IN RATING FOR CPP CALC                  
         MVC   COMCPPC,BWDCOST1    GET COST FOR CPP CALC                        
         CLI   APRECNUM,RECSID                                                  
         BE    VDEM22                                                           
         TM    APRECID,RIEFFDT2                                                 
         BZ    *+14                                                             
         MVC   COMCPPC,BWDCOST2                                                 
         B     VDEM22                                                           
         TM    APRECID,RIEFFDT3                                                 
         BZ    VDEM22                                                           
         MVC   COMCPPC,BWDCOST3                                                 
*                                                                               
VDEM22   BAS   RE,CALCCPP          CALCULATE CPP                                
         CLC   1(2,R4),SVDEMO1     TEST THIS IS TARGET DEMO                     
         BNE   *+10                                                             
         MVC   APRECID+1(3),LCOSTPP+1   YES - SAVE CPP FOR DISPLAY              
         XC    APWORK,APWORK                                                    
         XC    EBLOCK,EBLOCK       FORMAT CPP                                   
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         LA    RE,LCOSTPP                                                       
         ST    RE,EBAIN                                                         
         LA    RE,APWORK                                                        
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'DEMCPP1                                                 
         MVI   EBDECS,2                                                         
         CLC   LCOSTPP,=F'100000'                                               
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         L     R1,APFULL                                                        
         MVC   DEMRTGL+L'DEMCPP1H(L'DEMCPP1,R1),APWORK                          
         OI    DEMRTGL+6(R1),FVOXMT                                             
*                                                                               
VDEM90   MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VDEMX                                                            
*                                                                               
VDEM98   MVC   FVMSGNO,=AL2(FVIDADJ)                                            
         B     VDEMX                                                            
*                                                                               
VDEM99   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VDEMX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* AUTO DEMO ADJUSTMENT ROUTINE                                        *         
* INPUT  : R8 = A(DEMO CODE)                                          *         
*          LFULL = A(DEMO ELEMENT)                                    *         
*          LDEMOLD = OLD RATING                                       *         
*          APPARM+4(4) = NEW RATING                                   *         
***********************************************************************         
         SPACE 1                                                                
DEMADJ   NTR1                                                                   
         NI    LFLAG,FF-LADJALL                                                 
         OI    LFLAG,LADJIMP       ADJUST TARGET IMPRESSION                     
         TM    CMPOPTS,CAMOAALL+CAMOATGT  TEST AUTOADJ=ALL/TGT                  
         BZ    DEMA2                                                            
         MVC   APHALF,=X'D901'                                                  
         TM    CMPOPTS,CAMOAALL                                                 
         BO    *+10                                                             
         MVC   APHALF,ESTDEMS+1                                                 
         CLC   APHALF,1(R8)        YES-TEST TARGET IS ADJUSTMENT DEMO           
         BNE   DEMA2                                                            
         OI    LFLAG,LADJALL       YES                                          
         B     DEMA4                                                            
*                                                                               
DEMA2    CLI   1(R8),C'R'          TEST TARGET IS A RATING                      
         BE    DEMA4                                                            
         CLI   1(R8),C'E'          TEST TARGET IS A RATING                      
         BNE   DEMADJYS            NO - NO ADJUSTMENTS                          
*                                                                               
DEMA4    DS    0H                                                               
***  2 DECIMAL                                                                  
         XC    APWORK+64(16),APWORK+64                                          
         MVC   APWORK+64(4),LDEMOLD   OLD TARGET RATING                         
         MVC   APWORK+68(4),APPARM+4   NEW TARGET RATING                        
*                                                                               
         NI    APWORK+64,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         NI    APWORK+68,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         OC    APWORK+64(4),APWORK+64   ANYTHING IN THE OLD RATING?             
         BZ    DEMADJNO                  - NOPE                                 
***                                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BZ    DEMA5                - NOPE WE'RE NOT                            
*                                                                               
         TM    LDEMOLD,X'40'       2 DECIMAL?                                   
         BNZ   DEMA4E               - YUP, NOTHING TO DO                        
         L     R1,APWORK+64                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+64                                                     
DEMA4E   TM    4(R4),X'40'         2 DECIMAL?                                   
         BNZ   DEMA5                                                            
         L     R1,APWORK+68                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+68                                                     
*****  BOTH APFULL AND LNEWRTG SHOULD BE IN 2 DECIMAL MODE BY NOW               
***  2 DECIMAL                                                                  
DEMA5    L     R1,APWORK+64        CALCULATE PCT ADJUSTMENT                     
         SR    RE,RE                                                            
         L     RF,APWORK+68                                                     
         M     RE,=F'2000'                                                      
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,LDEMADJ          LDEMADJ = PCT ADJUSTMENT                     
*                                                                               
         L     R1,LFULL            SCAN ALL DEMOS IN DEMO ALEMENT               
         USING DMOEL,R1                                                         
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,DMODEMO                                                       
         LA    RE,L'DMODEMO                                                     
         DROP  R1                                                               
*                                                                               
DEMA6    CLC   1(2,R1),1(R8)       TEST OUR DEMO                                
         BE    DEMA12              YES - NO ADJ                                 
         TM    4(R1),DMODEMOV      TEST ALREADY MANUALLY OVERRIDDEN             
         BO    DEMA12              YES - NO ADJ                                 
         TM    LFLAG,LADJIMP       TEST TARGET IMPRESSION ADJUST                
         BZ    DEMA8                                                            
         CLI   1(R1),C'I'          YES - TEST ITS THE TARGET IMP                
         BNE   DEMA8                                                            
         CLC   2(1,R1),2(R8)                                                    
         BE    DEMA10                    YES - ADJUST                           
*                                                                               
DEMA8    TM    LFLAG,LADJALL       TEST ADJUST ALL                              
         BZ    DEMA12              NO                                           
         CLI   1(R1),C'I'          YES - TEST ITS AN IMPRESSION                 
         BNE   DEMA10                    NO - GO AND ADJUST                     
         L     R4,LFULL                  YES - CHECK THAT ITS RATING            
         LA    R4,DMODEMO-DMOEL(R4)            HAS NOT BEEN MANUALLY            
         CLI   1(R4),C'R'                      OVERRIDDEN                       
         BNE   *+14                                                             
         CLC   2(1,R4),2(R1)                                                    
         BE    *+12                                                             
         BXLE  R4,RE,*-18                                                       
         B     DEMA10                                                           
         TM    4(R4),DMODEMOV                                                   
         BO    DEMA12                                                           
*                                                                               
DEMA10   SR    R2,R2               DO THE ADJUSTMENT                            
         ICM   R2,7,5(R1)                                                       
         SR    R3,R3                                                            
         SRDA  R2,31                                                            
         M     R2,LDEMADJ                                                       
         D     R2,=F'1000'                                                      
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
***  2 DECIMAL                                                                  
         MVI   APBYTE,0                                                         
         TM    4(R1),X'40'         IS IT PREVIOUSLY 2 DECIMAL?                  
         BZ    DEMA11               - NOPE                                      
         OI    APBYTE,X'40'         - YUP, NEED THE 2 DECIMAL BIT ON            
***  2 DECIMAL                                                                  
DEMA11   ST    R3,4(R1)            STORE ADJUSTED DEMO IN ELEMENT               
         TM    APBYTE,X'40'                                                     
         BZ    DEMA12                                                           
         OI    4(R1),X'40'                                                      
*                                                                               
DEMA12   BXLE  R1,RE,DEMA6         NEXT DEMO                                    
DEMADJYS CR    RE,RE               SET EQUAL CONDITION                          
         B     DEMAX                                                            
*                                                                               
DEMADJNO XR    R1,R1                                                            
         CR    RB,R1               SET != CONDITION                             
*                                                                               
DEMAX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS LIST/SELECT ACTION                                          *         
* FOR NSID RECORD, TRANSFERS THE RECORD TO THE WORKSHEET (SEL CODE=X) *         
* FOR WORK RECORD, THE RECORD IS ERASED (SELECTION CODE=K)            *         
* INPUT  : APPARM(4) = A(TWA LINE)                                    *         
***********************************************************************         
         SPACE 1                                                                
PROCLS   CLI   APRECNUM,RECSID     TEST NSID RECORD                             
         BNE   PROC4                                                            
         MVC   COMATWAL,APPARM     YES --                                       
         GOTO1 ADTLBLD             BUILD DETAIL RECORD                          
         BNE   PROCX                                                            
         TM    INOIND,INOINOT      TEST NO TRANSFER OPTION SET                  
         BZ    PROC1                                                            
         GOTO1 AVALSEL1            YES-PICK UP EXISTING CHANGES                 
         BNE   PROCX                                                            
*                                                                               
PROC1    BAS   RE,XFRADD           ADD DETAIL RECORD                            
         BNE   PROCX                                                            
******** L     R8,COMATWAL                                                      
******** GOTO1 ADISPSEL            REDISPLAY THE RECORD                         
*                                                                               
         CLI   APACTN,ACTTFR       TEST FOR TRANSFER FOLLOWED BY LFM            
         BNE   PROC2                                                            
         MVI   APMODE,APMPLFM      YES - SET MODE                               
         MVI   APACTN,ACTCHA             SET ACTION TO CHANGE                   
         L     R1,ALSM                                                          
         MVI   LSMSACT-LSMD(R1),ACTCHA                                          
         MVI   INREC,RECWRK              SET RECORD TO WORK                     
         B     PROCX                                                            
*                                                                               
PROC2    CLI   APACTN,ACTXFR       NO - TEST FOR TRANSFER ONLY                  
         BNE   PROCX                                                            
         OI    TWAACTIV,TWAACXFR        YES - SHOW IT                           
         OI    TWAACTIV,TWAACLIN              SHOW LINE ACTION                  
         B     PROCX                                                            
*                                                                               
PROC4    BAS   RE,ERASE            WORK RECORD-ERASE IT                         
*                                                                               
PROCX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERASE A RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
ERASE    NTR1  ,                                                                
         MVC   IOKEY,APRECKEY                                                   
         GOTO1 AMIN,MINRD2                                                      
         BNE   ERASE1              RECORD NOT FOUND-MUST HAVE BEEN              
*                                  DELETED ONCE BEFORE!                         
         GOTO1 AMIN,MINDEL                                                      
*                                                                               
ERASE1   CLI   BWDKELPO,0          TEST PACKAGE/ORBIT RECORD                    
         BE    ERASEX                                                           
         CLI   BWDKELSQ,0          YES-TEST PACKAGE/ORBIT SLAVE                 
         BNE   ERASEX              YES-EXIT                                     
*                                  NO-THEN IT'S A PACKAGE/ORBIT MASTER          
         MVC   IOKEY,APRECKEY      DELETE ASSOCIATED SLAVES                     
         LA    R1,MINHI2                                                        
         B     ERASE2+4                                                         
*                                                                               
ERASE2   LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     ERASEX                                                           
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   ERASEX                                                           
*                                                                               
ERASE4   GOTO1 AMIN,MINDEL                                                      
         B     ERASE2              READ ALL PACKAGE RECORDS                     
*                                                                               
ERASEX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET DAY CODE WHICH WILL CAUSE DAYS TO SORT IN ASCENDING ORDER       *         
* INPUT  : R1 = A(DAYPAK DAY CODE)                                    *         
* OUTPUT : LDAY = SORT DAY CODE                                       *         
***********************************************************************         
         SPACE 1                                                                
GETDAY   NTR1                                                                   
         MVI   LDAY,X'FF'                                                       
         CLI   0(R1),0             TEST PACKAGE/ORBIT MASTER                    
         BE    GDAYX               YES-SORT LAST                                
         MVI   LDAY,0                                                           
         CLI   0(R1),X'7C'         M-F                                          
         BE    GDAYX                                                            
         MVI   LDAY,1                                                           
         CLI   0(R1),X'7F'         M-SU                                         
         BE    GDAYX                                                            
         SR    RF,RF                                                            
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)                                                       
         SRDL  R0,9                                                             
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   RF,*-10                                                          
         LPR   RF,RF                                                            
         STC   RF,LDAY                                                          
GDAYX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CALCULATE CPP                                                       *         
* INPUT  : COMCPPC = COST                                             *         
*          COMCPPD = RATING                                           *         
* OUTPUT : LCOSTCPP = CPP                                             *         
***********************************************************************         
         SPACE 1                                                                
CALCCPP  XC    LCOSTPP,LCOSTPP                                                  
***  2 DECIMAL                                                                  
         TM    COMCPPD,X'40'       WE DOING 2 DECIMALS?                         
         BO    CALCCPP5             - YES WE ARE                                
***  2 DECIMAL                                                                  
         OC    COMCPPD,COMCPPD                                                  
         BZR   RE                                                               
         L     R1,COMCPPC                                                       
         M     R0,=F'20'                                                        
         D     R0,COMCPPD                                                       
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         ST    R1,LCOSTPP                                                       
         BR    RE                                                               
*                                                                               
***  2 DECIMAL                                                                  
CALCCPP5 NI    COMCPPD,FF-X'40'    TAKE OFF 2 DECIMAL BIT                       
         OC    COMCPPD,COMCPPD                                                  
         BZR   RE                                                               
         L     R1,COMCPPC                                                       
         M     R0,=F'200'          200 INSTEAD OF 20 (2 DECIMAL)                
         D     R0,COMCPPD                                                       
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         ST    R1,LCOSTPP                                                       
         BR    RE                                                               
***  2 DECIMAL                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TRANFER NSID RECORD TO BWS                               *         
***********************************************************************         
         SPACE 1                                                                
XFRADD   NTR1  ,                                                                
         TM    LFLAG,LNEWMIN       TEST FIRST ADD OF TRANSACTION                
         BZ    XFR2                                                             
         XC    IOKEY,IOKEY         YES-TEST IF MINIO DETAIL RECORD SET          
         L     R1,AIOAREA2         EXISTS                                       
         MVC   IOKEY(BWDKEL-BWDKEY),0(R1)                                       
         GOTO1 AMIN,MINHI3                                                      
         BNE   *+18                                                             
         CLC   IOKEY(BWDKEL-BWDKEY),IOKEYSAV                                    
         BNE   *+8                                                              
         NI    LFLAG,255-LNEWMIN   YES                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
XFR2     GOTO1 AXFRADD                                                          
         BNE   XFRX                                                             
         TM    LFLAG,LNEWMIN       TEST NEW MINIO DETAIL RECORD SET             
         BZ    XFR4                                                             
         GOTO1 AMIN,MINCLS         YES-CLOSE MINIO (TO AVOID ERROR ON           
         NI    LFLAG,255-LNEWMIN                    FIRST RECORD SPLIT)         
*                                                                               
XFR4     ICM   R4,15,COMATWAL                                                   
         BZ    XFRX                                                             
         USING WRKL1H,R4                                                        
         OI    WRKLSTH+6,FVOXMT    MARK LINE WITH TRANSFER INDICATOR            
         LA    R4,WRKLST                                                        
         USING LIST1D,R4                                                        
         MVI   LISTLINE,C'+'                                                    
         B     XFRX                                                             
         DROP  R4                                                               
*                                                                               
XFRX     CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EDEMOP   MVC   FVMSGNO,=AL2(FVIDEMOP)                                           
         LA    R1,BWSOPTH                                                       
         B     ERREXR1                                                          
ESLNOP   MVC   FVMSGNO,=AL2(FVISLNOP)                                           
         LA    R1,BWSOPTH                                                       
         B     ERREXR1                                                          
ESCH     MVC   FVMSGNO,=AL2(FVNOSCHM)                                           
         B     EXIT                                                             
EPER     MVC   FVMSGNO,=AL2(FVNOPER)                                            
         B     EXIT                                                             
ETMR     MVC   FVMSGNO,=AL2(FVTMR)                                              
         B     EXIT                                                             
EFLTDPT  MVC   FVMSGNO,=AL2(FVFLTDPT)                                           
         B     EXIT                                                             
EPF4     MVC   FVMSGNO,=AL2(FVPF4)                                              
         B     EXIT                                                             
*                                                                               
ERREXR1  ST    R1,FVADDR                                                        
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
FF       EQU   X'FF'                                                            
NLSTLINS EQU   7                                                                
NDEMLINS EQU   7                                                                
*                                                                               
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
SPACES   DC    CL16' '                                                          
LENTABLE DC    0XL1                                                             
       ++INCLUDE SPSLNTAB                                                       
         DC    AL1(0)                                                           
*                                                                               
SLNPFKS  DC    AL1(PFK07),CL2'10'    SPOT LENGTH PF KEYS                        
         DC    AL1(PFK08),CL2'15'                                               
         DC    AL1(PFK09),CL2'30'                                               
         DC    AL1(PFK10),CL2'45'                                               
         DC    AL1(PFK11),CL2'60'                                               
         DC    AL1(0)                                                           
*                                                                               
FUTLINE  DC    CL40'PF7=10  PF8=15  PF9=30  PF10=45  PF11=60'                   
*                                                                               
LHSHEAD  DC    CL27'--STA-- --DAYS- ---TIMES---'                                
*                                                                               
DHED1DSP EQU   0                   TWA DISPLACEMENTS                            
DHED2DSP EQU   DHED1DSP+25+8+51+8                                               
DLINEDSP EQU   DHED2DSP+51+8                                                    
*                                                                               
TWABELS  DS    0C                    TWABLD ELEMENTS                            
TWABRHD1 DC    XL7'013A011E332800'   RHS HEADLINE 1                             
TWABLHED DC    XL7'01200102192800'   LHS HEADLINE                               
TWABRHD2 DC    XL7'013A001E332800'   RHS HEADLINE 2                             
TWABLINE DC    XL7'012001021B2000'   LHS - STATION/DAYS/TIMES                   
TWABLRHS DC    XL7'010C001E050000'   RHS - RATING 1 (UNPROT)                    
         DC    XL7'010D0024062000'         CPP 1                                
         DC    XL7'010C002B050000'         RATING 2 (UNPROT)                    
         DC    XL7'010D0031062000'         CPP 2                                
         DC    XL7'010C0038050000'         RATING 3 (UNPROT)                    
         DC    XL7'010D003E062000'         CPP 3                                
         DC    XL7'010C0045050000'         RATING 4 (UNPROT)                    
         DC    XL7'010D004B062000'         CPP 4                                
         DC    XL1'00'                                                          
         EJECT                                                                  
DEMLINED DSECT                     DEMO LINE DSECT                              
*                                                                               
DEMSDTH  DS    CL8                                                              
DEMSDT   DS    0CL27                                                            
DEMSTA   DS    CL7                 STATION                                      
         DS    X                                                                
DEMDAY   DS    CL7                 DAYS                                         
         DS    X                                                                
DEMTIME  DS    CL11                TIMES                                        
*                                                                               
DEMRTG1H DS    CL8                 RATING                                       
DEMRTG1  DS    CL5                                                              
DEMRTGL  EQU   *-DEMRTG1H                                                       
DEMCPP1H DS    CL8                 CPP                                          
DEMCPP1  DS    CL6                                                              
DEMRCPPL EQU   *-DEMRTG1H                                                       
DEMRTG2H DS    CL8                                                              
DEMRTG2  DS    CL5                                                              
DEMCPP2H DS    CL8                                                              
DEMCPP2  DS    CL6                                                              
DEMRTG3H DS    CL8                                                              
DEMRTG3  DS    CL5                                                              
DEMCPP3H DS    CL8                                                              
DEMCPP3  DS    CL6                                                              
DEMRTG4H DS    CL8                                                              
DEMRTG4  DS    CL5                                                              
DEMCPP4H DS    CL8                                                              
DEMCPP4  DS    CL6                                                              
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
COMWRK   DS    0C                  COMMON BETWEEN BWS05/BWS06                   
*                                                                               
       ++INCLUDE SPNWS05WRK                                                     
         EJECT                                                                  
LOCALD   DSECT                                                                  
         ORG   LOCALD+2048                                                      
         DS    0H              *** RESERVED FOR BWS05 ***                       
         SPACE                                                                  
         ORG   LOCALD+3072     *** BWS06 WORK AREA ***                          
LASAVE   DS    A                                                                
LARECTAB DS    A                                                                
LANXTREC DS    A                                                                
LARCTABX DS    A                                                                
LADEMLIN DS    A                                                                
LAOVREL  DS    A                                                                
LFULL    DS    F                                                                
LCOSTPP  DS    F                                                                
LDEMOLD  DS    F                                                                
LDEMADJ  DS    F                                                                
LUPFIL   DS    CL1                                                              
LUPGRD   DS    XL8                                                              
LUPFRBK  DS    XL2                                                              
LUPFRBKT DS    XL1                 LUPFRBK BOOKTYPE                             
LUPFRBKL DS    XL6                                                              
LUPSTA   DS    CL5                                                              
LUPDAY   DS    XL1                                                              
LUPTIM   DS    XL4                                                              
LUPPUT   DS    CL1                                                              
LUPSHR   DS    CL1                                                              
LACTCOST DS    XL4                                                              
LACTDAY  DS    XL1                                                              
LACTTIME DS    XL4                                                              
LSTA     DS    CL8                                                              
LSVRECID DS    XL(L'APRECID)                                                    
LSVRECKY DS    XL(L'APRECKEY)                                                   
LDMUPBLK DS    (SPDEMUP2)X                                                      
LDEMS    DS    (LNDEMOS)XL3                                                     
         DS    X                                                                
LDEMVALS DS    (LNDEMOS)XL4                                                     
LDAY     DS    X                                                                
LSVRTG   DS    XL4                                                              
LSVCOST  DS    XL4                                                              
LSLNLST  DS    XL16                                                             
LRECIND  DS    CL1                                                              
*                                                                               
LFLAG    DS    X                                                                
LFSTLINE EQU   X'80'                                                            
LOVRCOST EQU   X'40'                                                            
LADJIMP  EQU   X'20'                                                            
LADJALL  EQU   X'10'                                                            
LDEMUP   EQU   X'08'                                                            
LNEWMIN  EQU   X'04'                                                            
         SPACE 1                                                                
         ORG   LOCALD+4096                                                      
         SPACE 1                                                                
LOCALX   EQU   *                                                                
         EJECT                                                                  
LRECTABD DSECT                     RECORD TABLE ENTRY DSECT                     
*                                                                               
RTDPT    DS    CL1                                                              
RTSLN    DS    XL1                                                              
RTDAYCOD DS    XL1                                                              
RTDAY    DS    XL1                                                              
RTTIME   DS    XL4                                                              
RTSTA    DS    CL8                                                              
RTDPT2   DS    CL1                                                              
RTSLN2   DS    XL1                                                              
LRECKEYL EQU   *-LRECTABD                                                       
*                                                                               
RTRECID  DS    XL4                              +0(1) RECORD ID                 
*                                  NSID RECORD: +1(3) STATION                   
RTELKEY  DS    XL6                 ELEMENT KEY                                  
         ORG   RTELKEY                                                          
RTPRG    DS    CL1                 NSID PROGRAM TYPE                            
RTYEAR   DS    XL1                 NSID YEAR                                    
RTPER    DS    CL4                 NSID PERIOD                                  
         ORG                                                                    
RTINDS   DS    XL1                 RECORD INDICATOR                             
RTIPKG   EQU   X'80'               PACKAGE RECORD                               
RTIORB   EQU   X'40'               ORBIT RECORD                                 
*                                                                               
LRECTABL EQU   *-LRECTABD                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSFBD                                                       
         EJECT                                                                  
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPDEMUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDTWABLDD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENSIR                                                                      
         PRINT OFF                                                              
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* FACHKPT                                                                       
         PRINT OFF                                                              
       ++INCLUDE FACHKPT                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'160SPNWS06   02/26/07'                                      
         END                                                                    
