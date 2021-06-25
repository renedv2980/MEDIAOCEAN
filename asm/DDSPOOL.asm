*          DATA SET DDSPOOL    AT LEVEL 024 AS OF 05/12/20                      
*PHASE T00A0CA                                                                  
         TITLE 'SPOOL - ON LINE PRINT CONTROLLER'                               
SPOOL    CSECT                                                                  
         DS    4000C                                                            
         ORG   SPOOL                                                            
         PRINT NOGEN                                                            
         NMOD1 SPOOLERX-SPOOLERD,**SPUL**,R9,RR=RE                              
         USING SPOOLERD,RC                                                      
         ST    RE,RELO                                                          
         MVC   USERRD,4(RD)        SAVE USERS RD                                
         L     R8,0(R1)                                                         
         USING SPOOLD,R8                                                        
         MVC   DATCON,RCDATCON                                                  
         EJECT                                                                  
*              SET UP FOR 132 OR 198 PRINT SUPPORT                              
         SPACE 3                                                                
         MVC   PWIDTH,=F'132'      PRESET FOR REGULAR PRINT LINES               
         MVI   WIDEOPT,C'N'                                                     
         LA    R1,HEAD1                                                         
         ST    R1,AH1                                                           
         LA    R1,MID1                                                          
         ST    R1,AM1                                                           
         LA    R1,MID2                                                          
         ST    R1,AM2                                                           
         LA    R1,P                                                             
         ST    R1,AP1                                                           
         LA    R1,P4                                                            
         ST    R1,AP4                                                           
         SPACE 1                                                                
         L     RA,ABOX                                                          
         LTR   RA,RA                                                            
         BZ    SPOOL1                                                           
         USING BOXD,RA                                                          
         CLC   BOXWIDTH,=F'132'    CHECK FOR WIDE PRINT                         
         BNH   SPOOL1                                                           
         MVC   PWIDTH,=F'198'      SET UP FOR WIDE PRINT LINES                  
         MVI   WIDEOPT,C'Y'                                                     
         L     R2,BOXAWIDE                                                      
         USING WIDED,R2                                                         
         LA    R1,XHEAD1                                                        
         ST    R1,AH1                                                           
         LA    R1,XMID1                                                         
         ST    R1,AM1                                                           
         LA    R1,XMID2                                                         
         ST    R1,AM2                                                           
         LA    R1,XP                                                            
         ST    R1,AP1                                                           
         LA    R1,XP4                                                           
         ST    R1,AP4                                                           
         DROP  R2                                                               
         SPACE 1                                                                
SPOOL1   MVI   MYSPACES,C' '                                                    
         MVC   MYSPACES+1(197),MYSPACES                                         
         NI    SPOOLIND,255-(SPHHOOK+SPMHOOK+SPFHOOK)                           
         L     R1,PWIDTH                                                        
         BCTR  R1,0                                                             
         ST    R1,EXWIDE           WIDTH-1 FOR EX INSTRUCTIONS                  
         EJECT                                                                  
*              FIGURE OUT WHERE WE ARE GOING                                    
         SPACE 3                                                                
         CLI   SPMODE,0                                                         
         BNE   SPOOL1A                                                          
         BAS   RE,SPINIT                                                        
         MVI   SPMODE,1                                                         
         B     XIT                                                              
         SPACE 1                                                                
SPOOL1A  CLI   SPMODE,X'80'        TEST CLOSE WITH REOPEN FOR OFFLINE           
         BNE   SPOOL2              FORCE OF MULTIPLE PQ INTERVALS               
         BAS   RE,SPENDF            FORCE CLOSE                                 
         BAS   RE,SPINIT            NEW OPEN                                    
         MVI   SPMODE,1                                                         
         B     XIT                                                              
         SPACE 1                                                                
SPOOL2   CLI   SPMODE,X'FF'        TEST REGULAR CLOSE                           
         BNE   SPOOL3                                                           
         BAS   RE,SPEND                                                         
         B     XIT                                                              
         SPACE 1                                                                
SPOOL3   CLI   SPMODE,X'FD'        TEST CLOSE ERROR                             
         BNE   SPOOL3A                                                          
         CLI   SPOOLKEY,0          TEST IF REPORT GENERATED                     
         BE    XIT                                                              
         GOTO1 SPOOLDM,SPPARA,=C'CLO/ERR',=C'PRTQUE',0,SPOOLKEY,       *        
               SPOOLBUF                                                         
         CLI   8(R1),0             CHECK FOR ERROR                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SPOOLBUF                                                      
         USING PQRECD,R1                                                        
         MVC   SPOOLPAG,PQPAGES                                                 
         MVC   SPOOLLIN,PQLINES+1                                               
         TM    PQTYPE,PQTYNEW                                                   
         BO    XIT                                                              
         MVC   SPOOLPAG,PQPAGES                                                 
         MVC   SPOOLLIN,PQLINES+1                                               
         B     XIT                                                              
         SPACE 1                                                                
SPOOL3A  CLI   SPMODE,X'FE'        TEST PURGE CLOSE                             
         B     SPOOL4                                                           
         CLI   SPOOLKEY,0          TEST IF REPORT GENERATED                     
         BE    XIT                                                              
         GOTO1 SPOOLDM,SPPARA,=C'CLO/PUR',=C'PRTQUE',0,SPOOLKEY,       *        
               SPOOLBUF                                                         
         CLI   8(R1),0             CHECK FOR ERROR                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SPOOLBUF                                                      
         USING PQRECD,R1                                                        
         XC    SPOOLPAG,SPOOLPAG                                                
         XC    SPOOLLIN,SPOOLLIN                                                
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*              ROUTINE TO INITIALIZE PRINT DSECT                                
         SPACE 3                                                                
SPINIT   NTR1                                                                   
         MVC   LINE-8(8),=C'*CONTROL'        SEED                               
         MVC   HEAD1-8(8),=C'**HEAD**'                                          
         MVC   MID1-8(8),=C'**MIDS**'                                           
         MVC   P1-8(8),=C'**P1-4**'                                             
         MVI   LINE,99             PRESET VALUES                                
         MVI   ALLOWLIN,0                                                       
         MVI   MAXLINES,58                                                      
         LTR   RA,RA               MAXLINES MAY BE SET EXTERNALLY               
         BZ    SPIN2                                                            
         OI    BOXDDCTL,BOXDDREQ+BOXDDLC                                        
         CLI   BOXMAXL,0                                                        
         BE    SPIN2                                                            
         MVC   MAXLINES,BOXMAXL                                                 
         SPACE 1                                                                
SPIN2    MVI   SPACING,1                                                        
         MVI   CLEARHED,C'Y'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   FORCEMID,C'N'                                                    
         MVI   FORCEFUT,C'N'                                                    
         MVI   FORCECLR,C'N'                                                    
         MVI   SKIPSPEC,C'N'                                                    
         MVI   RCSUBPRG,0                                                       
         XC    HEADHOOK,HEADHOOK                                                
         XC    FOOTHOOK,FOOTHOOK                                                
         MVC   PAGE,=H'1'                                                       
         MVC   SUBPAGE,=H'1'                                                    
         XC    ACTPAGES,ACTPAGES                                                
         XC    MAXPAGES,MAXPAGES                                                
         XC    MIDHOOK,MIDHOOK                                                  
         LA    R2,H1                                                            
         LA    R3,14                                                            
         BAS   RE,CLEAR132                                                      
         MVC   MID1,H1                                                          
         MVC   MID2,H1                                                          
         LA    R2,P1                                                            
         LA    R3,4                                                             
         BAS   RE,CLEAR132                                                      
         MVC   SPACES,MYSPACES                                                  
         SR    RE,RE                                                            
         IC    RE,USERLANG                                                      
         SLL   RE,2                                                             
         L     RF,MTHIDX(RE)                                                    
         A     RF,RELO                                                          
         MVC   MONTHS,0(RF)                                                     
         L     RF,DAYIDX(RE)                                                    
         A     RF,RELO                                                          
         MVC   DAYTABL,0(RF)                                                    
         SPACE 1                                                                
         LA    R2,SPOOLKEY         INITIALIZE REMOTE PRINT ENTRY                
         USING PQPLD,R2                                                         
         SPACE 1                                                                
*NOP*    MVC   PLDESC+8(2),SPACES  CLEAN UP KEY                                 
*NOP*    TM    SPOOLIND,SPUINIT    TEST ALLOW USER VALUE                        
*NOP*    BZ    *+12                                                             
*NOP*    CLI   PLDESC+10,0         TEST USER COPIES                             
*NOP*    BNE   *+8                                                              
*NOP*    MVI   PLDESC+10,C'1'      SET NUMBER OF COPIES                         
         SPACE 1                                                                
         TM    SPOOLIND,SPUINIT    TEST ALLOW USER VALUE                        
         BZ    *+12                                                             
         CLI   PLLPP,0             TEST USER LPP                                
         BNE   *+8                                                              
         MVI   PLLPP,68            LINES PER PAGE                               
         SPACE 1                                                                
*&&US                                                                           
         TM    SPOOLIND,SPUINIT    TEST ALLOW USER VALUE                        
         BZ    *+12                                                             
         CLI   PLCLASS,0           TEST USER SET CLASS                          
         BNE   SPIN4                                                            
         MVI   PLCLASS,C'Z'        CLASS                                        
         CLC   PLSUBID,=C'AVA'     SPECIAL CLASS FOR AVAILS                     
         BNE   *+8                                                              
         MVI   PLCLASS,C'A'                                                     
         CLC   PLSUBID,=C'PRP'     PROPOSALS                                    
         BNE   *+8                                                              
         MVI   PLCLASS,C'A'                                                     
         CLC   PLDESC(6),=C'MAILER'    AND MAILERS                              
         BNE   *+8                                                              
         MVI   PLCLASS,C'A'                                                     
*&&                                                                             
         SPACE 1                                                                
* TEST IF USER HAS PASSED AN AREA TO BUILD NEW STYLE KEY *                      
* IF SO, PRESET VALUES ARE PRESERVED                                            
         SPACE 1                                                                
SPIN4    TM    SPOOLIND,SPUINIT    TEST USER INITIALIZED FIELDS                 
         BO    *+10                                                             
         XC    SPOOLQLK,SPOOLQLK   IF NOT, SPOOLQLK IS IGNORED                  
         CLI   SPOOLQLK,0          IF FIRST BYTE NON-ZERO, ITS                  
         BE    *+10                PROBABLY NOT A VALID ADDRESS,                
         XC    SPOOLQLK,SPOOLQLK   SO GET RID OF IT                             
         ICM   RF,15,SPOOLQLK      TEST NEW STYLE KEY PROVIDED                  
         BNZ   *+14                                                             
         LA    RF,SPLINE           ELSE USE LOCAL STORAGE                       
         XC    SPLINE,SPLINE                                                    
         SPACE 1                                                                
         LR    RE,R2               RE=A(OLD PQ PL... DATA)                      
         LR    R2,RF                                                            
         MVI   QLEXTRA,X'FF'       INDICATE NEW STYLE LIST                      
         OI    QLFLAG,QLFLDD       SET DICTIONARY SUPPORT REQUIRED              
         SPACE 1                                                                
         MVC   QLSRCID,PLUSER-PLOLD(RE)                                         
         MVC   QLSUBID,PLSUBID-PLOLD(RE)                                        
         CLI   QLCLASS,0                                                        
         BNE   *+10                                                             
         MVC   QLCLASS,PLCLASS-PLOLD(RE)                                        
         CLI   QLSTAT,0                                                         
         BNE   *+10                                                             
         MVC   QLSTAT,PLSTAT-PLOLD(RE)                                          
         CLI   QLLPP,0                                                          
         BNE   *+10                                                             
         MVC   QLLPP,PLLPP-PLOLD(RE)                                            
         OC    QLDESC,QLDESC                                                    
         BNZ   *+10                                                             
         MVC   QLDESC,PLDESC-PLOLD(RE)                                          
         SPACE 1                                                                
SPIN5    OC    QLMAKER(3),QLMAKER  SET SYS/PRG IF NOT DEFINED                   
         BNZ   SPIN6                                                            
         CLI   RCPROG,C' '         FORGET IT IF WE DONT KNOW SYSTEM             
         BNH   SPIN7                                                            
         MVC   QLSYS,RCPROG        SET SYSTEM FROM 1ST CHR OF SYSTEM ID         
         CLC   RCPROG(2),=C'MP'    ADJUST IF 1ST CHR NOT THE SAME               
         BNE   *+8                                                              
         MVI   QLSYS,C'L'                                                       
         CLC   RCPROG(2),=C'MB'                                                 
         BNE   *+8                                                              
         MVI   QLSYS,C'B'                                                       
         CLC   RCPROG(2),=C'PE'                                                 
         BNE   *+8                                                              
         MVI   QLSYS,C'E'                                                       
         MVC   QLPRG,RCPROG+2                                                   
         CLI   QLPRG,C' '                                                       
         BH    SPIN6                                                            
         MVC   QLPRG,=C'XX'        SET DEFAULT UNKNOWN PROGRAM ID               
         B     SPIN7                                                            
         SPACE 1                                                                
SPIN6    OC    QLMAKER+3(2),QLMAKER+3                                           
         BNZ   SPIN7                                                            
         ICM   RF,15,RCCOMFAC                                                   
         BZ    SPIN7                                                            
         ST    RF,SPPARA+8                                                      
         ICM   RF,15,CPQPROF-COMFACSD(RF)                                       
         BZ    SPIN7                                                            
         ST    R2,SPPARA+4                                                      
         MVI   SPPARA+4,1          PASS PQLD FORMAT                             
         LA    R1,SPPARA+16                                                     
         ST    R1,SPPARA                                                        
         XC    0(8,R1),0(R1)       SET PROFILE KEY SPPUU                        
         MVC   0(3,R1),QLMAKER                                                  
         MVC   3(2,R1),QLSRCID                                                  
         OC    QLSRCID,QLSRCID                                                  
         BZ    SPIN7                                                            
         LA    R1,SPPARA                                                        
         BASR  RE,RF                                                            
         SPACE 1                                                                
SPIN7    OC    QLDESC(8),QLDESC    TEST IF DESCRIPTION STILL UNKNOWN            
         BNZ   SPIN8                                                            
         MVC   QLDESC,SPACES                                                    
         MVC   QLDESC(3),QLMAKER                                                
         SPACE 1                                                                
SPIN8    OC    QLRETNL,QLRETNL     TEST IF RETAIN INFO STILL UNKNOWN            
         BNZ   SPIN10                                                           
         MVC   QLRETNL,=H'12'      SET ON-LINE LIVE/DEAD DEFAULT                
         MVC   QLRETND,=H'03'                                                   
         OC    VPRINT,VPRINT                                                    
         BZ    *+16                                                             
         MVC   QLRETNL,=H'48'      SET OFF-LINE LIVE/DEAD DEFAULT               
         MVC   QLRETND,=H'18'                                                   
         SPACE 1                                                                
SPIN10   OC    VPRINT,VPRINT                                                    
         BZ    SPIN10A                                                          
         CLI   SPMODE,X'80'                                                     
         BNE   *+8                                                              
         SPACE 1                                                                
SPIN10A  BAS   RE,SPUT4                                                         
         XC    SPOOLPAG,SPOOLPAG                                                
         XC    SPOOLLIN,SPOOLLIN                                                
         ICM   RF,15,SPOOLQLK      TEST USING CALLERS EXTENDED KEY              
         BNZ   *+8                                                              
         LA    RF,SPLINE           ELSE LOCAL EXTEDNED KEY                      
         LA    R2,SPOOLKEY                                                      
         MVC   19(2,R2),19(RF)     SET REPORT NUMBER IN OLD KEY                 
SPINX    B     XIT                                                              
         SPACE 1                                                                
CLEAR132 MVC   0(132,R2),MYSPACES                                               
         LA    R2,132(R2)                                                       
         BCT   R3,CLEAR132                                                      
         BR    RE                                                               
         SPACE 1                                                                
SPCLEAR  L     R1,EXWIDE                                                        
SPCLEAR2 EX    R1,MVCR2SP                                                       
         A     R2,PWIDTH                                                        
         BCT   R3,SPCLEAR2                                                      
         BR    RE                                                               
         SPACE 1                                                                
MVCR2SP  MVC   0(0,R2),MYSPACES    EXECUTABLE INSTRUCTIONS                      
CLCR2SP  CLC   0(0,R2),MYSPACES                                                 
CLCR4SP  CLC   0(0,R4),MYSPACES                                                 
         EJECT                                                                  
*                   OPTION JUST TO CLEAR ALL PRINT LINES                        
         SPACE 3                                                                
SPOOL4   CLI   FORCECLR,C'N'                                                    
         BE    PC4                                                              
         MVI   FORCECLR,C'N'                                                    
         SPACE 1                                                                
PC1      L     R2,AH1                                                           
         LA    R3,14                                                            
         BAS   RE,SPCLEAR                                                       
         L     R2,AM1                                                           
         LA    R3,2                                                             
         BAS   RE,SPCLEAR                                                       
         SPACE 1                                                                
PC3      L     R2,AP1                                                           
         LA    R3,4                                                             
         BAS   RE,SPCLEAR                                                       
         B     PCEXT                                                            
         EJECT                                                                  
*                   CONTROL SPEC PROCESSING                                     
         SPACE 1                                                                
PC4      CLI   SKIPSPEC,C'Y'                                                    
         BE    PC6                                                              
         CLI   FORCEFUT,C'Y'       OPTION JUST TO DO FOOTLINES                  
         BNE   PC4A                                                             
         CLI   FOOTLNS,0                                                        
         BE    *+8                                                              
         BAS   RE,CONFEET                                                       
         MVI   FORCEFUT,C'N'                                                    
         B     PCEXT                                                            
         SPACE 1                                                                
PC4A     SR    RF,RF               CHECK IF LINE + ALLOWLIN EXCEEDS MAX         
         IC    RF,LINE                                                          
         SR    R0,R0                                                            
         IC    R0,ALLOWLIN                                                      
         MVI   ALLOWLIN,0                                                       
         AR    RF,R0                                                            
         IC    R0,FOOTLNS          MAY NEED ROOM FOR FOOTLINES                  
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)            (SPACE BEFORE)                               
         AR    RF,R0                                                            
         IC    R0,MAXLINES                                                      
         CR    RF,R0                                                            
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'       SO ENSURE HEADLINES                          
         BAS   RE,MIDHEAD          MAY BE MID HEADS TO PRINT                    
*                                  THIS SETS FORCEHED TO Y OR N                 
         CLI   FORCEHED,C'Y'       DONT EXECUTE SPECS IF JUST                   
         BE    PC5                 HANDLING PLINES                              
         CLI   FORCEMID,C'Y'                                                    
         BE    PC5                                                              
         SPACE 1                                                                
PC4B     L     R2,AP4              HOW MANY PRINT LINES ARE THERE               
         LA    R3,4                                                             
         L     R1,EXWIDE                                                        
         SPACE 1                                                                
PC4D     EX    R1,CLCR2SP                                                       
         BNE   PC4F                                                             
         S     R2,PWIDTH                                                        
         BCT   R3,PC4D                                                          
         LA    R3,1                                                             
         SPACE 1                                                                
PC4F     LLC   R2,SPACING          ADD IN SPACING                               
         AR    R3,R2                                                            
         BCTR  R3,0                -1                                           
         LLC   R2,LINE             AND PRESENT LINE COUNT                       
         AR    R3,R2                                                            
         IC    R2,FOOTLNS          AND FOOTLINES                                
         LTR   R2,R2                                                            
         BZ    *+8                                                              
         LA    R2,1(R2)            (+1 IF PRESENT)                              
         AR    R3,R2                                                            
         LLC   R2,MAXLINES         WILL THEY FIT                                
         CR    R3,R2                                                            
         BNH   PC14                                                             
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
PC5      BAS   RE,PROCSPEC                                                      
         EJECT                                                                  
*                   CONTROL OF HEADLINE PRINTING                                
         SPACE 3                                                                
PC6      CLI   FORCEHED,C'N'                                                    
         BE    PC12                                                             
         MVI   SKPLINES,0                                                       
         LTR   RA,RA               TEST BOXAREA RESOLVED                        
         BZ    *+10                                                             
         MVC   SKPLINES,BOXHDSKP                                                
         MVI   FORCEHED,C'N'                                                    
         SPACE 1                                                                
PC10     MVI   FORCEMID,C'Y'                                                    
         CLI   FOOTLNS,0                                                        
         BE    PC10A                                                            
         CLI   FORCEFUT,C'S'       TEST SKIPPING FOOTLINES THIS TIME            
         BNE   *+12                                                             
         MVI   FORCEFUT,C'N'                                                    
         B     PC10A                                                            
         BAS   RE,CONFEET                                                       
PC10A    GOTO1 PRINT,REPARA,MYSPACES,=C'BC01'                                   
         LH    R1,ACTPAGES                                                      
         LA    R1,1(R1)                                                         
         STH   R1,ACTPAGES                                                      
         OC    MAXPAGES,MAXPAGES                                                
         BZ    PC10AA                                                           
         CLC   ACTPAGES,MAXPAGES                                                
         BNH   PC10AA                                                           
         DC    H'0'                                                             
PC10AA   MVI   LINE,0                                                           
         TM    SPOOLIND,SPFRMDF    TEST FORMDEF=Y SET (AFP)                     
         BZ    PC11AA                                                           
         IC    R0,BOXOFF           SAVE CURRENT BOXOFF VALUE                    
         MVI   BOXOFF,C'Y'         ENSURE NO BOXES PRINT                        
         GOTO1 PRINT,REPARA,MYSPACES,=C'BL08'  SKIP 10 LINES                    
         MVI   LINE,10                                                          
         STC   R0,BOXOFF           REINSTATE BOXOFF VALUE                       
         SPACE 1                                                                
PC11AA   DS    0H                                                               
         CLI   SKPLINES,0                                                       
         BE    PC11AX                                                           
         SR    R0,R0                                                            
         IC    R0,SKPLINES                                                      
PC11AB   DS    0H                                                               
         GOTO1 PRINT,REPARA,MYSPACES,=C'BL01'                                   
         BCT   R0,PC11AB                                                        
*                                                                               
PC11AX   OC    HEADHOOK,HEADHOOK   USER SUPPLIED HEADLINE HOOK                  
         BZ    PC11A                                                            
         L     RF,HEADHOOK                                                      
         OI    SPOOLIND,SPHHOOK    LET USER KNOW TYPE OF HOOK                   
         BAS   RE,GOHOOK                                                        
         NI    SPOOLIND,255-SPHHOOK                                             
         SPACE 1                                                                
PC11A    SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,SKPLINES                                                      
         IC    RF,LINE                                                          
         LA    RE,1(RF)                                                         
         STC   RE,LINE                                                          
*                                                                               
         CLI   HEADHOOK,C'A'        PRINT HEADS FROM SPECIAL IO AREA?           
         BNE   PC11B                NO                                          
         L     R2,HEADHOOK          A(HEADHOOK) HAS A(I/O AREA)!                
         ICM   RE,15,0(R2)          A(HEADHOOK) IS FIRST 4 BYTES                
         ST    RE,HEADHOOK          RESTORED!                                   
         MVI   HEADHOOK,0           CLEAR HOB!                                  
         LLC   R3,5(R2)             NUMBER OF HEADLINES TO PRINT                
         AHI   R2,6                 A(HEADLINES START HERE)                     
         B     PC11C                                                            
*                                                                               
PC11B    L     R2,AH1                                                           
         LA    R3,14                                                            
PC11C    BAS   RE,PRINCON                                                       
*                                                                               
         CLI   HEADHOOK,C'R'       TEST USER REQUESTED RETURN                   
         BE    PC11AX                                                           
         SR    R4,R4                                                            
         MVC   REPDUB(4),PAGE      ADD 1 TO PAGE AND SUBPAGE                    
         ICM   R4,3,REPDUB         PAGE NUMBER                                  
         LA    R4,1(R4)            INCREMENT                                    
         STCM  R4,3,REPDUB                                                      
         SR    R4,R4                                                            
         ICM   R4,3,REPDUB+2       SUBPAGE NUMBER                               
         LA    R4,1(R4)            PAGE NUMBER                                  
         STCM  R4,3,REPDUB+2                                                    
         MVC   PAGE(4),REPDUB                                                   
         TM    SPOOLIND,SPNGAPS    NO GAP BETWEEN HEAD AND MID                  
         BO    *+12                                                             
         TM    SPOOLIND,SPNSPACE   MULTI HEADLINE OPTION                        
         BO    PC1                                                              
         NI    SPOOLIND,X'FF'-SPNGAPS                                           
         EJECT                                                                  
*                   CONTROL OF MIDLINE PRINTING                                 
         SPACE 3                                                                
PC12     CLI   FORCEMID,C'N'                                                    
         BE    PC14                                                             
         MVI   FORCEMID,C'N'                                                    
         OC    MIDHOOK,MIDHOOK     USER SUPPLIED MIDLINE HOOK                   
         BZ    PC12B                                                            
*                                                                               
PC12A    L     RF,MIDHOOK                                                       
         OI    SPOOLIND,SPMHOOK    LET USER KNOW TYPE OF HOOK                   
         BAS   RE,GOHOOK                                                        
         NI    SPOOLIND,255-SPMHOOK                                             
         SPACE 1                                                                
PC12B    L     R2,AM1                                                           
         LA    R3,2                                                             
         BAS   RE,PRINCON                                                       
         CLC   LINE,MAXLINES                                                    
         BNL   PC10                                                             
         CLI   MIDHOOK,C'R'        TEST USER REQUESTED RETURN                   
         BE    PC12A                                                            
         SPACE 1                                                                
*                   CONTROL OF PRINT LINES                                      
         SPACE 3                                                                
PC14     L     R2,AP1                                                           
         LA    R3,4                                                             
         BAS   RE,PRINP                                                         
         MVI   SPACING,1                                                        
         SPACE 1                                                                
         CLI   CLEARHED,C'N'       GO BACK TO CLEAR JUST THE PRINT              
         BE    PC3                 LINES OR, OPTIONALLY, ALL LINES              
         B     PC1                                                              
         EJECT                                                                  
*              CONTROL MID HEADS                                                
         SPACE 1                                                                
MIDHEAD  NTR1                                                                   
         CLI   FORCEHED,C'M'                                                    
         BNE   XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         LLC   R1,LINE                                                          
         LA    R1,10(R1)           NOT WORTH IT UNLESS 10 LINES LEFT            
         LLC   R0,MAXLINES                                                      
         CR    R1,R0                                                            
         BH    XIT                                                              
         SPACE 1                                                                
         OC    HEADHOOK,HEADHOOK   MUST HAVE A HEAD HOOK                        
         BZ    XIT                                                              
         MVI   BOXREQ,C'C'         CLOSE PRESENT BOX                            
         GOTO1 PRINT,REPARA,MYSPACES,=C'BL02'                                   
         AI    LINE,2                                                           
         BAS   RE,PROCSPEC                                                      
         L     RF,HEADHOOK                                                      
         OI    SPOOLIND,SPHHOOK    LET USER KNOW TYPE OF HOOK                   
         BAS   RE,GOHOOK                                                        
         NI    SPOOLIND,255-SPHHOOK                                             
         SPACE 1                                                                
         L     R2,AH1              NOW HAVE ALL THE HEADLINES                   
         LA    R3,14               ONLY GOING TO PRINT THE HEADINGS             
         LA    R4,BOXROWS                                                       
         SPACE 1                                                                
MIDH2    CLI   0(R4),C'T'          LOOKING FOR THE TOP OF THE BOX               
         BE    MIDH4                                                            
         A     R2,PWIDTH                                                        
         LA    R4,1(R4)                                                         
         BCT   R3,MIDH2                                                         
         B     XIT                 NONE FOUND - OUT OF LUCK                     
         SPACE 1                                                                
MIDH4    LLC   R1,LINE             SHUFFLE THE T (AND M)                        
         LA    R1,BOXROWS-1(R1)    DOWN TO THE PRESENT POS IN BOXROWS           
         MVC   DUB,0(R4)                                                        
         MVC   0(8,R4),SPACES                                                   
         MVC   0(8,R1),DUB                                                      
         MVI   BOXINIT,0                                                        
         BAS   RE,PRINCON          PRINT THE REST OF THE HEADLINES              
         MVI   FORCEHED,C'N'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL FOOT LINES                                               
         SPACE 1                                                                
CONFEET  NTR1                                                                   
         MVC   SAVEP,P             SAVE FIRST PRINT LINE                        
         MVC   P,MYSPACES                                                       
         LLC   R1,MAXLINES         FIGURE OUT HOW MANY LINES TO                 
         LLC   R0,FOOTLNS          ADVANCE PRINTER TO REACH FIRST LINE          
         SR    R1,R0                                                            
         IC    R0,LINE                                                          
         SR    R1,R0                                                            
         BNP   CONF6                                                            
         EDIT  (R1),(4,CONTROL),FILL=0                                          
         MVC   CONTROL(2),=C'BL'                                                
         GOTO1 PRINT,REPARA,P,CONTROL                                           
         MVC   CONTROL+2(2),=C'01'                                              
         OI    SPOOLIND,SPFHOOK    LET USER KNOW TYPE OF HOOK                   
         SPACE 1                                                                
CONF4    L     RF,FOOTHOOK         ASK USER TO GIVE ME DATA                     
         BAS   RE,GOHOOK                                                        
         CLC   P,MYSPACES          NO MORE                                      
         BE    CONF6                                                            
         GOTO1 PRINT,REPARA,P,CONTROL                                           
         B     CONF4                                                            
         SPACE 1                                                                
CONF6    MVC   P,SAVEP             SO RESTORE                                   
         NI    SPOOLIND,255-SPFHOOK                                             
         B     XIT                                                              
         EJECT                                                                  
*                   CONTROL PRINTING OF A BLOCK                                 
         SPACE 3                                                                
PRINCON  NTR1                                                                   
         MVC   REPWORK,MYSPACES                                                 
         LR    R4,R2                                                            
         LR    R5,R3                                                            
         LA    R6,REPWORK                                                       
         L     R1,EXWIDE                                                        
         SPACE 1                                                                
PC20     EX    R1,CLCR4SP          BUILD A TABLE OF PRINT LINE BYTES            
         BE    PC21                                                             
         MVI   0(R6),C'P'                                                       
         SPACE 1                                                                
PC21     A     R4,PWIDTH                                                        
         LA    R6,1(R6)                                                         
         BCT   R5,PC20                                                          
         SPACE 1                                                                
*                                  NOW CONVERT TO PRINTING NUMBERS              
         LA    R6,REPWORK                                                       
         SPACE 1                                                                
PC22     CLC   0(16,R6),MYSPACES   ANY LEFT                                     
         BE    PC30                NO                                           
         SPACE 1                                                                
PC26     LA    R1,2                                                             
         TM    SPOOLIND,SPNSPACE                                                
         BNO   *+8                                                              
         LA    R1,1                                                             
         CLC   1(15,R6),MYSPACES   IS THIS THE LAST                             
         BE    PC28                                                             
         LA    R1,2                                                             
         CLC   1(2,R6),=C' P'                                                   
         BE    PC28                                                             
         LA    R1,1                                                             
         CLI   1(R6),C'P'                                                       
         BE    PC28                                                             
         LA    R1,3                                                             
         SPACE 1                                                                
PC28     STC   R1,0(R6)                                                         
         AR    R6,R1                                                            
         SR    R7,R7                                                            
         IC    R7,LINE                                                          
         AR    R7,R1                                                            
         STC   R7,LINE                                                          
         B     PC22                                                             
         SPACE 1                                                                
PC30     LA    R6,REPWORK          NOW PRINT R2 = DATA                          
*                                            R3 = N'LINES                       
*                                            R6 = SKIP NUMBERS                  
PC32     CLI   0(R6),C' '                                                       
         BE    PC34                                                             
         MVC   CONTROL(3),=C'BL0'                                               
         LLC   RF,0(R6)                                                         
         CVD   RF,REPDUB                                                        
         UNPK  CONTROL+2(2),REPDUB                                              
         OI    CONTROL+3,X'F0'                                                  
         GOTO1 PRINT,REPARA,(R2),CONTROL                                        
         SPACE 1                                                                
PC34     A     R2,PWIDTH                                                        
         LA    R6,1(R6)                                                         
         BCT   R3,PC32                                                          
         B     XIT                                                              
         EJECT                                                                  
*                   SPECIAL ROUTINE FOR PRINT LINES                             
         SPACE 1                                                                
PRINP    NTR1                                                                   
         BCTR  R3,R0                                                            
         SPACE 1                                                                
PRINP2   CLI   WIDEOPT,C'Y'        SINGLE PRINT UNTIL LAST                      
         BE    PRINP4                                                           
         CLC   132(132,R2),MYSPACES                                             
         BE    PC36                                                             
         B     PRINP6                                                           
         SPACE 1                                                                
PRINP4   CLC   198(198,R2),MYSPACES                                             
         BE    PC36                                                             
         SPACE 1                                                                
PRINP6   GOTO1 PRINT,REPARA,(R2),=C'BL01'                                       
         SR    R1,R1                                                            
         IC    R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,LINE                                                          
         A     R2,PWIDTH                                                        
         BCT   R3,PRINP2                                                        
         SPACE 1                                                                
PC36     MVC   CONTROL(3),=C'BL0'  USERS SPACING FOR ONE OR 2/2                 
         LLC   RF,SPACING                                                       
         CVD   RF,REPDUB                                                        
         UNPK  CONTROL+2(2),REPDUB                                              
         OI    CONTROL+3,X'F0'                                                  
         MVC   REPDUB(1),SPACING                                                
         GOTO1 PRINT,REPARA,(R2),CONTROL                                        
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,REPDUB                                                        
         IC    R0,LINE                                                          
         AR    R1,R0                                                            
         STC   R1,LINE                                                          
         B     XIT                                                              
         SPACE 1                                                                
PCEXT    MVI   SKIPSPEC,C'N'                                                    
         XMOD1 1                                                                
         EJECT                                                                  
*                   EXECUTE THE SPEC POOL                                       
         SPACE 3                                                                
PROCSPEC NTR1                                                                   
         ICM   R2,15,SPECS         POINT TO SPEC POOL                           
         BZ    XIT                                                              
         ICM   RF,15,RCCOMFAC      MUST RESOLVE DICTATE FOR SPECS               
         BZ    XIT                                                              
         ICM   RF,15,CDICTATE-COMFACSD(RF)                                      
         BZ    XIT                                                              
*&&UK*&& GOTO1 (RF),SPPARA,C'LL  ',DCLPMC,LPMCASE                               
*&&US*&& GOTO1 (RF),SPPARA,C'LU  ',DCLPMC,LPMCASE                               
         SPACE 1                                                                
         SR    R3,R3                                                            
         MVI   PROGSW,C'Y'         PRESET SWITCHES                              
         MVI   FILTSW,C'Y'                                                      
         CLI   RCSUBPRG,0                                                       
         BE    EX4                                                              
         MVI   PROGSW,C'N'                                                      
         B     EX4                                                              
         SPACE 2                                                                
EX2      IC    R3,1(R2)                                                         
         AR    R2,R3               GET NEXT SPEC                                
         SPACE 1                                                                
EX4      SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         SLL   R4,2                                                             
         L     R4,INBRTAB(R4)      GO TO ROUTINE DEPENDENT                      
         A     R4,RELO                                                          
         BR    R4                  ON THE VALUE OF ELEMENT                      
         SPACE 1                                                                
EX6      B     XIT                                                              
         SPACE 1                                                                
EX8      MVI   PROGSW,C'N'                                                      
         MVI   FILTSW,C'Y'         RESET FOR SUB-PROGRAM FILTERING              
         B     EX2                                                              
         SPACE 1                                                                
EX10     CLC   RCSUBPRG(1),2(R2)   CHECK MATCH OF SUB-PROGRAM                   
         BNE   EX2                                                              
         MVI   PROGSW,C'Y'                                                      
         B     EX2                                                              
         SPACE 1                                                                
EX12     CLC   RCSUBPRG(1),2(R2)   SPROG N THRU M FEATURE                       
         BL    EX2                                                              
         CLC   RCSUBPRG(1),3(R2)                                                
         BH    EX2                                                              
         MVI   PROGSW,C'Y'                                                      
         B     EX2                                                              
         EJECT                                                                  
*                   PRINT SPEC CONTROL AND LITERALS                             
         SPACE 1                                                                
EX46     CLI   PROGSW,C'Y'                                                      
         BNE   EX2                                                              
         CLI   FILTSW,C'Y'                                                      
         BNE   EX2                                                              
         MVC   REPDUB(2),2(R2)     WORK OUT DISLACEMENT                         
         LH    R5,REPDUB                                                        
         A     R5,AH1                                                           
         CLI   0(R2),13                                                         
         BE    EX48                                                             
         SPACE 1                                                                
         LA    R4,PRBRTAB          R4=A(SPEC BRANCH TABLE)                      
         LA    R0,L'PRBRTAB        R0=BXLE INCREMENT                            
         LA    R1,PRBRTABX-1       R1=BXLE LIMIT                                
         SPACE 1                                                                
         CLC   4(1,R2),0(R4)       TEST SPEC NUMBER VS. TABLE                   
         BE    *+12                                                             
         BXLE  R4,R0,*-10                                                       
         B     EX2                 CANNOT FIND SPEC                             
         SPACE 1                                                                
         SR    RF,RF                                                            
         ICM   RF,7,1(R4)                                                       
         A     RF,RELO                                                          
         BR    RF                                                               
         SPACE 1                                                                
EX48     SR    R4,R4               HANDLE LITERALS                              
         IC    R4,1(R2)                                                         
         SHI   R4,5                LENGTH IS L'ELEMENT - 4                      
         EX    R4,MUVLIT                                                        
         B     EX2                                                              
         SPACE 1                                                                
MUVLIT   MVC   0(0,R5),4(R2)                                                    
         EJECT                                                                  
*                   TABLE OF INTERNAL BRANCH VALUES                             
         SPACE 3                                                                
         DS    0F                                                               
INBRTAB  DC    AL4(EX6)     0      END OF SPECS                                 
         DC    AL4(EX8)     1      PROGRAM RESET                                
         DC    AL4(EX10)    2              FILTER                               
         DC    AL4(EX2)     3                                                   
         DC    AL4(EX2)     4                                                   
         DC    AL4(EX2)     5                                                   
         DC    AL4(EX2)     6                                                   
         DC    AL4(EX2)     7                                                   
         DC    AL4(EX12)    8      THRU SPEC                                    
         DC    AL4(EX2)                                                         
         DC    AL4(EX2)                                                         
         DC    AL4(EX2)                                                         
         DC    AL4(EX46)   12      PRINT KEYWORD SPEC                           
         DC    AL4(EX46)   13      PRINT LITERAL SPEC                           
         EJECT                                                                  
*                   ROUTINES FOR PRINT SPECS (0-3)                              
         SPACE 1                                                                
EX50     MVC   0(110,R5),MYSPACES  SPACES                                       
         CLI   SKIPSPEC,C'T'                                                    
         BE    EX2                                                              
         L     R1,EXWIDE                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),MYSPACES                                                 
         B     EX2                                                              
         SPACE 1                                                                
EX52     MVC   0(RUNMSGLQ,R5),MYSPACES                                          
       ++INCLUDE DDGETIME                                                       
         ST    R1,REPWORK                                                       
         XC    REPDUB,REPDUB                                                    
         MVC   REPDUB+5(3),REPWORK                                              
         OI    REPDUB+7,X'0F'                                                   
         CVB   R6,REPDUB           R6=TIME                                      
*                                                                               
         CLC   RCPROG(2),=C'AC'                                                 
         BNE   EX52A                                                            
         CLC   RCPROG(4),=C'ACRW'                                               
         BE    *+14                                                             
         CLC   RCPROG(4),=C'ACEW'                                               
         BNE   EX52B                                                            
*                                                                               
EX52A    MVC   0(#RUNONLQ,R5),GE@RUNON       'RUN ON'                           
         LA    R0,#RUNONLQ                                                      
         BAS   RE,FINDEND                                                       
         LA    R5,1(R5)                                                         
*&&UK*&& GOTO1 DATCON,REPARA,(X'44',RCDATE),(8,0(R5))                           
*&&US*&& GOTO1 DATCON,REPARA,(X'04',RCDATE),(8,0(R5))                           
         LA    R0,MAXDATLQ                                                      
         BAS   RE,FINDEND                                                       
         LA    R5,1(R5)                                                         
         MVC   0(#ATLQ,R5),GE@AT             'AT'                               
         LA    R0,#ATLQ                                                         
         BAS   RE,FINDEND                                                       
         EDIT  (R6),(5,1(R5)),2                                                 
         B     EX2                                                              
*                                                                               
EX52B    MVC   0(#RUNDTLQ,R5),GE@RUNDT       'RUN DATE'                         
         ST    R5,REPSAVE          SAVE A(FIRST PRINT POSITION)                 
         LA    R0,#RUNDTLQ                                                      
         BAS   RE,FINDEND                                                       
         LA    R5,1(R5)                                                         
*&&UK*&& GOTO1 DATCON,REPARA,(X'44',RCDATE),(8,0(R5))                           
*&&US*&& GOTO1 DATCON,REPARA,(X'04',RCDATE),(8,0(R5))                           
         L     R5,REPSAVE          POINT TO NEXT LINE                           
         A     R5,PWIDTH                                                        
         MVC   0(#RUNTMLQ,R5),GE@RUNTM       'RUN TIME'                         
         LA    R0,#RUNTMLQ                                                      
         BAS   RE,FINDEND                                                       
         EDIT  (R6),(5,1(R5)),2,FILL=0                                          
         B     EX2                                                              
         SPACE 1                                                                
EX54     MVC   0(#REPLLQ+1,R5),MYSPACES                                         
         MVC   0(#REPLLQ,R5),GE@REP          'REPORT'                           
         LA    R0,#REPLLQ                                                       
         BAS   RE,FINDEND                                                       
         MVC   1(4,R5),RCPROG                                                   
         B     EX2                                                              
         SPACE 1                                                                
EX56     MVC   0(#PAGELQ+1+4,R5),MYSPACES                                       
         MVC   0(#PAGELQ,R5),GE@PAGE         'PAGE'                             
         LA    R0,#PAGELQ                                                       
         BAS   RE,FINDEND                                                       
         SR    R0,R0                                                            
         ICM   R0,3,PAGE                                                        
         EDIT  (R0),(5,1(R5)),ALIGN=LEFT                                        
         B     EX2                                                              
         SPACE 1                                                                
EX57     MVC   0(#SUBPGLQ+1+4,R5),MYSPACES                                      
         MVC   0(#SUBPGLQ,R5),GE@SUBPG       'SUBPAGE'                          
         LA    R0,#SUBPGLQ                                                      
         BAS   RE,FINDEND                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SUBPAGE                                                     
         EDIT  (R0),(5,1(R5)),ALIGN=LEFT                                        
         B     EX2                                                              
         SPACE 1                                                                
EX60     MVC   0(#PERLQ+1+MAXPERLQ,R5),MYSPACES                                 
         CLC   RCPROG(2),=C'AC'                                                 
         BNE   EX60A                                                            
         CLC   USERQSTR(6),=6X'F0'                                              
         BE    EX2                                                              
         B     EX61                                                             
*                                                                               
EX60A    MVC   0(#PERLQ,R5),GE@PER           'PERIOD'                           
         LA    R0,#PERLQ                                                        
         BAS   RE,FINDEND                                                       
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
EX61     DS    0H                                                               
*&&UK*&& GOTO1 DATCON,REPARA,(X'60',USERQSTR),(8,0(R5)),USERQEND                
*&&US*&& GOTO1 DATCON,REPARA,(X'20',USERQSTR),(8,0(R5)),USERQEND                
         B     EX2                                                              
         SPACE 1                                                                
EX64     MVC   0(#REQRLQ+1,R5),MYSPACES                                         
         MVC   0(#REQRLQ,R5),GE@REQR         'REQUESTOR'                        
         LA    R0,#REQRLQ                                                       
         BAS   RE,FINDEND                                                       
         MVC   1(3,R5),SPOOLID                                                  
         OC    SPOOLRPN,SPOOLRPN                                                
         BZ    EX2                                                              
         MVI   4(R5),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,5(R5)),ALIGN=LEFT                                        
         B     EX2                                                              
         SPACE 1                                                                
EX66     MVC   0(33,R5),USERNAME                                                
         B     EX2                                                              
         SPACE 1                                                                
EX68     MVC   0(33,R5),USERADDR                                                
         B     EX2                                                              
         SPACE 1                                                                
EX70     MVC   0(REPMSGLQ,R5),MYSPACES                                          
         MVC   0(#REPLLQ,R5),GE@REP          'REPORT'                           
         LA    R0,#REPLLQ                                                       
         BAS   RE,FINDEND                                                       
         MVC   1(4,R5),RCPROG                                                   
         LA    R5,6(R5)                                                         
         MVC   0(#ONLQ,R5),GE@ON             'ON'                               
         LA    R0,#ONLQ                                                         
         BAS   RE,FINDEND                                                       
*&&UK*&& GOTO1 DATCON,REPARA,(X'44',RCDATE),(8,1(R5))                           
*&&US*&& GOTO1 DATCON,REPARA,(X'04',RCDATE),(8,1(R5))                           
         LA    R0,MAXDATLQ                                                      
         BAS   RE,FINDEND                                                       
         MVI   1(R5),C'-'                                                       
       ++INCLUDE DDGETIME                                                       
         ST    R1,REPWORK                                                       
         XC    REPDUB,REPDUB                                                    
         MVC   REPDUB+5(3),REPWORK                                              
         OI    REPDUB+7,X'0F'                                                   
         CVB   R6,REPDUB                                                        
         EDIT  (R6),(5,3(R5)),2                                                 
         B     EX2                                                              
         SPACE 1                                                                
EX72     MVC   0(CREMSGLQ,R5),MYSPACES                                          
         MVC   0(#CREONLQ,R5),GE@CREON       'CREATED ON'                       
         LA    R0,#CREONLQ                                                      
         BAS   RE,FINDEND                                                       
         LA    R5,1(R5)                                                         
*&&UK*&& GOTO1 DATCON,REPARA,(X'44',RCDATE),(8,0(R5))                           
*&&US*&& GOTO1 DATCON,REPARA,(X'04',RCDATE),(8,0(R5))                           
         LA    R0,MAXDATLQ                                                      
         BAS   RE,FINDEND                                                       
         LA    R5,1(R5)                                                         
         MVC   0(#ATLQ,R5),GE@AT             'AT'                               
         LA    R0,MAXDATLQ                                                      
         BAS   RE,FINDEND                                                       
         TIME  DEC                                                              
         SRL   R0,16               SHIFT OFF SECONDS,TENTHS,HUNDREDTHS          
         SLL   R0,4                                                             
         XC    REPDUB,REPDUB                                                    
         STCM  R0,7,REPDUB+5                                                    
         OI    REPDUB+7,X'0F'                                                   
         CVB   R0,REPDUB                                                        
         EDIT  (R0),(5,1(R5)),2                                                 
         B     EX2                                                              
         SPACE 1                                                                
*                   BRANCH TABLE FOR PRINT SPECS                                
         SPACE 3                                                                
PRBRTAB  DS    0CL4                                                             
         DC    AL1(00),AL3(EX50)    0   SPACES                                  
         DC    AL1(01),AL3(EX52)    1   RUN                                     
         DC    AL1(02),AL3(EX54)    2   REPORT                                  
         DC    AL1(03),AL3(EX56)    3   PAGE                                    
         DC    AL1(04),AL3(EX60)    4   PERIOD                                  
         DC    AL1(05),AL3(EX64)    5   REQUESTOR                               
         DC    AL1(06),AL3(EX70)    6   NETREP                                  
         DC    AL1(07),AL3(EX66)    7   AGYNAM                                  
         DC    AL1(08),AL3(EX68)    8   AGYADD                                  
         DC    AL1(40),AL3(EX72)   40   CREATED                                 
PRBRTABX EQU   *                                                                
         EJECT                                                                  
* FIND THE LAST NON SPACE CHARCTER IN A TEXT SRING                              
* ENTRY R5=A(START OF TEXT)                                                     
*       R0=MAX LENGTH OF TEXT                                                   
* EXIT  R5=FIRST BLANK/NULL CHR TRAILING TEXT                                   
*                                                                               
FINDEND  AR    R5,R0                                                            
         BCTR  R5,0                                                             
         TM    0(R5),X'FF'-X'40'                                                
         BNZ   *+10                                                             
         BCTR  R5,0                                                             
         BCT   R0,*-10                                                          
         LA    R5,1(R5)                                                         
         BR    RE                                                               
         EJECT                                                                  
*              CONVERT LOGICAL TO PHYSICAL CONTROL                              
         SPACE 3                                                                
PRINT    NTR1                                                                   
         OC    VPRINT,VPRINT                                                    
         BZ    PRINTB                                                           
         L     R2,0(R1)            BACK UP TO PREVIOUS POSITION                 
         BCTR  R2,0                                                             
         ST    R2,0(R1)                                                         
         GOTO1 VPRINT                                                           
         B     XIT                                                              
         SPACE 1                                                                
PRINTB   LM    R2,R3,0(R1)         A(PRINT LINE)  A(CONTROL)                    
         MVC   SPLINE,0(R2)                                                     
         PACK  SPDUB,2(2,R3)       CONTROL IS (B) C/L NN                        
         CVB   R4,SPDUB                                                         
         BCTR  R4,0                                                             
         LA    R5,CHANTAB                                                       
         CLI   1(R3),C'C'          CHANNEL                                      
         BE    PRINT4                                                           
         LA    R5,LINTAB                                                        
         CHI   R4,2                IF MORE THAN 3 LINES,                        
         BNH   PRINT4                                                           
         MVI   SPCC,X'19'          WRITE BEFORE 3                               
         BAS   RE,SPUT                                                          
         LA    R5,SPATAB                                                        
         SHI   R4,3                                                             
         SPACE 1                                                                
PRINT2   CHI   R4,2                THEN CONTROL SPACING DOWN                    
         BNH   PRINT4                                                           
         MVI   SPCC,X'1B'                                                       
         BAS   RE,SPUT                                                          
         SHI   R4,3                                                             
         B     PRINT2                                                           
         SPACE 1                                                                
PRINT4   AR    R4,R5                                                            
         MVC   SPCC(1),0(R4)                                                    
         BAS   RE,SPUT                                                          
         B     XIT                                                              
         SPACE 1                                                                
CHANTAB  DC    X'899199A1A9B1B9C1C9'                                            
LINTAB   DC    X'091119'                                                        
SPATAB   DC    X'0B131B'                                                        
         EJECT                                                                  
*              CONTROL OF PUTS TO PRINT FILE AND CLOSE                          
SPUT     NTR1                                                                   
         SPACE 1                                                                
SPUT2    LA    R2,SPCC                                                          
         BAS   RE,SPUT4                                                         
         MVI   SPOOLKEY,1                                                       
         B     XIT                                                              
         SPACE 1                                                                
SPUT4    NTR1                                                                   
         GOTO1 SPOOLDM,SPPARA,=C'DMPRINT',=C'PRTQUE',0,(R2),SPOOLBUF            
         CLI   SPPARA+8,0          CK FOR ERROR                                 
         BE    XIT                                                              
         DC    H'0',C'$PQFULL'                                                  
         SPACE 1                                                                
SPEND    NTR1                                                                   
         CLI   SPOOLKEY,0                                                       
         BE    XIT                                                              
         B     SPENDF1                                                          
SPENDF   NTR1                                                                   
SPENDF1  MVI   SPOOLKEY,X'FF'                                                   
         LA    R2,SPOOLKEY                                                      
         BAS   RE,SPUT4                                                         
*                                                                               
         L     R4,SPOOLBUF                                                      
         USING PQRECD,R4                                                        
         MVC   SPOOLPAG,PQPAGES                                                 
         MVC   SPOOLLIN,PQLINES+1                                               
         TM    PQTYPE,PQTYNEW                                                   
         BO    XIT                                                              
         MVC   SPOOLPAG,PQPAGES                                                 
         MVC   SPOOLLIN,PQLINES+1                                               
         DROP  R4                                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
GOHOOK   NTR1                                                                   
         L     RE,USERRD           USERS RD                                     
         LM    R0,RC,20(RE)        RESTORE USERS R0-RC                          
         BASR  RE,RF               RF CONTAINED A(HOOK ROUTINE)                 
         XIT1                                                                   
         SPACE 1                                                                
DCLPMC   DS    0X                  MIXED CASE LITERALS                          
         DCDDL GE#AT,(#ATLQ)           AT                                       
         DCDDL GE#CREON,(#CREONLQ)     CREATED ON                               
         DCDDL GE#ON,(#ONLQ)           ON                                       
         DCDDL GE#PAGE,(#PAGELQ)       PAGE                                     
         DCDDL GE#PER,(#PERLQ)         PERIOD                                   
         DCDDL GE#REP,(#REPLLQ)        REPORT                                   
         DCDDL GE#REQR,(#REQRLQ)       REQUESTOR                                
         DCDDL GE#RUNDT,(#RUNDTLQ)     RUN DATE                                 
         DCDDL GE#RUNON,(#RUNONLQ)     RUN ON                                   
         DCDDL GE#RUNTM,(#RUNTMLQ)     RUN TIME                                 
         DCDDL GE#SUBPG,(#SUBPGLQ)     SUBPAGE                                  
         DC    AL1(0)                                                           
         SPACE 1                                                                
RUNMSGLQ EQU   #RUNONLQ+1+#ATLQ+1+MAXDATLQ+1+HHMMLQ                             
REPMSGLQ EQU   #REPLQ+1+4+1+#ONLQ+1+MAXDATLQ+3+HHMMLQ                           
CREMSGLQ EQU   #CREONLQ+1+MAXDATLQ+1+#ATLQ+1+HHMMLQ                             
MAXDATLQ EQU   8                   MAX DATE LENGTH (O/P TYPE 8)                 
MAXPERLQ EQU   MAXDATLQ+1+MAXDATLQ DDMMYY-DDMMYY                                
HHMMLQ   EQU   5                   HH.MM                                        
         SPACE 1                                                                
MTHIDX   DC    A(MTHENG),A(MTHENG),A(MTHENG),A(MTHGER),A(MTHFRE)                
         DC    A(MTHENG),A(MTHENG),A(MTHENG),A(MTHENG),A(MTHENG)                
*                                                                               
DAYIDX   DC    A(DAYENG),A(DAYENG),A(DAYENG),A(DAYGER),A(DAYFRE)                
         DC    A(DAYENG),A(DAYENG),A(DAYENG),A(DAYENG),A(DAYENG)                
         SPACE 1                                                                
MTHENG   DC    CL36'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                       
MTHGER   DC    CL36'JANFEBMRZAPRMAIJUNJULAUGSEPOKTNOVDEZ'                       
MTHFRE   DC    CL36'JANFEVMARAVRMAIJUNJULAOUSEPOCTNOVDEC'                       
*                                                                               
DAYENG   DC    CL21'MONTUEWEDTHUFRISATSUN'                                      
DAYGER   DC    CL21'MONDIEMITDONFRESAMSON'                                      
DAYFRE   DC    CL21'LUNMARMERJEUVENSAMDIM'                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR SPOOL CONTROL                                          
         SPACE 3                                                                
SPOOLERD DSECT                                                                  
REPDUB   DS    D                                                                
REPWORK  DS    CL40                                                             
CONTROL  DS    CL4                                                              
PROGSW   DS    CL1                                                              
FILTSW   DS    CL1                                                              
PRINTSW  DS    CL1                                                              
SKPLINES DS    XL1                                                              
REPARA   DS    6F                                                               
REPSAVE  DS    F                                                                
SPPARA   DS    6F                                                               
RELO     DS    A                                                                
SPCC     DS    CL1                                                              
SPLINE   DS    CL198                                                            
MYSPACES DS    CL198                                                            
SPDUB    DS    D                                                                
DUB      DS    D                                                                
WORK     DS    CL32                                                             
DATCON   DS    A                                                                
USERRD   DS    A                                                                
PWIDTH   DS    F                                                                
EXWIDE   DS    F                                                                
WIDEOPT  DS    CL1                                                              
SAVEP    DS    CL198                                                            
AH1      DS    A                   A(HEADLINES)                                 
AM1      DS    A                   A(MIDLINES)                                  
AM2      DS    A                                                                
AP1      DS    A                   A(PRINT LINES)                               
AP4      DS    A                                                                
         SPACE 1                                                                
LPMCASE  DS    0X                  MIXED CASE LITERAL POOL                      
         DSDDL PRINT=YES                                                        
         SPACE 1                                                                
SPOOLERX EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDWIDED                                                        
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*DMPRTQD                                                                        
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
*DMPRTQL                                                                        
       ++INCLUDE DMPRTQL                                                        
         SPACE 1                                                                
       ++INCLUDE GEDDEQUS                                                       
         SPACE 1                                                                
       ++INCLUDE DDCOMFACS                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024DDSPOOL   05/12/20'                                      
         END                                                                    
