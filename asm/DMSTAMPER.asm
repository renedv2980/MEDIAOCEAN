*          DATA SET DMSTAMPER  AT LEVEL 003 AS OF 11/06/13                      
*PHASE DMSTAMPA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DMSECHK                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE LOGIO                                                                  
         TITLE 'DMSTAMPER - SET CPUID/DSPACE IN REC ZERO OF 1ST TRACK'          
         PRINT NOGEN                                                            
STAMPER  CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,DMSTAMP,WORK=A(STMPWORK)                                       
*                                                                               
INIT     L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         MVC   TITLE(40),=C'XXX - STAMP CPU AND DSPACE IDS INTO FILE'           
*                                                                               
INIT1    L     R1,X'10'(,R0)       GET CPU ID                                   
         L     R1,X'C4'(,R1)       R1=A(SMCA) FROM CVT                          
         MVC   CPUID,X'10'(R1)     CPU IS C'XXXX' FROM SMCASID                  
         MVC   TITLE(3),CPUID                                                   
         MVC   P(15),=C'PARAMETER CARDS'                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(15),=C'---------------'                                        
         GOTO1 =V(PRINTER)                                                      
         EJECT                                                                  
INPUT    GOTO1 =V(CARDS),P1,C,=C'RE00'                                          
         CLC   C(2),=C'/*'                                                      
         BE    STAMPERX                                                         
*                                                                               
INPUT1   CLC   C(10),SPACES        ALLOW BLANK AND ASTERISK CARDS               
         BE    *+12                                                             
         CLI   C,C'*'                                                           
         BNE   INPUT2                                                           
         MVC   P(80),C             PRINT COMMENT CARDS                          
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
*                                                                               
INPUT2   CLC   C(6),=C'DDSIO='     OVERRIDE DDSIO                               
         BNE   INPUT3                                                           
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),C+6                                                      
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
*                                                                               
INPUT3   CLC   C(6),=C'CPUID='     SET DEFAULT CPU ID                           
         BNE   INPUT4                                                           
         MVC   P(80),C                                                          
         LA    R4,CPUIDTAB         INPUT CPU ID MUST BE IN CPU TABLE            
INPUT3A  CLC   0(3,R4),=C'END'                                                  
         BE    INPUT3E                                                          
         CLC   0(3,R4),C+6                                                      
         BE    INPUT3B                                                          
         LA    R4,L'CPUIDTAB(R4)                                                
         B     INPUT3A                                                          
INPUT3B  MVC   ICPUID,3(R4)        SAVE INTERNAL INPUT VALUE OF CPU ID          
         LA    R4,CPUIDTAB                                                      
INPUT3C  CLC   0(3,R4),=C'END'     RESEARCH TABLE FOR FULL ALPHA ID             
         JE    *+2                                                              
         CLC   3(1,R4),ICPUID                                                   
         BE    INPUT3D                                                          
         LA    R4,L'CPUIDTAB(R4)                                                
         B     INPUT3C                                                          
INPUT3D  MVC   ICPUIDA,0(R4)                                                    
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
INPUT3E  MVC   PEMSG,=CL32'*ERROR* INVALID CPU ID'                              
         B     STMPERRY                                                         
*                                                                               
INPUT4   CLC   C(7),=C'DSPACE='    SET DEFAULT DSPACE ID                        
         BNE   INPUT5                                                           
         MVC   P(80),C                                                          
         LA    R1,C+7                                                           
         BAS   RE,VALDSP           VALIDATE DATA SPACE ID CHR                   
         BNE   INPUT4E                                                          
         L     RE,=V(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RE),C+7                                        
*                                                                               
         MVC   IDSPIDA(3),=C'DMG'                                               
         MVC   IDSPIDA+3(1),C+7                                                 
         MVC   IDSPID,1(RF)        SAVE INPUT VALUE FROM TABLE                  
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
INPUT4E  MVC   PEMSG,=CL32'*ERROR* INVALID DATA SPACE ID'                       
         B     STMPERRY                                                         
*                                                                               
INPUT5   CLC   C(6),=C'WRITE='     WRITE=N TO INHIBIT STAMP TO DISK             
         BNE   INPUT6                                                           
         MVC   P(80),C                                                          
         CLI   C+6,C'Y'                                                         
         BE    *+12                                                             
         CLI   C+6,C'N'                                                         
         BNE   INPUT5E                                                          
         MVC   WRITE,C+6                                                        
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
INPUT5E  MVC   PEMSG,=CL32'*ERROR* INVALID WRITE= CARD'                         
         B     STMPERRY                                                         
*                                                                               
INPUT6   CLC   C(6),=C'FORCE='     FORCE=Y TO FORCE STAMPING                    
         BE    INPUT6A                                                          
         CLC   C(8),=C'OVERRIDE'   OVERRIDE SAME AS FORCE=Y                     
         BNE   INPUT7                                                           
         MVC   P(80),C                                                          
         MVI   FORCE,C'Y'                                                       
         B     INPUT6B                                                          
INPUT6A  MVC   P(80),C                                                          
         CLI   C+6,C'Y'                                                         
         BE    *+12                                                             
         CLI   C+6,C'N'                                                         
         BNE   INPUT6E                                                          
         MVC   FORCE,C+6                                                        
INPUT6B  GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
INPUT6E  MVC   PEMSG,=CL32'*ERROR* INVALID FORCE= CARD'                         
         B     STMPERRY                                                         
*                                                                               
INPUT7   CLC   C(7),=C'GLOBAL='    GLOBAL=Y TO SET FILE AS GLOBAL               
         BNE   INPUT8                                                           
         MVC   P(80),C                                                          
         CLI   C+7,C'Y'                                                         
         BE    *+12                                                             
         CLI   C+7,C'N'                                                         
         BNE   INPUT7E                                                          
         MVC   GLOBAL,C+7                                                       
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
INPUT7E  MVC   PEMSG,=CL32'*ERROR* INVALID GLOBAL= CARD'                        
         B     STMPERRY                                                         
*                                                                               
INPUT8   MVC   P,SPACES            PRINT FILE NAME CARD                         
         GOTO1 =V(PRINTER)                                                      
INPUT8A  MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     STMP                                                             
*                                                                               
STAMPERX LLC   RF,RETCODE                                                       
         XBASE RC=(RF)                                                          
         EJECT                                                                  
***********************************************************************         
*FILE CARD CCCFFFFFFFFD WHERE CCC=CPUID, FFFFFFFF=FILEID, D=DSPID     *         
***********************************************************************         
STMP     LA    R8,FILE             R8=A(FILEDTF)                                
         USING DTFPHD,R8                                                        
         MVI   17(R8),0            SET SENUM UNKNOWN                            
         MVI   18(R8),0            SET EXTERNAL FILE NUMBER UNKNOWN             
*                                                                               
         NI    19(R8),255-X'40'    SET LOCAL FILE                               
         CLI   GLOBAL,C'Y'                                                      
         BNE   *+8                                                              
         OI    19(R8),X'40'        SET GLOBAL FILE IF GLOBAL=Y INPUT            
*                                                                               
         NI    36(R8),255-X'80'    SET UPDATIVE FILE                            
         CLI   WRITE,C'N'                                                       
         BNE   *+8                                                              
         OI    36(R8),X'80'        SET READ-ONLY IF WRITE=NO                    
*                                                                               
         MVC   DTFDD,C+3           SET FILE NAME IN DTF                         
         MVC   FILEDD,C+3                                                       
         MVI   RESULT,0                                                         
         MVI   ERRORF,0                                                         
*                                                                               
         MVC   PLINE,PTEXT         SET UP PRINT LINE FIELDS                     
         MVI   PRINTF,X'00'                                                     
         MVC   PDD,C+3             FILE NAME TO PRINT LINE                      
         MVC   PNCPU,C             NEW CPU ID TO PRINT LINE                     
         MVC   PNDSP,C+11          NEW DSP ID TO PRINT LINE                     
*                                                                               
STMP1    MVC   DUB(3),C            DUB=INPUT CPUID VALUE                        
         LA    R4,CPUIDTAB         INPUT CPU ID MUST BE IN CPU TABLE            
STMP1A   CLI   DUB,C'&&'           REPLACE & CPUID WITH INPUT DEFAULT           
         BNE   STMP1B                                                           
         CLI   ICPUIDA,C' '                                                     
         BE    STMPERR1                                                         
         MVC   DUB(3),ICPUIDA      SET DEFAULT VALUE FROM CPUID= CARD           
         B     STMP1D                                                           
STMP1B   CLC   DUB(3),SPACES       BLANK OR DOTS MEANS NO CHANGE                
         BE    *+14                                                             
         CLC   DUB(3),NOCHANGE     BLANK OR DOTS MEANS NO CHANGE                
         BNE   STMP1D                                                           
         MVI   NCPUID,X'FF'                                                     
         MVC   NCPUIDA,NOCHANGE                                                 
         B     STMP1X                                                           
STMP1D   CLC   0(3,R4),=C'END'                                                  
         BE    STMPERR1                                                         
         CLC   0(3,R4),DUB                                                      
         BE    STMP1E                                                           
         LA    R4,L'CPUIDTAB(R4)                                                
         B     STMP1D                                                           
STMP1E   MVC   NCPUID,3(R4)        SAVE INTERNAL VALUE OF CPU ID                
         LA    R4,CPUIDTAB                                                      
STMP1F   CLC   0(3,R4),=C'END'     RE SEARCH TABLE FOR FULL ALPHA ID            
         JE    *+2                                                              
         CLC   3(1,R4),NCPUID                                                   
         BE    STMP1G                                                           
         LA    R4,L'CPUIDTAB(R4)                                                
         B     STMP1F                                                           
STMP1G   MVC   NCPUIDA,0(R4)                                                    
STMP1X   MVC   PNCPU,NCPUIDA       FULL THREE CHR CPU ID TO PRINT LINE          
*                                                                               
STMP2    MVC   DUB(1),C+11         DUB=INPUT DSPID VALUE                        
STMP2A   CLI   DUB,C'&&'           REPLACE & DSPID WITH INPUT DEFAULT           
         BNE   STMP2B                                                           
         CLI   IDSPIDA,C' '                                                     
         BE    STMPERR5                                                         
         MVC   DUB(1),IDSPIDA+3    SET DEFAULT VALUE FROM DSPACE=CARD           
         B     STMP2D                                                           
STMP2B   CLI   DUB,C' '            BLANK OR DOT MEANS NO CHANGE                 
         BE    *+14                                                             
         CLC   DUB(1),NOCHANGE                                                  
         BNE   STMP2D                                                           
         MVI   NDSPID,X'FF'                                                     
         MVC   NDSPIDA,NOCHANGE                                                 
         B     STMP2X                                                           
STMP2D   LA    R1,DUB                                                           
         BAS   RE,VALDSP                                                        
         BNE   STMPERR5                                                         
         MVC   NDSPID,1(RF)        SAVE INTERNAL VALUE OF DATA SPACE ID         
         MVC   NDSPIDA(3),=C'DMG'                                               
         MVC   NDSPIDA+3(1),0(RF)                                               
         CLI   NDSPID,0                                                         
         BNE   *+10                                                             
         MVC   NDSPIDA,NONE                                                     
STMP2X   MVC   PNDSP,NDSPIDA+3     NEW DSPACE CHR TO PRINT LINE                 
*                                                                               
STMP3    GOTO1 =V(DADDS),P1,A(DAOPEN),DISKREC,0,(R8),DISKADR                    
*                                                                               
STMP4    GOTO1 =V(DADDS),P1,A(RDID),DISKREC,0,(R8),DISKADR                      
         OC    P3(2),P3                                                         
         BNZ   STMPERR3            DISK READ ERROR                              
         MVC   OCPUID,DISKREC+6    SAVE OLD CPU ID                              
         MVC   ODSPID,DISKREC+7    SAVE OLD DSP ID                              
*                                                                               
STMP5    LA    R4,CPUIDTAB         SEARCH CPU TABLE FOR RECZERO VALUE           
         MVC   OCPUIDA,UNKNOWN                                                  
STMP5A   CLC   0(3,R4),=C'END'                                                  
         BE    STMP5C                                                           
         CLC   3(1,R4),OCPUID                                                   
         BE    STMP5B                                                           
         LA    R4,L'CPUIDTAB(R4)                                                
         B     STMP5A                                                           
STMP5B   MVC   OCPUIDA,0(R4)       SAVE EXTERNAL VALUE OF CPU ID                
STMP5C   MVC   POCPU,OCPUIDA       OLD CPU ID TO PRINT LINE                     
*                                                                               
STMP6    LA    R4,DSPTAB           SEARCH DSP TABLE FOR RECZERO VALUE           
         MVC   ODSPIDA,UNKNOWN                                                  
         CLI   ODSPID,C' '         TEST IF DSPACE ID NEVER SET                  
         BNE   *+12                                                             
         MVI   ODSPID,0                                                         
         B     STMP6C                                                           
STMP6A   CLI   0(R4),X'FF'                                                      
         BE    STMP6C                                                           
         CLC   1(1,R4),ODSPID                                                   
         BE    STMP6B                                                           
         LA    R4,L'DSPTAB(R4)                                                  
         B     STMP6A                                                           
STMP6B   MVC   ODSPIDA,=C'DMG'     SAVE EXTERNAL VALUE OF DSP ID                
         MVC   ODSPIDA+3(1),0(R4)                                               
         CLI   1(R4),0                                                          
         BNE   STMP6C                                                           
         MVC   ODSPIDA,NONE                                                     
STMP6C   MVC   PODSP,ODSPIDA+3     OLD DSPACE CHR TO PRINT LINE                 
*                                                                               
STMP7    CLC   OCPUIDA,NONE        TEST FILE HAS A VALID CPU ID STAMP           
         BE    STMP8                                                            
         CLC   OCPUIDA,UNKNOWN                                                  
         BE    STMP8                                                            
         CLC   OCPUIDA,CPUID       MUST RUN STAMPER IN SAME CPU                 
         BE    STMP8                                                            
         CLI   FORCE,C'Y'          CAN OVERRIDE BY FORCE=Y                      
         BE    STMP8                                                            
         B     STMPERR6                                                         
*                                                                               
STMP8    CLC   ODSPIDA,NONE        TEST FILE HAS A VALID DSP ID STAMP           
         BE    STMP9                                                            
         CLC   ODSPIDA,UNKNOWN                                                  
         BE    STMP9                                                            
         CLI   NCPUID,X'FF'        TEST IF CPU STAMP STAYS THE SAME             
         BE    STMP9                                                            
         CLC   NCPUID,OCPUID                                                    
         BE    STMP9                                                            
         CLI   GLOBAL,C'N'         TEST TO TREAT FILE AS LOCAL                  
         BE    STMP9                                                            
         MVC   SAVEP,P             SAVE PRINT LINE                              
         LA    R0,0                                                             
         CLI   FORCE,C'Y'                                                       
         BNE   *+8                                                              
         LA    R0,X'20'                                                         
         GOTO1 =V(DMSECHK),DMCB,((R0),=C'DMSLCK'),FILEDD,(R8),(4,0)             
         L     RE,12(R1)                                                        
         MVC   MSG,0(RE)                                                        
         MVC   RESULT,12(R1)                                                    
         L     RE,16(R1)                                                        
         MVC   FILEINFO,0(RE)                                                   
         MVC   RESERR,16(R1)                                                    
         MVC   P,SAVEP                                                          
         CLI   RESULT,2            0,1,2 OK TO STAMP FILE                       
         BL    STMP9                                                            
         BH    STMPERR7            OPERATOR CANCEL                              
         MVC   PEMSG,MSG           OPERATOR/AUTO IGNORE                         
         LA    R0,4                RC=4 FOR AUTO IGNORE                         
         CLI   FORCE,C'Y'                                                       
         BE    *+8                                                              
         LA    R0,8                RC=8 FOR OPERATOR IGNORE                     
         CLM   R0,1,RETCODE                                                     
         BNH   *+8                                                              
         STCM  R0,1,RETCODE        SET RC IF HIGHER                             
*                                                                               
STMP9    EQU   *                                                                
*                                                                               
STMPA    CLI   NCPUID,X'FF'        WRITE BACK RECORD ZERO                       
         BE    *+10                                                             
         MVC   DISKREC+6(1),NCPUID PUT NEW CPU ID IN RECZERO+6(1)               
         CLI   NDSPID,X'FF'                                                     
         BE    *+10                                                             
         MVC   DISKREC+7(1),NDSPID PUT NEW DSP ID IN RECZERO+7(1)               
         CLI   WRITE,C'Y'                                                       
         BNE   STMPB                                                            
         GOTO1 =V(DADDS),P1,A(WTID),,8                                          
         OC    P3(2),P3                                                         
         BNZ   STMPERR4            DISK WRITE ERROR                             
*                                                                               
STMPB    MVC   CONSOLE(28),C       CREATE CONSOLE MESSAGE                       
         MVC   CONSOLE+28(32),PEMSG                                             
         TM    ERRORF,X'80'        OUTPUT CONSOLE MESSAGE IF ERROR              
         BZ    STMPB1                                                           
         GOTO1 =V(LOGIO),DUB,X'FF000001',(60,CONSOLE)                           
STMPB1   GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         CLI   RESULT,1            TEST IF DSPACE LOCK APPLIED                  
         BNE   STMPC                                                            
         GOTO1 =V(DMSECHK),DMCB,(X'00',=C'DMSUNL')                              
*                                                                               
STMPC    GOTO1 =V(DADDS),P1,A(DACLOSE),DISKREC,0,(R8),DISKADR                   
*                                                                               
STMPX    TM    ERRORF,X'80'        TEST CANCEL FLAG                             
         BO    STMPERRY                                                         
         TM    PRINTF,X'01'        TEST IF HAVE ALREADY PRINTED                 
         BO    INPUT                                                            
         GOTO1 =V(PRINTER)                                                      
         B     INPUT               BACK FOR NEXT INPUT CARD                     
*                                                                               
VALDSP   LA    RF,DSPTAB           POINT TO TABLE OF DSPACE IDS                 
         SR    R0,R0                                                            
VALDSP1  CLI   0(RF),X'FF'         TEST END OF TABLE                            
         BNE   *+12                                                             
         LA    R0,1                                                             
         B     VALDSPX                                                          
VALDSP2  CLC   0(1,R1),0(RF)       IDENTIFY BY LAST CHR                         
         BE    VALDSPX                                                          
         LA    RF,L'DSPTAB(RF)                                                  
         B     VALDSP1                                                          
VALDSPX  LTR   R0,R0               EXIT WITH CC EQL IF VALID                    
         BR    RE                                                               
         EJECT                                                                  
STMPERR1 MVC   PEMSG,=CL32'*ERROR* INVALID CPU ID'                              
         B     STMPERRX                                                         
*                                                                               
STMPERR2 MVC   PEMSG,=CL32'*ERROR* INVALID FILE ID'                             
         B     STMPERRX                                                         
*                                                                               
STMPERR3 MVC   PEMSG,=CL32'*ERROR* DISK READ'                                   
         B     STMPERRX                                                         
*                                                                               
STMPERR4 MVC   PEMSG,=CL32'*ERROR* DISK WRITE'                                  
         B     STMPERRX                                                         
*                                                                               
STMPERR5 MVC   PEMSG,=CL32'*ERROR* INVALID DATA SPACE ID'                       
         B     STMPERRX                                                         
*                                                                               
STMPERR6 MVC   PEMSG,=CL32'*ERROR* MUST STAMP FROM OLD CPU'                     
         B     STMPERRX                                                         
*                                                                               
STMPERR7 MVC   PEMSG,MSG           MSG PASSED BACK FROM V(DMSECHK)              
         OI    ERRORF,X'80'        SET OPERATOR CANCEL REQUEST                  
         B     STMPERRX                                                         
*                                                                               
STMPERRX OI    ERRORF,X'01'        SET ERROR FLAG                               
         MVI   RETCODE,16                                                       
         B     STMPB                                                            
*                                                                               
STMPERRY MVC   CONSOLE(28),C       CREATE CONSOLE MESSAGE                       
         MVC   CONSOLE+28(32),PEMSG                                             
         GOTO1 =V(LOGIO),DUB,X'FF000001',(60,CONSOLE)                           
         GOTO1 =V(PRINTER)                                                      
         MVI   RETCODE,16                                                       
         B     STAMPERX                                                         
         EJECT                                                                  
         PRINT GEN                                                              
FILE     DMDA  DSKXTNT=0,BIG=22BIT                                              
*                                                                               
BLDXTNT  DS    0C                                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
***********************************************************************         
*CPUID TABLE CONTAINS POSSIBLE 3 CHR CARD INPUT FIELDS                *         
*SYNONYMS ARE DEFINED WITH THE ONE CHR HEX VALUE THAT IS IN REC0+6(1) *         
*DSPACE TABLE CONTAINS ONE CHR VALUE THAT IS IN REC0+7(1)             *         
***********************************************************************         
*&&US                                                                           
CPUIDTAB DS    0CL4                CPU-ID TABLE FOR USA                         
*                                                                               
         DC    C'000',X'00'                                                     
         DC    C'SYA',X'C1'                                                     
         DC    C'SYB',X'C2'                                                     
         DC    C'SYC',X'C3'                                                     
         DC    C'SYD',X'C4'                                                     
         DC    C'SYN',X'D5'                                                     
         DC    C'SYT',X'E3'                                                     
*                                                                               
         DC    C'00 ',X'00'                                                     
         DC    C'SA ',X'C1'                                                     
         DC    C'SB ',X'C2'                                                     
         DC    C'SC ',X'C3'                                                     
         DC    C'SD ',X'C4'                                                     
         DC    C'SN ',X'D5'                                                     
         DC    C'ST ',X'E3'                                                     
*                                                                               
         DC    C'0  ',X'00'                                                     
         DC    C'A  ',X'C1'                                                     
         DC    C'B  ',X'C2'                                                     
         DC    C'C  ',X'C3'                                                     
         DC    C'D  ',X'C4'                                                     
         DC    C'N  ',X'D5'                                                     
         DC    C'T  ',X'E3'                                                     
*                                                                               
CPUIDTAX DC    C'END',X'FF'                                                     
*                                                                               
DSPTAB   DS    0CL2                                                             
         DC    C'0',X'00'          NONE                                         
         DC    C'A',C'A'           ADV                                          
         DC    C'C',C'C'           CSC                                          
         DC    C'Q',C'Q'           FQA                                          
         DC    C'R',C'R'           REP                                          
         DC    C'T',C'T'           TEST                                         
DSPTABX  DC    X'FFFF'                                                          
*&&                                                                             
*&&UK                                                                           
CPUIDTAB DS    0CL4                CPU-ID TABLE FOR UK                          
*                                                                               
         DC    C'000',X'00'                                                     
         DC    C'SY1',X'F1'                                                     
         DC    C'SY2',X'F2'                                                     
         DC    C'SY7',X'F7'                                                     
*                                                                               
         DC    C'00 ',X'00'                                                     
         DC    C'S1 ',X'F1'                                                     
         DC    C'S2 ',X'F2'                                                     
         DC    C'S7 ',X'F7'                                                     
*                                                                               
         DC    C'0  ',X'00'                                                     
         DC    C'1  ',X'F1'                                                     
         DC    C'2  ',X'F2'                                                     
         DC    C'7  ',X'F7'                                                     
*                                                                               
CPUIDTAX DC    C'END',X'FF'                                                     
*                                                                               
DSPTAB   DS    0CL2                                                             
         DC    C'0',X'00'          NONE                                         
         DC    C'A',C'A'           ADV                                          
         DC    C'C',C'C'           CSC                                          
         DC    C'Q',C'Q'           FQA                                          
         DC    C'B',C'B'           BAR                                          
         DC    C'T',C'T'           TEST                                         
DSPTABX  DC    X'FFFF'                                                          
*&&                                                                             
         EJECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
DISKADR  DC    X'00000400'         22-BIT TRACK 1/BLOCK 0/RECORD 0              
FILEDD   DC    CL8' '                                                           
*                                                                               
UNKNOWN  DC    CL4'????'                                                        
NONE     DC    CL4'0000'                                                        
NOCHANGE DC    CL4'....'                                                        
*                                                                               
CPUID    DC    CL4' '              CPU ID                                       
*                                                                               
ICPUID   DC    XL1'00'             INPUT VALUES                                 
IDSPID   DC    XL1'00'                                                          
ICPUIDA  DC    CL3' '                                                           
IDSPIDA  DC    CL4' '                                                           
*                                                                               
NCPUID   DC    XL1'00'             NEW VALUES                                   
NDSPID   DC    XL1'00'                                                          
NCPUIDA  DC    CL3' '                                                           
NDSPIDA  DC    CL4' '                                                           
*                                                                               
OCPUID   DC    XL1'00'             OLD VALUES                                   
ODSPID   DC    XL1'00'                                                          
OCPUIDA  DC    CL3' '                                                           
ODSPIDA  DC    CL4' '                                                           
*                                                                               
WRITE    DC    C'Y'                SET BY WRITE=                                
FORCE    DC    C'N'                SET BY FORCE=                                
GLOBAL   DC    C' '                SET BY GLOBAL=                               
PRINTF   DC    X'00'                                                            
ERRORF   DC    X'00'                                                            
RESULT   DC    X'00'                                                            
RESERR   DC    X'00'                                                            
*                                                                               
RETCODE  DC    X'00'                                                            
*                                                                               
DISKREC  DC    XL100'00'                                                        
C        DC    CL80' '                                                          
MSG      DC    CL60' '                                                          
CONSOLE  DC    CL60' '                                                          
SAVEP    DC    CL132' '                                                         
*                                                                               
PTEXT    DC    CL12'DD=........ '                                               
         DC    CL22'OLDCPU=... NEWCPU=... '                                     
         DC    CL18'OLDDSP=. NEWDSP=. '                                         
         DC    CL32' '                                                          
PTEXTX   EQU   *                                                                
*                                                                               
         DS    0F                                                               
FILEINFO DS    0XL16               RETURNED FILE INFO FROM V(DMDDNAME)          
         DS    X                   RESERVED                                     
SENUM    DS    X                   SE NUMBER                                    
SENAME   DS    CL5                 SE SHORT NAME                                
SEFLAG1  DS    X                   SE FLAGS                                     
FILNUM   DS    X                   FILE NUMBER                                  
FILFLAG1 DS    X                   FILE FLAGS                                   
FILFLAG2 DS    X                   FILE FLAGS                                   
         DS    X                   N/D                                          
FILADTF  DS    AL4                 FILE A(DTF)                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
         DC    C'**SSB***'                                                      
SSB      DC    XL2'00',X'FF',XL5'00'                                            
         DC    (SSOOFFX-SSODATE)X'00'                                           
*                                                                               
         DS    0D                                                               
         DC    C'**UTL***'                                                      
UTL      DC    512X'00'                                                         
*                                                                               
STMPWORK DS    1000D                                                            
         EJECT                                                                  
PLINED   DSECT                                                                  
PLINE    DS    0CL84                                                            
         DS    CL3                                                              
PDD      DS    CL8                 FILE DD NAME                                 
         DS    CL8                                                              
POCPU    DS    CL3                 OLD CPU ID                                   
         DS    CL8                                                              
PNCPU    DS    CL3                 NEW CPU ID                                   
         DS    CL8                                                              
PODSP    DS    CL1                 OLD DATASPACE ID                             
         DS    CL8                                                              
PNDSP    DS    CL1                 NEW DATASPACE ID                             
         DS    CL1                                                              
PEMSG    DS    CL32                ERROR MESSAGE                                
PLINEX   EQU   *                                                                
         EJECT                                                                  
*DMGREQUS                                                                       
       ++INCLUDE DMGREQUS                                                       
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
*FASSB                                                                          
       ++INCLUDE FASSB                                                          
         ORG     SSBD                                                           
       ++INCLUDE FASSBOFF                                                       
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DMSTAMPER 11/06/13'                                      
         END                                                                    
