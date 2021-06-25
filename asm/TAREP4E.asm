*          DATA SET TAREP4E    AT LEVEL 018 AS OF 11/12/15                      
*PHASE T7034EC,*                                                                
*INCLUDE ADSCAN                                                                 
*INCLUDE DLFLD                                                                  
         TITLE 'T7034E - CALIFORNIA CORP WITHHOLDING TAX STATEMENT'             
T7034E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7034E,R8                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING T703FFD,RA                                                       
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7              R7=A(LOCAL W/S)                              
         GOTO1 INITIAL,DMCB,0                                                   
         MVC   AMASTD,TWAMASTC                                                  
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   ALOGOC,TLOGOC                                                    
*                                                                               
         BAS   RE,MYCLEAR                                                       
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         MVC   MYRERUN,MCRERUN     SET RERUN STATUS                             
         MVC   MYAREMOT,MCVREMOT   SAVE A(REMOTE)                               
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
VREC     NTR1                                                                   
         LA    R2,SCCYEARH         YEAR                                         
         GOTO1 ANY                                                              
         CLI   5(R2),4             MUST BE 4 CHARACTERS                         
         BNE   INVXIT                                                           
         MVI   QTRNUM,0            INIT QTR NUMBER TO 0                         
         MVC   WORK(4),=C'0000'                                                 
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),=C'0000'                                                 
         BNE   INVXIT                                                           
         MVC   TIFYEAR(2),10(R2)                                                
         MVI   TIFYEAR+2,X'40'                                                  
         SPACE 1                                                                
         LA    R2,SCCEMPH          EMPLOYER REQUIRED                            
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2),0                                       
         MVC   TIFEMP,TGEMP                                                     
*                                                                               
         XC    TIFSSN,TIFSSN                                                    
         LA    R2,SCCSSNH          SS#                                          
         CLI   5(R2),0             ELSE, OPTIONAL                               
         BE    VREC6                                                            
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SCCSSNNH                        
         MVC   TIFSSN,TGSSN                                                     
*                                                                               
VREC6    LA    R2,SCCOPTH          VALIDATE OPTIONS                             
         BAS   RE,VOPTS                                                         
*                                                                               
         TM    WHEN,X'20'          IF SOON                                      
         BZ    VREC8                                                            
         CLI   QTRNUM,0            QUARTER NUMBER IS OK FOR SOON                
         BE    VREC7                                                            
         MVI   REQSML,C'L'         SOON LONG RUNNING JOB                        
         B     VREC8                                                            
*                                                                               
VREC7    LA    R2,SCCSSNH          SS#                                          
         CLI   5(R2),0             REQURIED FOR SOON IF NO QTR NUMBER           
         BE    INVSOON                                                          
*                                                                               
VREC8    CLI   OFFLINE,C'Y'        IF RUNNING OFFLINE                           
         BNE   VRECX                                                            
         L     R1,MYAREMOT         R1=A(REMOTE)                                 
         USING REMOTED,R1                                                       
         OC    REMOTKEY,REMOTKEY   IF GOING DIRECT                              
         BZ    VRECX                                                            
         MVC   REMOTKEY(7),=C'CAWITH-'  SHOW YEAR IN REPORT NAME                
         MVC   REMOTKEY+7(2),TIFYEAR                                            
VRECX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 1                                                                
VOPTS    NTR1                                                                   
         MVI   MYOPTS,0                                                         
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    INVXIT                                                           
         SPACE 1                                                                
OPT2     CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT3                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB        CHECK RECORD TRACE LIMIT                     
         B     OPTEND                                                           
*                                                                               
OPT3     CLC   12(6,R4),=C'NOFORM' DON'T PRINT ON FORM                          
         BNE   OPT4                                                             
         OI    MYOPTS,ONOFORM      SET NOT PRINTING ON FORM                     
         B     OPTEND                                                           
*                                                                               
OPT4     CLC   12(4,R4),=C'DISK'   CREATE MMREF DISK                            
         BNE   OPT5                                                             
         TM    WHEN,X'20'          IF SOON                                      
         BZ    *+8                                                              
         MVI   REQSML,C'L'         SOON LONG RUNNING JOB                        
*                                                                               
         OI    MYOPTS,OMMDISK      SET                                          
         B     OPTEND                                                           
*                                                                               
OPT5     CLC   12(3,R4),=C'QTR'    QUARTERLY REPORTING?                         
         BNE   OPT8                                                             
         CLI   1(R4),1             LENGTH OF QUARTER NUMBER                     
         BH    INVXIT                                                           
         CLI   22(R4),C'1'         CAN ONLY BE 1-4                              
         BL    INVXIT                                                           
         CLI   22(R4),C'4'                                                      
         BH    INVXIT                                                           
         MVC   QTRNUM,22(R4)                                                    
         NI    QTRNUM,X'0F'                                                     
         B     OPTEND                                                           
*                                                                               
OPT8     DS    0H                                                               
         B     INVXIT                                                           
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         SPACE 1                                                                
INVSOON  MVI   ERROR,ERSOONRQ                                                   
         B     *+8                                                              
INVXIT   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         SPACE 1                                                                
TRALIMIT DC    PL6'0'                                                           
CKCOUNT  DC    PL6'0'                                                           
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         XC    SVSTAX,SVSTAX       CLEAR AMOUNTS                                
         XC    TOTSTAX,TOTSTAX                                                  
         XC    TOTGROSS,TOTGROSS                                                
         XC    SVGROSS,SVGROSS                                                  
         XC    LASTSSN,LASTSSN                                                  
         XC    TPNAME,TPNAME                                                    
         XC    CRPSSN,CRPSSN                                                    
         XC    TAPECNT,TAPECNT                                                  
*                                                                               
         TM    MYOPTS,ONOFORM      IF NOT PRINTING ON FORM                      
         BZ    *+8                                                              
         BAS   RE,HEADING          PRINT OUT HEADING                            
*                                                                               
         TM    MYOPTS,OMMDISK      CREATING MMREF DISK                          
         BZ    PREP3                                                            
         BAS   RE,OPENF                                                         
*                                                                               
PREP3    MVI   MYBYTE,X'80'        SET NEED LINEUP PATTERN                      
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         MVI   TIREAD,TLCKYCDQ     SET TO READ CHECK YTD POINTER                
         MVI   TIFCUR,C'U'         ONLY INTERESTED IN $US                       
         MVC   WORK(2),TIFYEAR     SET CHECK DATE PERIOD                        
         MVC   WORK+6(2),TIFYEAR                                                
         MVC   WORK+2(4),=C'0101'                                               
         MVC   WORK+8(4),=C'1231'                                               
*                                                                               
         CLI   QTRNUM,0            NO QTR NUMBER?                               
         BE    PREP4               DO THE WHOLE YEAR                            
         CLI   QTRNUM,1                                                         
         BNE   PREP3B                                                           
         MVC   WORK+2(4),=C'0101'  SET QUARTER 1                                
         MVC   WORK+8(4),=C'0331'                                               
         B     PREP4                                                            
PREP3B   CLI   QTRNUM,2                                                         
         BNE   PREP3C                                                           
         MVC   WORK+2(4),=C'0401'  SET QUARTER 2                                
*        MVC   WORK+8(4),=C'0630'                                               
         MVC   WORK+8(4),=C'0531'                                               
         B     PREP4                                                            
PREP3C   CLI   QTRNUM,3                                                         
         BNE   PREP3D                                                           
*        MVC   WORK+2(4),=C'0701'  SET QUARTER 3                                
*        MVC   WORK+8(4),=C'0930'                                               
         MVC   WORK+2(4),=C'0601'                                               
         MVC   WORK+8(4),=C'0831'                                               
         B     PREP4                                                            
PREP3D   CLI   QTRNUM,4                                                         
         BNE   PREP4                                                            
*        MVC   WORK+2(4),=C'1001'  SET QUARTER 4                                
         MVC   WORK+2(4),=C'0901'                                               
         MVC   WORK+8(4),=C'1231'                                               
*                                                                               
PREP4    GOTO1 DATCON,DMCB,(0,WORK),(1,TIQPSTR)                                 
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,TIQPEND)                               
         GOTO1 (RF),(R1),(1,TIQPSTR),(20,MYYEAR)                                
         MVI   TIQDTYPE,TIQDCHK                                                 
         OI    TIQFLAGS,TIQFDIR    SET WANT DIRECTORY HOOK                      
*                                  WANT CORPS & FOREIGNERS                      
*        MVI   TIFW4TY,TAW4TYCO    ONLY WANT CORPS                              
         MVC   TIFUNIT,=C'CA '     ONLY INTERESTED IN CALIFORNIA                
         MVI   TIFTUNIT,TACWSWRK   WORKED IN CALIFORNIA                         
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
                                                                                
         OC    LASTSSN,LASTSSN     IF HAVE A GOOD SSN TO PROCESS                
         BZ    PREP5                                                            
         OC    SVSTAX,SVSTAX       AND HAVE AMOUNT WITHHELD                     
         BZ    PREP5                                                            
         BAS   RE,DOFORM           PRINT IT OUT                                 
         BAS   RE,SPLAT                                                         
         BAS   RE,DOTAPE           PUT IT TO TAPE                               
*                                                                               
PREP5    TM    MYOPTS,ONOFORM      IF NOT PRINTING ON FORM                      
         BZ    PREP6                                                            
         LA    R2,P                PRINT OUT TOTALS                             
         USING HEADD,R2                                                         
         MVC   HDSSN,=C'TOTALS:  '                                              
         EDIT  TOTSTAX,(14,HDWITH),2                                            
         EDIT  TOTGROSS,(14,HDGROSS),2                                          
         BAS   RE,SPLAT                                                         
*                                                                               
PREP6    TM    MYOPTS,OMMDISK      IF NOT PRINTING ON FORM                      
         BZ    PREPDX                                                           
         BAS   RE,CLOSEF                                                        
*                                                                               
         BRAS  RE,NEWPRTQ           SET NEW PRINT QUEUE REPORT                  
         GOTO1 REQTWA,DMCB,(3,ATWA),,VPRINT,(C'B',ABOX)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R5)                                                  
*                                                                               
         L     R2,=A(CATAPE)                                                    
         TM    WHEN,X'20'          IF SOON                                      
         BZ    *+8                                                              
         L     R2,=A(TADOWN)                                                    
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,INITDWN                                                       
         BAS   RE,DOWNHEAD         PUT HEADERS TO DOWNLOAD                      
         BAS   RE,FRSTLIN          PUT WITHHOLDING AGENT                        
         GOTO1 =A(DOWNTAPE),DMCB,1   PUT TAPE RECORD TO DOWNLOAD                
*                                                                               
PREP7    GET   (R2),TAPEREC        GET RECORD                                   
         GOTO1 =A(DOWNTAPE),DMCB,0   PUT TAPE RECORD TO DOWNLOAD                
         B     PREP7                                                            
*                                                                               
PREPDX   B     XIT                                                              
*----------------------------------------------------------------------         
*              DATA SET ROUTINE                                                 
*----------------------------------------------------------------------         
NOMORE   CLOSE ((2))               CLOSE DATASET                                
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   DLCBACT,DLCBEOR     END OF REPORT                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPDX                                                           
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
IOHOOK   NTR1                                                                   
         CLI   TIW4TY,C'C'         CORPS +                                      
         BE    IOH1                                                             
         CLI   TIW4TY,C'A'         CANADIANS +                                  
         BE    IOH1                                                             
         CLI   TIW4TY,C'F'         FOREIGNERS ONLY                              
         BNE   XIT                                                              
                                                                                
IOH1     CLI   TIMODE,PROCDIR      TEST FOR DIRECTORY HOOK                      
         BNE   IOH5                                                             
         USING TLCKPD,R4                                                        
         LA    R4,TIKEY                                                         
         MVC   CRPSSN,TLCKYSSN     ALWAYS SAVE CORP'S SSN                       
         OC    LASTSSN,LASTSSN     IF HAVE A GOOD SSN TO PROCESS                
         BZ    YES                                                              
         CLC   LASTSSN,TLCKYSSN    AND SSN CHANGED                              
         BE    YES                                                              
         OC    SVSTAX,SVSTAX       AND HAVE AMOUNT WITHHELD                     
         BZ    IOH3                                                             
         BAS   RE,DOFORM           PRINT OUT FORM                               
         BAS   RE,DOTAPE           PUT IT TO TAPE                               
*                                                                               
IOH3     XC    SVSTAX,SVSTAX       CLEAR AMOUNTS                                
         XC    SVGROSS,SVGROSS                                                  
         XC    LASTSSN,LASTSSN     AND LAST SSN                                 
         B     YES                                                              
         SPACE                                                                  
IOH5     CLI   TIMODE,PROCREC                                                   
         BNE   XIT                                                              
**       L     R6,TIAREC                                                        
**       MVI   ELCODE,TATIELQ      ONLY WANT INDIV PAID AS CORPS                
**       BAS   RE,GETEL                                                         
**       BNE   XIT                                                              
*  DOESN'T WORK WITH TAX REFUND                                                 
         MVC   LASTSSN,CRPSSN      SAVE GOOD SSN                                
         SPACE 1                                                                
         USING TACWD,R6                                                         
         MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TACWELQ      GET CHECK WITHHOLDING EL FOR CA              
         GOTO1 GETL,DMCB,(3,TIFUNIT)                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,TGELEM                                                        
         L     R0,SVSTAX                                                        
         A     R0,TACWTAX          ACCUMULATE TAX                               
         ST    R0,SVSTAX                                                        
         SPACE 1                                                                
         AP    CKCOUNT,=P'1'                                                    
         CP    CKCOUNT,TRALIMIT                                                 
         BH    XIT                                                              
         BAS   RE,TRACEINP                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PRINTS OUT THE FORM                                      
         SPACE 3                                                                
DOFORM   NTR1                                                                   
         LA    R2,P                                                             
         L     R1,SVSTAX           R1=TAX AMOUNT                                
         XR    R0,R0                                                            
*                                                                               
         CVD   R1,DUB              USING PACK DECIMAL CUZ BINARY TOO            
         MP    DUB,=PL3'1000'      SMALL TO HANDLE SOME AMOUNTS NOW             
         DP    DUB,=PL1'7'                                                      
         SRP   DUB(7),63,5         ROUND                                        
         ZAP   PCKTAX,DUB(7)                                                    
         CVB   R1,PCKTAX                                                        
*                                                                               
*        M     R0,=F'1000'                                                      
*        D     R0,=F'7'            / TAX RATE                                   
*        XR    R0,R0                                                            
*        D     R0,=F'5'            ROUND IT                                     
*        LTR   R1,R1                                                            
*        BM    *+8                                                              
*        AH    R1,=H'1'                                                         
*        SRA   R1,1                R1=GROSS AMOUNT                              
*                                                                               
         ST    R1,SVGROSS                                                       
         A     R1,TOTGROSS         ACCUMULATE TOTALS                            
         ST    R1,TOTGROSS                                                      
         L     R1,TOTSTAX                                                       
         A     R1,SVSTAX                                                        
         ST    R1,TOTSTAX                                                       
         SPACE                                                                  
         OC    TPNAME,TPNAME                                                    
         BNZ   DOF10                                                            
         MVC   TGEMP,TGTPEMP       SET TGEMP TO TP                              
         BAS   RE,GTEMP            GET EMPLOYER INFO                            
         MVC   TPNAME,MYWORK+8                                                  
         MVC   TPFDID,SPACES                                                    
         MVC   TPFDID(10),DUB                                                   
         MVC   TPADD1,BLOCK                                                     
         MVC   TPADD2,BLOCK+30                                                  
*                                                                               
         CLC   MYYEAR(4),=C'2004'                                               
         BL    DOF3                                                             
         MVC   TPNAME,=CL36'TALENT PARTNERS COM SERV, LLC'                      
*                                                                               
DOF3     CLC   TIFEMP,TGTPEMP      IF EMPLOYER IS NOT TP                        
         BE    DOF5                                                             
         MVC   TGEMP,TIFEMP        SET TGEMP TO SPECIFIED EMPLOYER              
         BAS   RE,GTEMP            GET EMPLOYER INFO                            
         MVC   EMPNAME,MYWORK+8                                                 
         MVC   EMPFDID,SPACES                                                   
         MVC   EMPFDID(10),DUB                                                  
         MVC   EMPADD1,BLOCK                                                    
         MVC   EMPADD2,BLOCK+30                                                 
         B     *+10                                                             
DOF5     MVC   EMPNAME(EMPLNQ),TPNAME  ELSE JUST COPY TP INFO                   
         SPACE                                                                  
DOF10    BAS   RE,GETW4            GET W4 INFO                                  
         BAS   RE,GETW4            GET W4 INFO                                  
         TM    MYOPTS,ONOFORM      IF NOT PRINTING ON FORM                      
         BZ    DOF20                                                            
         USING HEADD,R2                                                         
         MVC   HDSSN,LASTSSN                                                    
         MVC   HDNAME,W4NAME                                                    
         EDIT  SVSTAX,(14,HDWITH),2                                             
         EDIT  SVGROSS,(14,HDGROSS),2                                           
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE                                                                  
         USING PRINTD,R2                                                        
DOF20    TM    MYBYTE,X'80'        TEST NEED TO PRINT LINE-UP FORMS             
         BZ    *+12                                                             
         BAS   RE,LINEUP                                                        
         NI    MYBYTE,X'FF'-X'80'  TURN OFF FLAG                                
         SPACE                                                                  
         MVI   FORCEHED,C'N'                                                    
         MVI   LINE,0                                                           
         BAS   RE,SPLAT2                                                        
         BAS   RE,SPLAT                                                         
         MVC   PRW4NAME(4),MYYEAR                                               
         BAS   RE,SPLAT5                                                        
         MVC   PRW4NAME(L'W4NAME),W4NAME                                        
         MVC   PREMNAME(L'EMPNAME),EMPNAME                                      
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD(L'W4ADD1),W4ADD1                                         
         MVC   PREMNAME(L'EMPADD1),EMPADD1                                      
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD(L'W4ADD2),W4ADD2                                         
         MVC   PREMNAME(L'EMPADD2),EMPADD2                                      
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD(L'W4ADD3),W4ADD3                                         
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD(L'W4ADD4),W4ADD4                                         
         MVC   PRAREA,=C'312'                                                   
         MVC   PRPHONE,=C'923-7900'                                             
         BAS   RE,SPLAT2                                                        
         MVC   PRW4SSN,LASTSSN                                                  
         MVC   PREMFEIN,EMPFDID                                                 
         BAS   RE,SPLAT2                                                        
         CLC   TIFEMP,TGTPEMP      IF EMPLOYER IS TP                            
         BNE   DOF30                                                            
         BAS   RE,SPLAT5           SKIP PART 3                                  
         B     DOF35                                                            
DOF30    MVC   PRW4ADD(L'TPNAME),TPNAME                                         
         MVC   PRAREA,=C'312'                                                   
         MVC   PRPHONE,=C'923-7900'                                             
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD(L'TPADD1),TPADD1                                         
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD(L'TPADD2),TPADD2                                         
         BAS   RE,SPLAT                                                         
         MVC   PREMFEIN,TPFDID                                                  
         BAS   RE,SPLAT2                                                        
DOF35    MVC   PRTYPEX,EXES                                                     
         MVC   PRTYPE,=C'ENTERTAINMENT'                                         
         BAS   RE,SPLAT2                                                        
         BAS   RE,SPLAT                                                         
         EDIT  SVGROSS,(14,PRAMOUNT),2                                          
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         EDIT  SVSTAX,(14,PRAMOUNT),2                                           
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*=====================================================================          
*        FRSTLIN - FIRST LINE OF FILE, WITHHOLDING AGENT                        
*=====================================================================          
FRSTLIN  NTR1                                                                   
*                                                                               
         TM    MYOPTS,OMMDISK      CREATING MMREF DISK                          
         BZ    FRSTLINX                                                         
         LA    R3,TAPEREC                                                       
         USING TRECD,R3                                                         
         MVI   0(R3),C' '          SPACE OUT RECORD                             
         MVC   1(255,R3),0(R3)                                                  
         MVC   256(56,R3),MYSPACES                                              
*                                                                               
         MVC   TIDNUM,=C'200467282'                                             
         MVC   TIDTYPE,=CL6'FEIN'                                               
         MVC   TOTHNM1,=CL35'TALENT PARTNERS'                                   
         MVC   TADDR1,=CL32'111 W. JACKSON BLVD., SUITE 1525'                   
         MVC   TCITY,=CL17'CHICAGO'                                             
         MVC   TSTATE,=C'IL '                                                   
         MVC   TZIP,=C'60604'                                                   
         MVC   TCTRY(3),=C'USA'                                                 
         MVC   TQTR,QTRNUM                                                      
         OI    TQTR,X'F0'                                                       
*                                                                               
         MVC   TFRGNTXE,=C'12/31/YYYY'                                          
         MVC   TFRGNTXE+6(4),MYYEAR                                             
*                                                                               
FRSTLINX B     XIT                                                              
         EJECT                                                                  
*=====================================================================          
*        DOTAPE - SPECIAL FILE WHEN # OF EMPLOYEE >=250                         
*=====================================================================          
DOTAPE   NTR1                                                                   
         SR    RF,RF                                                            
         ICM   RF,3,TAPECNT        BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         STCM  RF,3,TAPECNT                                                     
*                                                                               
         TM    MYOPTS,OMMDISK      CREATING MMREF DISK                          
         BZ    DOTAPEX                                                          
         LA    R3,TAPEREC                                                       
         USING TRECD,R3                                                         
         MVI   0(R3),C' '          SPACE OUT RECORD                             
         MVC   1(255,R3),0(R3)                                                  
         MVC   256(56,R3),MYSPACES                                              
*                                                                               
         EDIT  TAPECNT,TPYNUM,0,FILL=0                                          
         MVC   TTAXYR,MYYEAR                                                    
         MVC   TIDNUM,LASTSSN                                                   
         MVC   TIDTYPE,=CL6'FEIN'                                               
         MVC   TOTHNM1(L'W4NAME),W4NAME                                         
         MVC   TADDR1,W4ADD1                                                    
         MVC   TADDR2,W4ADD2                                                    
         MVC   TCITY,W4CITY                                                     
         MVC   TSTATE,W4ST                                                      
         MVC   TZIP,W4ZIP                                                       
         MVC   TZIPX,W4ZIP+6                                                    
         MVC   TCTRY(3),=C'USA'                                                 
         MVC   TQTR,QTRNUM                                                      
         OI    TQTR,X'F0'                                                       
         MVC   TINCTYPE(13),=C'ENTERTAINMENT'                                   
         EDIT  SVGROSS,TINCSUBW,2,FILL=0                                        
         EDIT  SVSTAX,TAMTWITH,2,FILL=0                                         
         MVC   TAMTWOTH,SPACES                                                  
         MVC   TPRIPAY,SPACES                                                   
         MVC   TFRGNCRD,SPACES                                                  
         MVC   TFRGNBAL,SPACES                                                  
         MVC   TFRGNOVR,SPACES                                                  
         MVC   TFRGNCRN,SPACES                                                  
         MVC   TFRGNREF,SPACES                                                  
         MVC   TFRGNTXE,=C'12/31/YYYY'                                          
         MVC   TFRGNTXE+6(4),MYYEAR                                             
*                                                                               
DOTAPE99 BAS   RE,PUTF                                                          
DOTAPEX  B     XIT                                                              
         EJECT                                                                  
*              PRINT OUT HEADINGS WHEN NOT PRINTING ON FORM                     
         SPACE                                                                  
         USING HEADD,R2                                                         
HEADING  NTR1                                                                   
         LA    R2,P                                                             
         MVC   HDSSN,=CL9'CORP ID#'                                             
         MVC   HDNAME,=CL32'CORP NAME'                                          
         MVC   HDWITH+2(12),=C'AMT WITHHELD'                                    
         MVC   HDGROSS+9(5),=C'GROSS'                                           
         BAS   RE,SPLAT                                                         
         MVC   HDSSN,=CL9'--------'                                             
         MVC   HDNAME,=CL32'---------'                                          
         MVC   HDWITH+2(12),=C'------------'                                    
         MVC   HDGROSS+9(5),=C'-----'                                           
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS EMPLOYER INFO                                       
         SPACE                                                                  
GTEMP    NTR1                                                                   
         XC    MYWORK(8),MYWORK    INIT DUMMY AREA FOR NAME OF L'45             
         MVI   MYWORK,53           SET FIELD LENGTH                             
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A8',TGEMP),MYWORK NAME - MYWORK+8         
         SPACE                                                                  
         USING TATID,R6            GET FEDERAL ID IN DUB(10)                    
         MVI   ELCODE,TATIELQ                                                   
         MVI   FULL,TATITYUN       ELEMENT TYPE                                 
         MVC   FULL+1(3),=C'FD '                                                
         GOTO1 GETL,DMCB,(4,FULL)                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,TGELEM                                                        
         MVC   DUB(10),TATIID                                                   
         SPACE                                                                  
         MVC   BLOCK(132),SPACES                                                
         L     R6,AIO              GET ADDRESS                                  
         USING TAADD,R6                                                         
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,TAADADD                                                       
         ZIC   R2,TAADLNES         N'ADDRESS LINES                              
         LA    R1,BLOCK                                                         
GETEM1   MVC   0(30,R1),0(R3)      SAVE IN BLOCK                                
         LA    R1,30(R1)                                                        
         LA    R3,30(R3)                                                        
         BCT   R2,GETEM1                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS W4 RECORD INFO                                      
         SPACE                                                                  
GETW4    NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',LASTSSN) GET W4 RECORD                
         BE    *+6                                                              
         DC    H'0'                DIE IF NOT FOUND                             
         L     R6,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R6                                                         
         MVC   W4NAME,TAW4CRPN     SAVE CORP NAME                               
*                                                                               
         LA    RF,W4NAME                                                        
         LA    RE,L'W4NAME-3                                                    
GTW405   CLC   =C'FSO',0(RF)                                                    
         BE    GTW407                                                           
         CLC   =C'DBA',0(RF)                                                    
         BE    GTW407                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,GTW405                                                        
         B     GTW408                                                           
*                                                                               
GTW407   MVC   0(3,RF),=C'   '                                                  
         LA    RF,1(RF)                                                         
         BCT   RE,GTW407                                                        
*                                                                               
*                                                                               
GTW408   XC    W4ADD1,W4ADD1                                                    
         XC    W4ADD2,W4ADD2                                                    
         XC    W4ADD3,W4ADD3                                                    
         XC    W4ADD4,W4ADD4                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAA2D,R6                                                         
         LA    R2,W4ADD1                                                        
         MVC   0(L'TAA2ADD1,R2),TAA2ADD1  SAVE ADDRESS                          
*                                                                               
         LA    R2,W4ADD2                                                        
         MVC   W4ADD2,MYSPACES                                                  
         CLC   TAA2ADD2,MYSPACES                                                
         BNH   GTW410                                                           
         MVC   0(L'TAA2ADD2,R2),TAA2ADD2                                        
*                                                                               
         LA    R2,W4ADD3                                                        
         MVC   W4ADD3,MYSPACES                                                  
         CLC   TAA2ADD3,MYSPACES                                                
         BNH   GTW410                                                           
         MVC   0(L'TAA2ADD3,R2),TAA2ADD3                                        
*                                                                               
         LA    R2,W4ADD4                                                        
GTW410   MVC   0(L'W4ADD4,R2),MYSPACES                                          
*                                                                               
         MVC   W4CITY,TAA2CITY                                                  
         LA    RF,L'W4CITY                                                      
         LA    RE,W4CITY                                                        
GTW420   CLI   0(RE),C'A'                                                       
         BNL   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,GTW420                                                        
*                                                                               
         MVC   W4ST,TAA2ST                                                      
         MVC   W4ZIP,TAA2ZIP                                                    
*                                                                               
         OC    W4CITY(L'W4CITY+L'W4ST+L'W4ZIP),MYSPACES                         
*                                                                               
         XC    WORK(39),WORK                                                    
         MVC   WORK(25),TAA2CITY                                                
         MVC   WORK+26(2),TAA2ST                                                
         MVC   WORK+29(10),TAA2ZIP                                              
         GOTO1 SQUASHER,DMCB,WORK,39                                            
         MVC   0(L'W4ADD4,R2),WORK                                              
         B     XIT                                                              
         EJECT                                                                  
*              PRINT LINEUP PATTERNS                                            
         SPACE 3                                                                
         USING PRINTD,R2                                                        
LINEUP   NTR1                                                                   
         MVI   FORCEHED,C'N'                                                    
         MVI   LINE,0                                                           
         L     R1,=F'111111111'                                                 
         LA    R4,6                PRINT 6 PATTERNS                             
         LA    R2,P                                                             
LINEU2   MVI   FORCEHED,C'N'                                                    
         MVI   LINE,0                                                           
         BAS   RE,SPLAT2                                                        
         BAS   RE,SPLAT                                                         
         MVC   PRW4NAME(4),MYYEAR                                               
         BAS   RE,SPLAT5                                                        
         MVC   PRW4NAME,EXES                                                    
         MVC   PREMNAME,EXES                                                    
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD,EXES                                                     
         MVC   PREMNAME,EXES                                                    
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD,EXES                                                     
         MVC   PREMNAME,EXES                                                    
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD,EXES                                                     
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD,EXES                                                     
         MVC   PRAREA,EXES                                                      
         MVC   PRPHONE,EXES                                                     
         BAS   RE,SPLAT2                                                        
         MVC   PRW4SSN,EXES                                                     
         MVC   PREMFEIN,EXES                                                    
         BAS   RE,SPLAT2                                                        
         MVC   PRW4ADD,EXES                                                     
         MVC   PRAREA,EXES                                                      
         MVC   PRPHONE,EXES                                                     
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD,EXES                                                     
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD,EXES                                                     
         BAS   RE,SPLAT                                                         
         MVC   PRW4ADD,EXES                                                     
         MVC   PREMFEIN,EXES                                                    
         BAS   RE,SPLAT2                                                        
         MVC   PRTYPEX,EXES                                                     
         MVC   PRTYPE,EXES                                                      
         BAS   RE,SPLAT2                                                        
         BAS   RE,SPLAT                                                         
         EDIT  (R1),(14,PRAMOUNT),2                                             
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         EDIT  (R1),(14,PRAMOUNT),2                                             
         BAS   RE,SPLAT                                                         
         A     R1,=F'111111111'                                                 
         BCT   R4,LINEU2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              TRACING ROUTINES                                                 
         SPACE 3                                                                
TRACEINP NTR1                                                                   
         L     R6,TIAREC                                                        
         MVC   RECTYPE,=CL16'CHECK'                                             
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
         SPACE 1                                                                
TRACEREC NTR1                                                                   
         MVI   P,X'BF'                                                          
         MVC   P+1(131),P                                                       
         MVC   P+60(16),RECTYPE                                                 
         BAS   RE,SPLAT                                                         
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
TRACERC2 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         ZIC   R2,1(R6)                                                         
         BAS   RE,TRACEL                                                        
         B     TRACERC2                                                         
         SPACE 1                                                                
TRACEL   NTR1                                                                   
*                                  R2=LENGTH, R6=ADDRESS                        
         MVC   MYP,MYSPACES                                                     
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R6)                                                     
         OC    MYP,MYSPACES                                                     
         GOTO1 HEXOUT,DMCB,(R6),MYP3,132,=C'SEP'                                
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP2(0),MYP3                                                     
         MVC   MYP3,MYSPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP3(0),MYP4                                                     
         MVC   MYP4,MYSPACES                                                    
         MVC   P,MYP                                                            
         MVC   P2,MYP2                                                          
         MVC   P3,MYP3                                                          
         MVC   P4,MYP4                                                          
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
MYCLEAR  NTR1                                                                   
         MVI   MYP,C' '                                                         
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP2,MYP                                                         
         MVC   MYP3,MYP                                                         
         MVC   MYP4,MYP                                                         
         MVC   MYSPACES,MYP                                                     
         B     XIT                                                              
         SPACE 1                                                                
SPLAT2   NTR1                                                                   
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
SPLAT5   NTR1                                                                   
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R5)                                                  
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
OPENF    NTR1                                                                   
         LA    R2,CATAPE                                                        
         TM    WHEN,X'20'          IF SOON                                      
         BZ    *+8                                                              
         LA    R2,TADOWN                                                        
         OPEN  ((2),(OUTPUT))                                                   
         B     XIT                                                              
*                                                                               
CLOSEF   NTR1                                                                   
         LA    R2,CATAPE                                                        
         TM    WHEN,X'20'          IF SOON                                      
         BZ    *+8                                                              
         LA    R2,TADOWN                                                        
         CLOSE ((2))                                                            
         B     XIT                                                              
*                                                                               
*                                                                               
PUTF     NTR1                                                                   
         LA    R1,CATAPE                                                        
         TM    WHEN,X'20'          IF SOON                                      
         BZ    *+8                                                              
         LA    R1,TADOWN                                                        
         LA    R2,TAPEREC                                                       
         PUT   (1),(2)                                                          
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO PUT FIELD INTO DOWNLOAD                               
*---------------------------------------------------------------------          
FLDDOWN  NTR1                                                                   
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         L     R4,0(R1)            ADDRESS OF FIELD                             
         LA    R4,0(R4)                                                         
         ZIC   R2,0(R1)            LENGTH OF FIELD                              
         LTR   R2,R2                                                            
         BZ    FD010                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R4)                                                 
FD010    GOTO1 =V(DLFLD),DLCBD                                                  
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO PUT NUMBER INTO DOWNLOAD                              
*---------------------------------------------------------------------          
NUMDOWN  NTR1                                                                   
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBNUM     DATA TYPE IS NUMERIC                         
         L     R4,0(R1)            ADDRESS OF FIELD                             
         LA    R4,0(R4)                                                         
         ZIC   R2,0(R1)            LENGTH OF FIELD                              
         LTR   R2,R2                                                            
         BZ    FD010                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R4)                                                 
ND010    GOTO1 =V(DLFLD),DLCBD                                                  
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO PUT EOL INTO DOWNLOAD                                 
*---------------------------------------------------------------------          
EOLDOWN  NTR1                                                                   
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
INITDWN  NTR1                                                                   
         LA    R3,DLBLOCK                                                       
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,SPLATDWN         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
SPLATDWN NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R5)                                                  
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO OUTPUT HEADLINES TO DOWNLOAD                          
*---------------------------------------------------------------------          
         USING TRECD,R2                                                         
DOWNHEAD NTR1                                                                   
         LA    R2,TAPEREC                                                       
         GOTO1 =A(FLDDOWN),DMCB,(9,=C'Payee Num')                               
         GOTO1 (RF),DMCB,(8,=C'Tax Year')                                       
         GOTO1 (RF),DMCB,(9,=C'ID Number')                                      
         GOTO1 (RF),DMCB,(7,=C'ID Type')                                        
         GOTO1 (RF),DMCB,(10,=C'First Name')                                    
         GOTO1 (RF),DMCB,(2,=C'MI')                                             
         GOTO1 (RF),DMCB,(9,=C'Last Name')                                      
         GOTO1 (RF),DMCB,(17,=C'Other Name Line 1')                             
         GOTO1 (RF),DMCB,(17,=C'Other Name Line 2')                             
         GOTO1 (RF),DMCB,(14,=C'Address Line 1')                                
         GOTO1 (RF),DMCB,(14,=C'Address Line 2')                                
         GOTO1 (RF),DMCB,(4,=C'City')                                           
         GOTO1 (RF),DMCB,(5,=C'State')                                          
         GOTO1 (RF),DMCB,(5,=C'Zip 5')                                          
         GOTO1 (RF),DMCB,(5,=C'Zip 4')                                          
         GOTO1 (RF),DMCB,(7,=C'Country')                                        
         GOTO1 (RF),DMCB,(16,=C'Domestic Quarter')                              
         GOTO1 (RF),DMCB,(11,=C'Income Type')                                   
         GOTO1 (RF),DMCB,(29,=C'Income Subject to Withholding')                 
         GOTO1 (RF),DMCB,(15,=C'Amount Withheld')                               
         GOTO1 (RF),DMCB,(33,=C'Amount Withheld by Another Entity')             
         GOTO1 (RF),DMCB,(14,=C'Prior Payments')                                
         GOTO1 (RF),DMCB,(25,=C'Foreign Prior Year Credit')                     
         GOTO1 (RF),DMCB,(19,=C'Foreign Balance Due')                           
         GOTO1 (RF),DMCB,(19,=C'Foreign Overpayment')                           
         GOTO1 (RF),DMCB,(27,=C'Foreign Credit to Next Year')                   
         GOTO1 (RF),DMCB,(14,=C'Foreign Refund')                                
         GOTO1 (RF),DMCB,(20,=C'Foreign Tax Year End')                          
         BAS   RE,EOLDOWN                                                       
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------          
*              ROUTINE TO OUTPUT TAPE RECORD TO DOWNLOAD                        
*---------------------------------------------------------------------          
         USING TRECD,R2                                                         
DOWNTAPE NTR1                                                                   
         L     R3,0(R1)                                                         
         LA    R2,TAPEREC                                                       
         GOTO1 =A(FLDDOWN),DMCB,(L'TPYNUM,TPYNUM)                               
         GOTO1 (RF),DMCB,(L'TTAXYR,TTAXYR)                                      
         GOTO1 (RF),DMCB,(L'TIDNUM,TIDNUM)                                      
         GOTO1 (RF),DMCB,(L'TIDTYPE,TIDTYPE)                                    
         GOTO1 (RF),DMCB,(L'TFRSTNM,TFRSTNM)                                    
         GOTO1 (RF),DMCB,(L'TMIDI,TMIDI)                                        
         GOTO1 (RF),DMCB,(L'TLASTNM,TLASTNM)                                    
         GOTO1 (RF),DMCB,(L'TOTHNM1,TOTHNM1)                                    
         GOTO1 (RF),DMCB,(L'TOTHNM2,TOTHNM2)                                    
         GOTO1 (RF),DMCB,(L'TADDR1,TADDR1)                                      
         GOTO1 (RF),DMCB,(L'TADDR2,TADDR2)                                      
         GOTO1 (RF),DMCB,(L'TCITY,TCITY)                                        
         GOTO1 (RF),DMCB,(L'TSTATE,TSTATE)                                      
         GOTO1 (RF),DMCB,(L'TZIP,TZIP)                                          
         GOTO1 (RF),DMCB,(L'TZIPX,TZIPX)                                        
         GOTO1 (RF),DMCB,(L'TCTRY,TCTRY)                                        
         GOTO1 (RF),DMCB,(L'TQTR,TQTR)                                          
         GOTO1 (RF),DMCB,(L'TINCTYPE,TINCTYPE)                                  
*                                                                               
         LTR   R3,R3                                                            
         BZ    DT100                                                            
         GOTO1 =A(FLDDOWN),DMCB,(1,=C' ')                                       
         GOTO1 (RF),DMCB,(1,=C' ')                                              
         B     DT200                                                            
*                                                                               
DT100    GOTO1 =A(NUMDOWN),DMCB,(L'TINCSUBW,TINCSUBW)                           
         GOTO1 (RF),DMCB,(L'TAMTWITH,TAMTWITH)                                  
*&&DO                                                                           
         GOTO1 (RF),DMCB,(L'TAMTWOTH,TAMTWOTH)                                  
         GOTO1 (RF),DMCB,(L'TPRIPAY,TPRIPAY)                                    
         GOTO1 (RF),DMCB,(L'TFRGNCRD,TFRGNCRD)                                  
         GOTO1 (RF),DMCB,(L'TFRGNBAL,TFRGNBAL)                                  
         GOTO1 (RF),DMCB,(L'TFRGNOVR,TFRGNOVR)                                  
         GOTO1 (RF),DMCB,(L'TFRGNCRN,TFRGNCRN)                                  
         GOTO1 (RF),DMCB,(L'TFRGNREF,TFRGNREF)                                  
*&&                                                                             
DT200    GOTO1 =A(FLDDOWN),DMCB,(1,=C' ')                                       
         GOTO1 (RF),DMCB,(1,=C' ')                                              
         GOTO1 (RF),DMCB,(1,=C' ')                                              
         GOTO1 (RF),DMCB,(1,=C' ')                                              
         GOTO1 (RF),DMCB,(1,=C' ')                                              
         GOTO1 (RF),DMCB,(1,=C' ')                                              
         GOTO1 (RF),DMCB,(1,=C' ')                                              
         GOTO1 (RF),DMCB,(1,=C' ')                                              
*                                                                               
*        GOTO1 =A(FLDDOWN),DMCB,(L'TFRGNTXE,TFRGNTXE)                           
         BAS   RE,EOLDOWN                                                       
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
*                                  COUNTS                                       
EXES     DC    120C'X'                                                          
*                                                                               
         ENTRY CATAPE                                                           
CATAPE   DCB   DDNAME=CATAPE,DSORG=PS,MACRF=(GM,PM),                   X        
               RECFM=FB,LRECL=383,BUFNO=2,BLKSIZE=3830,EODAD=NOMORE             
*                                                                               
TADOWN   DCB   DDNAME=TADOWN,DSORG=PS,MACRF=(GM,PM),                   X        
               RECFM=FB,LRECL=383,BUFNO=2,BLKSIZE=3830,EODAD=NOMORE             
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*---------------------------------------------------------------------          
NEWPRTQ  NTR1  BASE=*,LABEL=*                                                   
         L     R2,ALOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 ALOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BO    NPQ010                                                           
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
*                                                                               
NPQ010   XC    SPECS,SPECS         CLEAR SPECS                                  
         XC    HEADHOOK,HEADHOOK   AND HEADLINE HOOK                            
*                                                                               
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         LA    RE,132                                                           
         ST    RE,BOXWIDTH         SET LENGTH OF PRINT LINE                     
*                                                                               
         L     RF,AMASTD                                                        
         USING MASTD,RF                                                         
         L     R2,MYAREMOT                                                      
         USING REMOTED,R2                                                       
         TM    WHEN,X'20'          SOON?                                        
         BZ    NPQ050                                                           
         XC    MCREMPQK,MCREMPQK                                                
         B     NPQ060                                                           
*                                                                               
NPQ050   MVC   REMOTABF,MCVPQBUF   OVERNIGHT                                    
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         OI    REMOTTYP,X'10'      DOWNLOAD                                     
         L     RF,MCVLOGO          DOWN/SQL - PATCH LOGOS NOT TO PRINT          
         MVC   0(2,RF),=X'07FE'                                                 
         LA    RF,MCREMOTE                                                      
*                                                                               
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'TCA'                                                 
NPQ060   MVC   REMOTKEY(11),=CL11' '                                            
         MVC   REMOTSYS(6),=C'CACORP'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*              TASYSCATS BELOW                                                  
*              TASYSEQUS                                                        
*              TASYSDSECT                                                       
         PRINT OFF                                                              
       ++INCLUDE TASYSCATS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
         PRINT ON                                                               
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MYAREMOT DS    A                                                                
AMASTD   DS    A                                                                
ALOGOC   DS    A                                                                
ALOGO    DS    A                                                                
MYRERUN  DS    CL1                                                              
LASTSSN  DS    CL9                 LAST GOOD SSN TO PRINT OUT                   
CRPSSN   DS    CL9                                                              
MYWORK   DS    CL64                                                             
MYYEAR   DS    CL8                 DISPLAYABLE YEAR                             
MYSPACES DS    CL132                                                            
MYP      DS    CL132                                                            
MYP2     DS    CL132                                                            
MYP3     DS    CL132                                                            
MYP4     DS    CL132                                                            
QTRNUM   DS    X                                                                
SVGROSS  DS    F                                                                
SVSTAX   DS    F                                                                
TOTGROSS DS    F                                                                
TOTSTAX  DS    F                                                                
*                                  OPTIONS                                      
PCKTAX   DS    PL8                                                              
*                                                                               
MYOPTS   DS    X                                                                
ONOFORM  EQU   X'80'               DON'T PRINT ON FORM                          
OMMDISK  EQU   X'40'               MMREF DISK                                   
         SPACE 1                                                                
RECTYPE  DS    CL16                                                             
         SPACE 1                                                                
EMPNAME  DS    CL36                MUST BE SAME AS TP INFO BLOCK                
EMPADD1  DS    CL30                                                             
EMPADD2  DS    CL30                                                             
EMPFDID  DS    CL14                                                             
EMPLNQ   EQU   *-EMPNAME                                                        
TPNAME   DS    CL36                MUST BE SAME AS EMP INFO BLOCK               
TPADD1   DS    CL30                                                             
TPADD2   DS    CL30                                                             
TPFDID   DS    CL14                                                             
W4NAME   DS    CL32                                                             
W4ADD1   DS    CL37                                                             
W4ADD2   DS    CL37                                                             
W4ADD3   DS    CL37                                                             
W4ADD4   DS    CL37                                                             
W4CITY   DS    CL17                                                             
W4ST     DS    CL2                                                              
W4ZIP    DS    CL10                                                             
W4TYPE   DS    XL1                                                              
MYBYTE   DS    CL1                 X'80' = NEED TO PRINT LINEUP PATTERN         
*                                  X'40' = PRINT PENDING                        
DLBLOCK  DS    CL(DLCBXLX)                                                      
*                                                                               
TAPECNT  DS    XL2                 NUMBER OF PAYEES ON TAPE                     
         DS    0D                                                               
TAPEREC  DS    312C                                                             
         EJECT                                                                  
*======================================================================         
TRECD    DSECT                                                                  
TPYNUM   DS    CL5                 SEQUENTIAL NUMBER OF THE PAYEE               
TTAXYR   DS    CL4                 TAX YEAR                                     
TIDNUM   DS    CL9                 TAXPAYER ID NUMBER                           
TIDTYPE  DS    CL6                 ID TYPE                                      
TFRSTNM  DS    CL11                FIRST NAME                                   
TMIDI    DS    CL1                 MIDDLE INITIAL                               
TLASTNM  DS    CL17                LAST NAME                                    
TOTHNM1  DS    CL35                CORP NAME 1                                  
TOTHNM2  DS    CL35                CORP NAME 2                                  
TADDR1   DS    CL30                ADDRESS LINE 1                               
TADDR2   DS    CL30                ADDRESS LINE 2                               
TCITY    DS    CL17                CITY                                         
TSTATE   DS    CL2                 STATE                                        
TZIP     DS    CL5                 ZIP                                          
TZIPX    DS    CL4                 ZIP EXTENSION                                
TCTRY    DS    CL22                COUNTRY (USA)                                
TQTR     DS    CL1                 QUARTER                                      
TINCTYPE DS    CL22                INCOME TYPE                                  
TINCSUBW DS    CL13                INCOME SUBJECT WITHHOLDING                   
TAMTWITH DS    CL13                AMOUNT CA TAX WITHHELD                       
TAMTWOTH DS    CL13                AMOUNT TAX WITHHELD OTHER                    
TPRIPAY  DS    CL13                PRIOR PAYMENTS                               
TFRGNCRD DS    CL13                FOREIGN PRIOR YEAR CREDIT                    
TFRGNBAL DS    CL13                FOREIGN BALANCE DUE                          
TFRGNOVR DS    CL13                FOREIGN OVERPAYMENT                          
TFRGNCRN DS    CL13                FOREIGN CREDIT TO NEXT YEAR                  
TFRGNREF DS    CL13                FOREIGN REFUND                               
TFRGNTXE DS    CL10                FOREIGN TAX YEAR END                         
TRECLNQ  EQU   *-TRECD                                                          
         EJECT                                                                  
*              DSECT TO COVER PRINT LINES                                       
         SPACE 3                                                                
PRINTD   DSECT                                                                  
         DS    CL5                                                              
PRW4NAME DS    CL33                                                             
         DS    CL6                                                              
PREMNAME DS    CL36                                                             
         ORG   PRW4NAME                                                         
PRW4ADD  DS    CL37                                                             
         DS    CL2                                                              
PRAREA   DS    CL3                                                              
         DS    CL2                                                              
PRPHONE  DS    CL8                                                              
         ORG   PRW4NAME                                                         
PRW4SSN  DS    CL9                                                              
         DS    CL30                                                             
PREMFEIN DS    CL14                EMPLOYER FEIN                                
         ORG   PRW4NAME                                                         
         DS    CL55                                                             
PRTYPEX  DS    C                   PUT "X" HERE                                 
         DS    CL5                                                              
PRTYPE   DS    CL13                PUT "ENTERTAINMENT" HERE                     
         ORG   PRW4NAME                                                         
         DS    CL60                                                             
PRAMOUNT DS    CL14                                                             
         EJECT                                                                  
*              DSECT TO COVER HEADING LINES WHEN NOT PRINTING ON FORM           
         SPACE 3                                                                
HEADD    DSECT                                                                  
         DS    C                                                                
HDSSN    DS    CL9                 CORP'S SSN                                   
         DS    C                                                                
HDNAME   DS    CL32                CORP NAME                                    
         DS    C                                                                
HDWITH   DS    CL14                AMOUNT WITHHELD                              
         DS    C                                                                
HDGROSS  DS    CL14                GROSS SUBJECT TO WITHHOLDING                 
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*DDMASTD                                                                        
*DDTWADCONS                                                                     
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDLOGOD                                                        
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPCED                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018TAREP4E   11/12/15'                                      
         END                                                                    
