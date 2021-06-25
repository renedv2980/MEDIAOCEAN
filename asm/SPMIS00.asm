*          DATA SET SPMIS00    AT LEVEL 079 AS OF 11/19/19                      
*PHASE T20B00C                                                                  
*INCLUDE MEDGET                                                                 
*INCLUDE EQVRD                                                                  
*INCLUDE DPTRD                                                                  
         TITLE 'SPMIS00 - SPOTPAK MIS - HEADLINE EDITS'                         
*=====================================================================*         
* DDMMMYY LVL WHO  DESCRIPTION                                        *         
* ------- --- ---- -------------------------------------------------- *         
* 13SEP95 11  SPRI OPTION IDR, 2 BYTE ERRORS                          *         
* 22AUG96 19  SPRI OPTION LOCKIN                                      *         
* 03NOV98     MHER SUPPORT NET DOLLARS FOR CARAT                      *         
* 12MAY99     MHER ALLOW TRD=Y SO WIM CAN COMBINE CASH/TRD PRDS       *         
* 20SEP99     MHER DEFAULT TO TRD=Y                                   *         
* 02FEB00 10  MZEI TRANSFER FROM SUPERDESK                            *         
* 05APR01     MHER PURPOSE CODES                                      *         
* 04MAY01     MHER MARKET AND STATION LOCKIN                          *         
*                  BUILD CANADIAN NETWORK LIST                        *         
* 11JUN01     MHER CALL NEW OFFICER                                   *         
* 03JUL02     MHER PF12 DOES RETURN GLOBBER CALL                      *         
* 11JUN03     MHER ADD PFKEYS FOR TOP/NEXT                            *         
* 01JUL03     AWIL GRIDS                                              *         
*   JAN04     MHER 2-DECIMAL RATINGS                                            
* 07SEP06     HWON CORRECTLY SAVE/USE THE 00 PROFILE                            
*   SEP06     MHER LOCKED GOALS                                       *         
* 14NOV06     EJOR 6K BUYS                                            *         
* 05NOV09 69  KWAN D0 PROFILE FOR NO SPILL CONTROL                    *         
* 02NOV16 *72 MHER COMSCORE SUPPORT                                             
* 17NOV16 *73 HWON FIX EST=ALL OPTION TO SUPPORT EXTENDED ESTHDR RECS           
* 18JUL18     MHER SUPPORT FOR CUSTOM COMSCORE DEMOS                  *         
*=====================================================================*         
                                                                                
T20B00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MISTWA2X-GENOLD,T20B00,RR=R8                                     
         ST    R8,RELO                                                          
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         USING T20B00+4096,R8                                                   
         USING GENOLD,RC                                                        
*                                                                               
         L     RA,4(R1)                                                         
         USING T20BFFD,RA                                                       
*                                                                               
         ST    R8,BASER8                                                        
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
* CLEAR WORK AREA                                                               
         LA    RE,12(RC)           * DONT DESTROY REGS JUST SAVED               
         LR    RF,RD                                                            
         SR    RF,RE               GIVES LEN TO CLEAR                           
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         MVCL  RE,R6                                                            
*                                                                               
         ST    R1,VMONPARM         CAREFUL TO SAVE *AFTER* CLEAR                
         LM    R2,R4,0(R1)                                                      
         ST    R2,VTIOB                                                         
         ST    R3,VTWA                                                          
*                                                                               
         MVC   FULL,RELO           PASS RELO VALUE IN FULL                      
         BRAS  RE,INITL            INITIALIZE WORK AREA                         
         B     M2                                                               
*                                                                               
EQXIT    CR    RB,RB               SET CC =                                     
         J     XIT                                                              
NEQXIT   LTR   RB,RB               SET CC NOT =                                 
XIT      XIT1                                                                   
*===============================================================                
* TEST IF THIS IS A TRANSFER OF CONTROL FROM ANOTHER PROGRAM                    
*===============================================================                
         SPACE 1                                                                
M2       CLI   PFKEY,12                                                         
         BNE   M4                                                               
         OC    SVXFRCTL,SVXFRCTL   IS THERE SOMEWHERE TO GO BACK                
         BZ    M4                                                               
         BRAS  RE,SETRTRN                                                       
         B     EXIT                                                             
*                                                                               
M4       BRAS  RE,GETGLOB                                                       
*                                                                               
         LA    R9,IOAREA                                                        
         B     M6                                                               
RELO     DC    A(0)                                                             
*                                                                               
M6       LA    R2,MISL10H          CLEAR DATA LINES                             
*                                                                               
         NI    MISPTITH+1,X'F3'    NORMAL INTENSITY                             
         NI    MISPAGEH+1,X'D8'    HIGH INTENSITY                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    M10                                                              
         OI    MISPTITH+1,X'0C'    ZERO INTENSITY                               
         OI    MISPAGEH+1,X'2C'    ZERO INTENSITY-PROTECTED                     
         LA    R2,MISL8H                                                        
         XC    8(L'MISL8,R2),8(R2)                                              
         B     M11                                                              
*                                                                               
M10      OC    8(L'MISL10,R2),8(R2)  DATA LINE                                  
         BZ    M20                                                              
         XC    8(L'MISL10,R2),8(R2)                                             
M11      FOUT  (R2)                                                             
*                                                                               
M20      ZIC   RE,0(R2)            FIELD LENGTH                                 
         AR    R2,RE               NEXT FIELD                                   
         LA    R0,MISPFKH                                                       
         CR    R2,R0               LAST TWA FIELD?                              
         BNH   M10                                                              
*&&DO                                                                           
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    M22                                                              
         XC    MISPFK,MISPFK                                                    
         FOUT  MISPFKH                                                          
*&&                                                                             
M22      L     RE,AMISTWA2                                                      
         LH    R0,=Y(BUCKLEN-MISTWA2)                                           
         AR    R0,RE                                                            
         ST    R0,ABUCKLEN                                                      
         LH    R0,=Y(BUCKETS-MISTWA2)                                           
         AR    R0,RE                                                            
         ST    R0,ABUCKETS                                                      
         GOTO1 VCALLOV,DMCB,0,X'D9000A72'  GET MOBILE ADDRESS                   
         MVC   VMOBILE,0(R1)                                                    
         SPACE 1                                                                
* FETCH TWA SAVE AREAS *                                                        
         SPACE 1                                                                
         XC    DMCB(16),DMCB                                                    
         LA    R2,TWAPAGES         NUMBER OF TWA PAGES                          
         L     R3,AMISTWA2         INPUT AREA                                   
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    2 BYTE TERM NO.                              
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),TWASIZE                                               
         LA    R4,1                SET TWA START PAGE                           
GETTWAS  DS    0H                                                               
         STC   R4,DMCB+8           PAGE NO                                      
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(R3)                       
*                                                                               
         LA    R4,1(R4)            NEXT PAGE                                    
         AH    R3,TWASIZE          NEXT INPUT AREA                              
         BCT   R2,GETTWAS                                                       
         EJECT                                                                  
* VALIDATE MEDIA *                                                              
         BRAS  RE,CHKPFKEY         SEE IF PFKEY INPUT                           
*                                                                               
         LA    R2,MISMEDH                                                       
         BRAS  RE,CHKGRIDS         GRID INFO SET PREVIOUSLY?                    
         BNE   XIT                 . NO                                         
*                                                                               
         TM    MISOPTH+4,X'20'     TEST OPTIONS CHANGED                         
         BO    *+8                                                              
         NI    MISMEDH+4,X'DF'     NEED TO REVALIDATE ALL                       
*                                                                               
         MVI   ERRCD,13                                                         
         XC    KEY,KEY                                                          
         TM    4(R2),X'20'         VALID MEDIA?                                 
         BO    M50                                                              
*                                                                               
         NI    MISMKTH+4,X'DF'     INVALIDATE MARKET                            
         NI    MISSTAH+4,X'DF'     INVALIDATE STATION                           
         FOUT  MISMKTNH,SPACES,23                                               
         BAS   RE,M15100           CLEAR LOWER FIELDS                           
         FOUT  MISMEDNH,SPACES,10  IN CASE OF ERROR                             
*                                                                               
         L     R0,ANETLST          CLEAR SVNETLST ON CHANGE OF MED              
         LHI   R1,SVNETLSX-SVNETLST                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
M26      MVI   SVORIGNL,C'N'                                                    
         CLI   5(R2),2                                                          
         BNE   M48                                                              
         CLI   9(R2),C'*'          TEST ORIGINAL BUY REQUEST                    
         BE    *+12                                                             
         CLI   9(R2),C'X'          SFSTEREO                                     
         BNE   M48                                                              
         MVI   SVORIGNL,C'Y'                                                    
         B     M49                                                              
*                                                                               
M48      CLI   5(R2),1                                                          
         BE    *+18                                                             
         CLC   =C'-NS',MISMED+1                                                 
         BNE   MISERR                                                           
         OI    SVOPTS,X'80'        NO SPILL                                     
*                                                                               
M49      GOTO1 =V(MEDGET),DMCB,(MISMED,AGYALPHA),VDATAMGR,WORK,RR=RELO          
*                                                                               
         CLI   DMCB+8,X'FF'        INVALID MEDIA?                               
         BE    MISERR                                                           
*                                                                               
         MVC   SAVBKEY(1),WORK     AGY-MEDIA BYTE                               
         MVC   MISMEDN,WORK+1      MEDIA EXPANSION                              
         CLI   SVORIGNL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   MISMEDN(10),=CL10'*ORIGINAL*'                                    
         SPACE 1                                                                
* AND NOW A LITTLE MORE CODE TO READ THE AGENCY HEADER *                        
         SPACE 1                                                                
M50      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),AGYALPHA                                                
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         USING DAGYHDR,R9                                                       
         MVC   SVAGPRF7,AGYPROF+7  SAVE US/CANAD FLAG                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SAVBKEY    AGY-MEDIA BYTE                               
         OI    4(R2),X'20'                                                      
         SPACE 1                                                                
*=====================================================*                         
* INITIALIZE SECRET                                   *                         
*=====================================================*                         
         SPACE 1                                                                
         OC    T20BFFD+4(2),T20BFFD+4   TEST ON NEW SECURITY                    
         BNZ   *+14                                                             
         OC    T20BFFD+6(2),T20BFFD+6   OR HAVE LIMIT ACCESS                    
         BZ    M51                                                              
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         LR    R0,RA                                                            
         AHI   R0,SECBLK-T20BFFD                                                
         GOTO1 (RF),DMCB,('SECPINIT',(R0)),0                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
M51      B     M200                                                             
         SPACE 2                                                                
* ELIMINATE LOWER FIELDS *                                                      
         SPACE 1                                                                
M15100   NI    MISCLTH+4,X'DF'                                                  
M15200   FOUT  MISCLTNH,SPACES,20                                               
         NI    MISPRDH+4,X'DF'                                                  
*                                                                               
M15300   FOUT  MISPRDNH,SPACES,20                                               
         NI    MISESTH+4,X'DF'                                                  
*                                                                               
M15400   FOUT  MISESTNH,SPACES,20                                               
         NI    MISPERH+4,X'DF'                                                  
*                                                                               
M15900   XC    MISPAGE,MISPAGE                                                  
         MVI   MISPAGEH+5,0                                                     
         NI    MISOPTH+4,X'DF'                                                  
         L     RF,ABUCKETS                                                      
         XC    0(256,RF),0(RF)                                                  
         BR    RE                                                               
         EJECT                                                                  
*================================================================               
* VALIDATE CLIENT                                                               
*================================================================               
                                                                                
M200     LA    R2,MISCLTH                                                       
         MVI   ERRCD,14                                                         
         GOTO1 MOVE                                                             
* CLPACK                                                                        
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),WORK,KEY+2                                             
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    M300                                                             
*                                                                               
M202     XC    SAVBKEY2,SAVBKEY2   OLD BUY KEY                                  
         XC    SAVGKEY2,SAVGKEY2    OLD GOAL KEY                                
*                                                                               
         BAS   RE,M15200           CLEAR LOWER FIELDS                           
         NI    MISSTAH+4,X'DF'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   MISERR                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING DCLTHDR,R9                                                       
*                                                                               
         MVC   SVCOPT2,COPT2       SAVE COPT2                                   
         MVC   SVOFFC,COFFICE                                                   
         MVC   SVCACCS,CACCESS                                                  
         XC    SVMACCS,SVMACCS                                                  
         MVI   SVMACCS,X'FF'       SUPPRESS MARKET AUTH FOR NOW                 
         CLI   T20BFFD+6,C'+'                                                   
         BE    M206                                                             
         BRAS  RE,CALLOFCR                                                      
*                                                                               
M206     CLC   SAVBAMC,KEY+1       AGY/MEDIA AND CLIENT CODE CHANGED?           
         JE    *+8                                                              
         BRAS  RE,CKNSPROF         CHECK FOR NO SPILL PROFILE CONTROL           
         MVC   SAVBAMC,KEY+1                                                    
         MVC   SAVBKEY(3),KEY+1                                                 
         MVC   SAVGKEY+1(3),KEY+1                                               
         MVI   SAVGKEY,2                                                        
*                                                                               
         FOUT  MISCLTNH,CNAME,20                                                
         OI    4(R2),X'20'         CLIENT VALID                                 
* SAVE PRD LIST                                                                 
         LA    R0,4                                                             
         LA    R1,CPRDLIST                                                      
         LA    RE,CLIST                                                         
         MVC   0(220,R1),0(RE)                                                  
         LA    R1,220(R1)                                                       
         LA    RE,220(RE)                                                       
         BCT   R0,*-14                                                          
         MVC   SAVCLPRO,CPROF      CLIENT PROFILE                               
         MVC   SAVCLXTR,CEXTRA                                                  
         CLI   SAVCLXTR+2,C'N'                                                  
         BNE   *+8                                                              
         MVI   SAVCLXTR+2,0                                                     
         SPACE 1                                                                
* GET EQUIVALENCE HEADER                                                        
         SPACE 1                                                                
         MVC   DUB(2),AGYALPHA     AGENCY CODE                                  
         MVC   DUB+2(1),MISMED     MEDIA                                        
         MVC   DUB+3(2),SAVBKEY+1   CLIENT                                      
         L     R5,VDATAMGR                                                      
*                                                                               
         GOTO1 =V(EQVRD),DMCB,DUB,EQUDPT,EQUSECT1,(R5),RR=RELO                  
         CLI   DMCB+12,0           ERROR ?                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,RDPROFS                                                       
         SPACE 1                                                                
* WRITE TWA IN CASE ERROR AFTER THIS IN HEADLINE                                
         SPACE 1                                                                
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    2 BYTE TERM NO.                              
         MVI   DMCB+8,1            PAGE 1                                       
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,AMISTWA2                    
         EJECT                                                                  
M300     MVC   DMCB+4(4),=X'D9000A57'  GET A(SLNTAB)                            
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               RELOCTATE EOT ADDRESS                        
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         MVI   BYTE,C'T'                                                        
         CLI   MISMED,C'T'                                                      
         BE    M302                                                             
         CLI   MISMED,C'N'                                                      
         BE    M302                                                             
         CLI   MISMED,C'C'                                                      
         BE    M302                                                             
*                                                                               
         MVI   BYTE,C'R'                                                        
         CLI   MISMED,C'R'                                                      
         BE    M302                                                             
         CLI   MISMED,C'X'                                                      
         BE    M302                                                             
         DC    H'0'                                                             
                                                                                
*                                                                               
M302     CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         BE    M303                                                             
         CLC   0(2,R1),AGYALPHA    ELSE MATCH AGY                               
         BNE   *+14                                                             
M303     CLC   BYTE,2(R1)          AND MEDIA                                    
         BE    M304                                                             
*                                                                               
         BXLE  R1,RE,M302                                                       
         DC    H'0'                                                             
*                                                                               
M304     AHI   R1,4                POINT BEYOND HEADER                          
         ST    R1,VSLNTAB          AND SET AS SLNTAB ADDRESS                    
                                                                                
*================================================================               
* VALIDATE PRODUCT                                                              
*================================================================               
                                                                                
         LA    R2,MISPRDH                                                       
         MVI   ERRCD,15                                                         
         XC    IOAREA(32),IOAREA                                                
         GOTO1 VSCANNER,DMCB,(R2),(1,IOAREA),C',=,-'                            
*                                                                               
         LA    R4,IOAREA                                                        
         CLI   0(R4),0                                                          
         BE    MISERR                                                           
         MVC   KEY+4(3),12(R4)                                                  
         MVC   PIGPROD,22(R4)                                                   
*                                                                               
         GOTO1 MOVE                                                             
         MVC   MISPRD,WORK         FOR 3270 PROBLEM                             
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    M400                                                             
         BAS   RE,M15300                                                        
*                                                                               
         USING DPRDHDR,R9                                                       
*                                                                               
         MVI   SAVBKEY+3,X'FF'     PRD                                          
         MVI   SAVGKEY+4,X'FF'     PRD                                          
         CLC   MISPRD(3),=C'ALL'                                                
         BNE   M325                                                             
*                                                                               
         FOUT  MISPRDNH,=CL20'PRODUCTS'                                         
         MVC   KEY+4(3),=C'POL'    PRD                                          
         B     M350                                                             
*                                                                               
M325     GOTO1 HIGH                PRODUCT DIRKEY                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   MISERR                                                           
*                                                                               
         GOTO1 GETREC                                                           
         MVC   SAVBKEY+3(1),PCODE+1                                             
         MVC   SAVGKEY+4(1),PCODE+1                                             
* CHECK FOR PIGGYBACK PRODUCT                                                   
         XC    PIGCODE,PIGCODE                                                  
         MVC   MISPRDN(10),PNAME                                                
         CLC   PIGPROD(3),=XL3'404040'                                          
         BNE   M335                                                             
*                                                                               
         FOUT  MISPRDH                                                          
         FOUT  MISPRDNH,PNAME,20                                                
         B     M350                                                             
*                                                                               
M335     MVC   KEY+4(3),PIGPROD                                                 
         GOTO1 HIGH                PRODUCT DIRKEY                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   MISERR                                                           
         GOTO1 GETREC                                                           
         MVC   PIGCODE,PCODE+1                                                  
         FOUT  MISPRDH                                                          
         MVI   MISPRDN+10,C'/'                                                  
         MVC   MISPRDN+11(9),PNAME                                              
         FOUT  MISPRDNH                                                         
*                                                                               
M350     OI    4(R2),X'20'         VALID PRODUCT                                
         EJECT                                                                  
* VALIDATE ESTIMATE                                                             
         SPACE 1                                                                
M400     LA    R2,MISESTH                                                       
         TM    MISESTH+4,X'20'     TEST ESTIMATE CHANGED                        
         BO    *+8                                                              
         BAS   RE,M15400           SET TO REVALIDATE ESTIMATE                   
*                                                                               
         MVI   ERRCD,16                                                         
         CLC   MISPER(2),=C'ES'                                                 
         BE    *+12                                                             
         TM    MISPERH+4,X'20'     PREVIOUSLY VALID?                            
         BO    M405                                                             
*                                                                               
         CLC   MISMKT,=C'LIST'     IN LIST MODE?                                
         BNE   M401                NO                                           
*                                  YES, MARKET LIST MODE!                       
         TM    MISMKTH+4,X'20'     DID MARKET CHANGE?                           
         BZ    M401                - YES, CHECK PERIOD                          
         TM    MISESTH+4,X'20'     DID ESTIMATE CHANGE?                         
         BO    M405                NO                                           
*                                                                               
M401     CLI   MISPERH+5,0         TEST PERIOD NOT INPUT                        
         BNE   M402                                                             
         MVC   MISPER(2),=C'ES'                                                 
         MVI   MISPERH+5,2                                                      
         FOUT  MISPERH                                                          
*                                                                               
M402     BAS   RE,M15400           CLEAR LOWER FIELDS                           
         XC    SAVESLST,SAVESLST   CLEAR EST FILTER LIST                        
*                                                                               
         CLC   =C'ALL',MISPRD      ALL PRODUCTS?                                
         BNE   *+10                                                             
         MVC   KEY+4(3),=C'POL'                                                 
         CLC   =C'F=',MISEST       TEST EST FILTER OPTION                       
         BE    M410                                                             
         CLC   =C'ALL',MISEST      OR EST=ALL                                   
         BE    M410                                                             
*                                                                               
         GOTO1 PACK                                                             
         LTR   R0,R0                                                            
         BZ    MISERR                                                           
         CH    R0,=H'255'                                                       
         BH    MISERR                                                           
         STC   R0,KEY+7                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   MISERR                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING DESTHDR,R9                                                       
         FOUT  MISESTNH,EDESC,20                                                
         MVC   SVEOWSDY,EOWSDAY    SAVE OUT-OF-WEEK DATA FLAG                   
         MVC   SVEDAILY,EDAILY     SAVE DAILY EST FLAG                          
         MVC   SVEPWPCT,EPWPCT                                                  
         MVC   SVCOS2,ECOST2                                                    
         DROP  R9                                                               
         MVC   SAVBKEY+9(1),KEY+7                                               
         MVC   SAVGKEY+7(1),KEY+7                                               
         B     M450                                                             
         SPACE 2                                                                
M405     CLC   =C'F=',MISEST       IF EST FILTER OR ALL                         
         BE    *+14                BEWARE A CHANGE OF PERIOD                    
         CLC   =C'ALL',MISEST                                                   
         BNE   M500                                                             
         TM    MISPERH+4,X'20'     TEST CHANGE OF PERIOD                        
         BO    M500                NO - CONTINUE                                
         EJECT                                                                  
* PROCESSING ESTIMATE FILTERS AND EST=ALL *                                     
         SPACE 1                                                                
M410     DS    0H                                                               
         MVI   ERRCD,70            NOT AUTHORIZED                               
         LA    RE,TWAAUTH-TWAD(RA)                                              
         TM    0(RE),X'80'         TEST REQUIRE AUTH TO SEE DATA                
         BO    MISERR              YES - ONLY ONE ESTIMATE ALLOWED              
*                                                                               
         XC    SAVESLST,SAVESLST   CLEAR FILTER LIST                            
         MVI   SAVBKEY+9,0         CLEAR ESTIMATE NUMBER                        
         MVI   SAVGKEY+7,0                                                      
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CPERVAL-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(MISPERH+5,MISPER),WORK3                               
         CLI   4(R1),X'00'                                                      
         BE    M411                                                             
         CLI   4(R1),X'04'                                                      
         BE    M411                                                             
         LA    R2,MISPERH                                                       
         MVI   ERRCD,20                                                         
         B     MISERR                                                           
*                                                                               
M411     LA    R6,WORK3                                                         
         USING PERVALD,R6                                                       
         TM    PVALASSM,PVALASD                                                 
         BNO   M412                                                             
*                                                                               
         MVI   DMCB+7,QGETBRD      CALLOV FOR THE GETBROAD ROUTINE              
         BRAS  RE,MCALLOV                                                       
         L     RF,0(R1)                                                         
*                                                                               
         L     RE,VGETDAY                                                       
         ST    RE,DMCB+8                                                        
         L     RE,VADDAY                                                        
         ST    RE,DMCB+12                                                       
         GOTO1 (RF),DMCB,(X'80',PVALESTA),WORK                                  
         MVC   PVALESTA,WORK                                                    
         TM    PVALASSM,PVALAED    IS THE DATE ASSUMED?                         
         BNO   M412A                                                            
         MVC   PVALEEND+4(2),=C'15'   ASSURE GETTING CORRECT BRD-MONTH          
         GOTO1 (RF),DMCB,(X'80',PVALEEND),WORK                                  
         MVC   PVALEEND,WORK+6                                                  
M412A    GOTO1 VDATCON,DMCB,PVALESTA,(11,PVALCPER),0                            
         GOTO1 VDATCON,DMCB,PVALEEND,(11,PVALCPER+9),0                          
*                                                                               
M412     MVC   MISPER(17),PVALCPER     DISPLAY THE DATE                         
         MVI   MISPERH+5,17                                                     
         OI    MISPERH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
         LA    R4,10(R2)           POINT TO FILTER LIST                         
         ZIC   R5,5(R2)            GET LENGTH                                   
         BCTR  R5,0                ADJUST FOR F=                                
         BCTR  R5,0                                                             
         MVC   DUB(6),=6C'*'       PRESET                                       
         LA    R6,DUB                                                           
         CLC   =C'ALL',8(R2)                                                    
         BE    M420                                                             
                                                                                
M413     CLI   0(R4),C'*'                                                       
         BE    M416                                                             
         CLI   0(R4),C'-'          TEST NEGATIVE FILTER                         
         BE    M414                                                             
         CLI   0(R4),C'A'                                                       
         BL    M418                                                             
         CLI   0(R4),C'9'                                                       
         BH    M418                                                             
         MVI   0(R6),C'+'          INDICATE POSITIVE FILTER                     
         MVC   1(1,R6),0(R4)                                                    
         B     M416                                                             
*                                                                               
M414     CLI   1(R4),C'A'                                                       
         BL    M418                                                             
         CLI   1(R4),C'9'                                                       
         BH    M418                                                             
         MVI   0(R6),C'-'          INDICATE NEGATIVE FILTER                     
         MVC   1(1,R6),1(R4)                                                    
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
*                                                                               
M416     LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BZ    M420                                                             
         LA    R6,2(R6)                                                         
         LA    R0,DUB+6                                                         
         CR    R6,R0                                                            
         BL    M413                                                             
         SPACE 2                                                                
M418     MVI   ERRCD,2             FILTER NOT VALID                             
         B     MISERR                                                           
         EJECT                                                                  
* READ ESTIMATE HEADERS AND FILTER ON FILTERS/DATE *                            
         SPACE 1                                                                
M420     MVC   KEY+8(5),=5X'FF'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE      SAME PRD?                                    
         BNE   M430                                                             
         OC    KEY+8(5),KEY+8      BILL REC?                                    
         BNZ   M420                                                             
*                                                                               
         GOTO1 GETREC                                                           
         USING DESTHDR,R9                                                       
         LA    R6,WORK3                                                         
         USING PERVALD,R6                                                       
         CLC   PVALEEND,ESTART                                                  
         BL    M420                                                             
         CLC   PVALESTA,EEND                                                    
         BH    M420                                                             
         LA    R0,3                                                             
         LA    R1,DUB                                                           
         LA    RE,EPROF                                                         
         DROP  R6                                                               
*                                                                               
M424     CLI   0(R1),C'*'                                                       
         BE    M428                                                             
         CLI   0(R1),C'+'                                                       
         BNE   M426                                                             
         CLC   1(1,R1),0(RE)       MATCH POSITIVE FILTER                        
         BNE   M420                NO - IGNORE                                  
         B     M428                                                             
*                                                                               
M426     CLC   1(1,R1),0(RE)       MATCH NEGATIVE FILTER                        
         BE    M420                YES - IGNORE                                 
*                                                                               
M428     LA    RE,1(RE)            NEXT ESTHDR FILTER                           
         LA    R1,2(R1)            NEXT INPUT FILTER                            
         BCT   R0,M424                                                          
*                                                                               
         OC    SAVESLST,SAVESLST   TEST ANY EST IN LIST YET                     
         BNZ   M429                YES                                          
*                                                                               
         LAY   RE,IOAREA                                                        
         LAY   R0,IOAREA+3000                                                   
         LLH   RF,ELEN                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE               SAVE FIRST ESTHDR IN PERIOD                  
*                                                                               
M429     CLC   ELEN,=AL2(3000)     IF RECLEN>3000, THEN DIE!!                   
         JH    *+2                  NEED TO SAVE FIRST ESTHDR ELSEWHERE         
*                                                                               
         ZIC   RE,KEY+7            GET ESTIMATE NUMBER                          
         LA    RE,SAVESLST(RE)     POINT TO SLOT                                
         MVC   0(1,RE),KEY+7       SET EST ACTIVE FLAG                          
         B     M420                                                             
*                                                                               
M430     MVI   ERRCD,16                                                         
         OC    SAVESLST,SAVESLST   TEST ANY ESTS ACTIVE                         
         BZ    MISERR                                                           
*                                                                               
         LAY   R0,IOAREA                                                        
         LAY   RE,IOAREA+3000                                                   
         LLH   RF,ELEN                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE               RESTORE FIRST ESTHDR                         
*                                                                               
         FOUT  MISESTNH,=CL20'ALL ESTIMATES'                                    
         NI    MISPERH+4,X'DF'         PERIOD INVALID                           
*                                                                               
M450     MVI   SVNEWDEM,C'Y'                                                    
         MVC   SVDEMLST(12),EDEMLST    SAVE 4 DEMO CODES                        
         L     RE,ASVNTDMS                                                      
         MVC   0(20*L'ENONTDMS,RE),ENONTDMS  SAVE 20 NONT DEMO NAMES            
         L     RE,ASVUSRNMS                                                     
         MVC   0(28,RE),EUSRNMS                                                 
* INITIALIZE DBLOCK (FOLLOWS ESTHDR)                                            
         XC    DBLOCK(256),DBLOCK                                               
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   MISMED,C'R'                                                      
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAGPRF7,C'C'       TEST CANADIAN                                
         BNE   M452                                                             
         CLI   SAVCLXTR,C'U'       TEST US DEMOS                                
         BE    M452                                                             
         MVI   DBSELMED,C'C'                                                    
* GET A(DEMOCON) *                                                              
M452     XC    DMCB(8),DMCB                                                     
         GOTO1 VCALLOV,DMCB,,X'D9000AE0'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
* GET DEMO NAMES *                                                              
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(4,EDEMLST),(2,SVDEMNMS),                     X        
               (C'S',DBLOCK),(SVSPPRFA,EUSRNMS),ENONTDMS                        
*                                                                               
         MVC   SAVPER,ESTART                                                    
         GOTO1 VDATCON,DMCB,ESTART,(3,SVESDTS),0                                
         GOTO1 (RF),(R1),EEND,(3,SVESDTS+3),0                                   
*                                                                               
         CLC   =C'ES',MISPER                                                    
         BNE   M460                                                             
         GOTO1 VDATCON,DMCB,ESTART,(11,WORK),0                                  
         MVI   WORK+8,C'-'                                                      
         GOTO1 (RF),(R1),EEND,(11,WORK+9),0                                     
         FOUT  MISPERH,WORK,17                                                  
         MVI   MISPERH+5,17                                                     
*                                                                               
M460     MVC   DMCB(2),AGYALPHA                                                 
         MVC   DMCB+2(1),MISMED                                                 
         MVC   DMCB+3(1),EDAYMENU                                               
         L     R4,VDATAMGR                                                      
         GOTO1 =V(DPTRD),DMCB,,SAVMENU,(R4),RR=RELO                             
         TM    DMCB+8,X'FF'                                                     
         BZ    M461                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(ERRNODPT)                DAYPART NOT FOUND           
         B     MISERR                                                           
*                                                                               
M461     DS    0H                                                               
         CLC   SVXFRSYS(6),=C'SPOSDE'                                           
         BE    M474                                                             
         LA    RE,TWAAUTH-TWAD(RA)                                              
         TM    0(RE),X'80'         TEST REQUIRE AUTH TO SEE DATA                
         BZ    M474                NO                                           
         CLC   SVXFRSYS(6),=C'SPOBUC'   BYPASS AUTH CHECK IF FROM               
         BE    M474                     BUCH (TWAAUTH NO GOOD?)                 
         CLC   SVXFRSYS(6),=C'SPOINC'                                           
         BE    M474                                                             
* NEED TO READ POL ESTIMATE TO FIND OUT                                         
         MVI   ERRCD,70            NOT AUTHORIZED                               
         CLC   =C'POL',KEY+4       TEST HAVE POL EST ALREADY                    
         BE    M472                                                             
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   MISERR                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING DESTHDR,R9                                                       
M472     TM    EFLAG1,EF1REQ       TEST TO ALLOW ACCESS                         
         BZ    MISERR                                                           
*                                                                               
M474     DS    0H                                                               
         OI    4(R2),X'20'             SET EST VALIDATED                        
         EJECT                                                                  
* VALIDATE MARKET *                                                             
         SPACE 1                                                                
M500     LA    R2,MISMKTH                                                       
         MVI   ERRCD,17                                                         
         CLC   =C'LIST',8(R2)      LOOK IF LIST CALL                            
         BE    MLIST                                                            
         CLC   =C'ALL',8(R2)                                                    
         BE    MISERR                                                           
         TM    4(R2),X'20'                                                      
         BO    M520                                                             
         FOUT  MISMKTNH,SPACES,23                                               
         NI    MISSTAH+4,X'DF'                                                  
         GOTO1 PACK                                                             
         LTR   R0,R0                                                            
         BNZ   M505                                                             
*                                                                               
         CLC   MISSTA(3),=C'ALL'                                                
         BE    MISERR                                                           
         CLC   MISSTA(4),=C'LIST'                                               
         BE    MISERR                                                           
         MVC   MISMKT(4),=C'0000'                                               
         XC    SAVBKEY+4(2),SAVBKEY+4                                           
         B     M507                                                             
*                                                                               
M505     STH   R0,DUB                                                           
         MVC   SAVBKEY+4(2),DUB                                                 
         MVC   SAVGKEY+5(2),DUB                                                 
         BRAS  RE,GETMKT                                                        
         BNE   MISERR                                                           
M507     OI    MISMKTH+4,X'20'       VALID MARKET                               
         B     M520                                                             
*                                                                               
MLIST    DS    0H                                                               
         OI    MISMKTH+4,X'20'     VALID MARKET                                 
         OI    MISOPTH+4,X'20'     FLAG OPTIONS FIELD VALID                     
         OI    MISPERH+4,X'20'     FLAG PERIOD FIELD VALID                      
         MVI   FORMIND,C'P'                                                     
         CLC   =C'LIST',MISMKT     MARKET LIST?                                 
         BNE   MLIST10                                                          
         MVI   FORMIND,C'M'                                                     
MLIST10  GOTO1 VCALLOV,DMCB,(3,0),(RA) CALL T20B03 OVERLAY                      
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC)      MAKE & DISPLAY LIST                          
*                                                                               
         NI    MISPERH+4,X'DF'                                                  
         TM    PCDRIVEN,PCGRIDQ    USING GRIDS?                                 
         BO    M1075               YES - GO HERE INSTEAD                        
         B     EXXMOD              NO, EXIT                                     
         EJECT                                                                  
* VALIDATE STATION *                                                            
M520     XC    SVMKT,SVMKT                                                      
         BRAS  RE,VALSTA                                                        
         BNE   MISERR                                                           
*                                                                               
         CLI   T20BFFD+6,C'+'      TEST LIMIT ACCESS BY MKT                     
         BE    *+12                                                             
         CLI   T20BFFD+8,C'+'      OR ALTERNATE LOCATION                        
         BNE   M600                                                             
*                                                                               
         BRAS  RE,GETMKT           READ MARKET/VALIDATE ACCESS                  
         BNE   MISERR                                                           
*                                                                               
* VALIDATE PERIOD *                                                             
         SPACE 1                                                                
M600     LA    R2,MISPERH                                                       
         MVI   ERRCD,20                                                         
         TM    4(R2),X'20'                                                      
         BO    M700                                                             
*                                                                               
M602     L     RF,VCOMFACS                                                      
         L     RF,CPERVAL-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(MISPERH+5,MISPER),WORK3                               
         CLI   4(R1),X'00'                                                      
         BE    M603                                                             
         CLI   4(R1),X'04'                                                      
         BE    M603                                                             
         B     MISERR                                                           
*                                                                               
M603     LA    R6,WORK3            DISPLAY THE DATE AFTER CONVERSION            
         USING PERVALD,R6                                                       
         TM    PVALASSM,PVALASD    START DAY ASSUMED?                           
         BNO   M604                                                             
*                                                                               
         MVI   DMCB+7,QGETBRD      CALLOV FOR THE GETBROAD ROUTINE              
         BRAS  RE,MCALLOV                                                       
         L     RF,0(R1)                                                         
*                                                                               
         L     RE,VGETDAY                                                       
         ST    RE,DMCB+8                                                        
         L     RE,VADDAY                                                        
         ST    RE,DMCB+12                                                       
         GOTO1 (RF),DMCB,(X'80',PVALESTA),WORK                                  
         MVC   PVALESTA,WORK                                                    
         TM    PVALASSM,PVALAED                                                 
         BNO   M604A                                                            
         MVC   PVALEEND+4(2),=C'15'   ASSURE GETTING CORRECT BRD-MONTH          
         GOTO1 (RF),DMCB,(X'80',PVALEEND),WORK                                  
         MVC   PVALEEND,WORK+6                                                  
M604A    GOTO1 VDATCON,DMCB,PVALESTA,(11,PVALCPER),0                            
         GOTO1 VDATCON,DMCB,PVALEEND,(11,PVALCPER+9),0                          
*                                                                               
M604     MVC   MISPER(17),PVALCPER                                              
         OI    MISPER+6,X'80'                                                   
         DROP  R6                                                               
*                                                                               
         OC    SAVESLST,SAVESLST    TEST FILTERS OR ALL ESTS                    
         BNZ   M650                                                             
         SPACE 1                                                                
* GET REQUEST DATES IN 3 BYTE BINARY                                            
         SPACE 1                                                                
         LA    R6,WORK3                                                         
         USING PERVALD,R6                                                       
         MVC   DUB(3),PVALBSTA                                                  
         MVC   DUB+3(3),PVALBEND                                                
         DROP  R6                                                               
*                                                                               
         MVI   ERRCD,79            'DATES NOT IN ES PERIOD'                     
         CLC   DUB+3(3),SVESDTS    REQ END TO EST START                         
         BL    MISERR                                                           
         CLC   DUB(3),SVESDTS+3    REQ START TO EST END                         
         BH    MISERR                                                           
*                                                                               
M650     LA    R6,WORK3                                                         
         USING PERVALD,R6                                                       
         MVC   SAVPER(6),PVALESTA  SAVE REQUEST PERIOD                          
         MVC   SAVPER+6(6),PVALEEND                                             
         XC    WORK,WORK                                                        
         MVC   WORK(17),PVALCPER                                                
         FOUT  MISPERH,WORK,17     PERIOD                                       
         MVI   MISPERH+5,17                                                     
M690     OI    4(R2),X'20'         PERIOD VALID INDICATOR                       
         EJECT                                                                  
         DROP  R6                                                               
* VALIDATE DAYPART *                                                            
         SPACE 1                                                                
M700     LA    R2,MISDPTH                                                       
         TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BO    M800                                                             
         L     RE,ABUCKETS                                                      
         XC    0(256,RE),0(RE)     FORCE NEW BUCKETS                            
*                                                                               
         MVI   ERRCD,21                                                         
         MVI   SAVDPTCD,0                                                       
         CLC   MISDPT(3),=C'ALL'                                                
         BE    M700X                                                            
         CLI   5(R2),0             TEST OMITTED                                 
         BE    M700X                                                            
*                                                                               
         LA    R5,SAVMENU          PREPARE TO GET DAYPART MENU CODE             
         LA    R6,5                                                             
         LA    R7,SAVMENU+L'SAVMENU-1                                           
         CLI   0(R5),0                                                          
         BE    MISERR                                                           
         CLC   0(1,R5),MISDPT                                                   
         BE    *+12                                                             
         BXLE  R5,R6,*-18                                                       
         B     MISERR                                                           
         MVC   SAVDPTCD,1(R5)                                                   
*                                                                               
M700X    OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         SPACE 1                                                                
* VALIDATE LENGTH *                                                             
         SPACE 1                                                                
M800     LA    R2,MISLENH                                                       
         MVI   ERRCD,23                                                         
         MVI   SAVLEN,0                                                         
         CLC   MISLEN(3),=C'ALL'                                                
         BE    M900                                                             
         CLI   5(R2),0             TEST OMITTED                                 
         BE    M900                                                             
         GOTO1 PACK                                                             
         LTR   R0,R0                                                            
         BZ    MISERR                                                           
         STC   R0,SAVLEN                                                        
         FOUT  MISLENH                                                          
         EJECT                                                                  
* VALIDATE FORMAT FOR DISPLAY *                                                 
         SPACE 1                                                                
M900     OI    MISDPTH+4,X'20'     SET VALIDATED                                
         OI    MISLENH+4,X'20'                                                  
*                                                                               
         LA    R2,MISFORH                                                       
         MVI   ERRCD,5             FORMAT NOT AVAILABLE MESSAGE                 
         MVI   FORMIND,C'D'        DEMOS                                        
         CLC   MISFOR(3),=C'DEM'                                                
         BE    M920                                                             
*                                                                               
         MVI   FORMIND,C'L'        LOCKIN V PURCHASED                           
         CLC   MISFOR(3),=C'LVP'                                                
         BNE   M902                                                             
         CLI   SVFORMGL,C'L'       TEST SAME AS PREVIOUS                        
         BE    M920                                                             
         L     RE,ABUCKETS                                                      
         XC    0(256,RE),0(RE)                                                  
         B     M920                                                             
*                                                                               
M902     MVI   FORMIND,C'G'        GVP IND                                      
         MVC   MISFOR(3),=C'GVP'    GOAL V PURCHASED                            
         CLI   SVFORMGL,C'L'       TEST PREVIOUS WAS LOCKIN                     
         BNE   M920                                                             
         L     RE,ABUCKETS                                                      
         XC    0(256,RE),0(RE)                                                  
*                                                                               
M920     MVC   SVFORMGL,FORMIND                                                 
         MVI   WEEKIND,0                                                        
         CLC   MISFOR+3(2),=C'-W'    WEEKS REQUESTED?                           
         BE    *+14                                                             
         XC    MISFOR+3(2),MISFOR+3                                             
         B     M950                                                             
         MVI   WEEKIND,C'W'                                                     
         CLC   MISPRD(3),=C'ALL'                                                
         BE    MISERR                                                           
         CLC   MISSTA(4),=C'LIST'                                               
         BE    MISERR                                                           
M950     FOUT  MISFORH                                                          
         OI    MISFORH+4,X'20'                                                  
*                                                                               
         LA    R2,MISPAGEH         PAGE FIELD                                   
         GOTO1 PACK                                                             
         MVI   PAGENUM,1                                                        
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         STC   R0,PAGENUM                                                       
         EJECT                                                                  
* VALIDATE OPTIONS *                                                            
*                                                                               
M960     LA    R2,MISOPTH                                                       
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    M1000               YES                                          
         L     RE,ABUCKETS                                                      
         XC    0(256,RE),0(RE)                                                  
         BAS   RE,M15900                                                        
*                                                                               
         XC    SVID,SVID           CLEAR ID                                     
         MVI   SVOPTS,0            RESET SAVED OPTIONS                          
         MVI   SVOPT2,0                                                         
         TM    SVCOPT2,COP2DIY     TEST DIY TRADE CLIENT                        
         BZ    M961                                                             
         CLI   MISPRD+2,C'#'       TEST TRADE PRODUCT                           
         BE    M961                                                             
         MVI   SVOPT2,SVOPT2_TRD   DEFAULT TO TRD=Y                             
*                                                                               
M961     CLI   5(R2),0             TEST ANY OPTIONS GIVEN                       
         BE    M1000               NO                                           
*                                                                               
         LA    R4,IOAREA+2000                                                   
         XC    0(256,R4),0(R4)                                                  
         GOTO1 VSCANNER,DMCB,(R2),(8,(R4)),C',=,='                              
         CLI   DMCB+4,0            TEST VALID SCANNER DATA                      
         BNE   *+12                YES                                          
         MVI   ERRCD,2             NO - 'INVALID INPUT'                         
         B     MISERR                                                           
*                                                                               
         ZIC   R0,DMCB+4           NO. OF OPTIONS                               
         MVI   NUMFLD,1            OPTION NUMBER                                
*                                                                               
M970     SR    RE,RE                                                            
         IC    RE,0(R4)            GET OPTION LENGTH                            
         BCTR  RE,0                                                             
         EX    RE,OPTDEM                                                        
         BNE   M971                                                             
         BAS   RE,VALDEM                                                        
         BE    M980                                                             
         B     M990                                                             
OPTDEM   CLC   12(0,R4),=C'DEMO'   DEMO OVERRIDE                                
*                                                                               
M971     EX    RE,OPTMENU                                                       
         BNE   M971A                                                            
         BAS   RE,VALMENU                                                       
         BE    M980                                                             
         B     M990                                                             
OPTMENU  CLC   12(0,R4),=C'MENU'   DEMO OVERRIDE                                
*                                                                               
M971A    EX    RE,OPTCV            PW CLIENT VERSION                            
         BNE   M971B                                                            
         OI    SVOPTS,SVOPTPW                                                   
         OC    SVEPWPCT,SVEPWPCT   MAKE SURE HAVE A PW CLIENT                   
         BNZ   M980                YES - OK                                     
         B     M990                NO - ERROR                                   
OPTCV    CLC   12(0,R4),=C'CV'                                                  
*                                                                               
M971B    CLC   =C'PUR   ',12(R4)   TEST PURPOSE CODE REQ                        
         BNE   M971C                                                            
         CLI   SVB0PROF+9,C'O'     PURPOSE CODES ARE OPTIONAL?                  
         JE    *+12                                                             
         CLI   SVB0PROF+9,C'Y'     TEST PURPOSE CODES ACTIVE                    
         BNE   M990                                                             
         MVC   SVID,22(R4)                                                      
         CLC   =C'LIST',22(R4)                                                  
         BE    MLIST                                                            
*                                                                               
K        USING PRPRECD,KEY         READ PURPOSE CODE RECORD                     
*                                                                               
         XC    KEY,KEY                                                          
         MVI   K.PRPKTYP,PRPKTYPQ                                               
         MVI   K.PRPKSUB,PRPKSUBQ                                               
         MVC   K.PRPKAGY,AGYALPHA                                               
         MVC   K.PRPKMED,MISMED                                                 
         MVC   K.PRPCODE,SVID                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   M990                                                             
         B     M980                                                             
         DROP  K                                                                
*                                                                               
M971C    CLC   =C'IDR   ',12(R4)    TEST ID ENTERED                             
         BNE   M971E                                                            
         CLI   SAVCLXTR+2,C'A'                                                  
         BL    M990                                                             
         CLI   SAVCLXTR+2,C'K'                                                  
         BH    M990                                                             
*                                                                               
         CLI   PIGCODE,0           CANNOT USE 2ND PRODUCT WITH ID               
         BNE   PRDIDROP                                                         
         MVC   SVID,22(R4)                                                      
         CLC   =C'LIST',SVID       CHECK IF IDR ENTERED WAS LIST                
         BE    MLIST               GO TO 03 TO DISPLAY LIST                     
         CLI   1(R4),5             ID CAN BE 5-6 CHAR ONLY                      
         BL    M990                                                             
         CLI   1(R4),6                                                          
         BH    M990                                                             
         B     M980                                                             
*                                                                               
M971E    CLI   0(R4),2             MUST HAVE AT LEAST 2 CHAR                    
         BNH   M971G                                                            
*                                                                               
         LA    RF,SVOPT2_MLK       DEFAULT LOCKIN IS MKT                        
         EX    RE,OPTLOCK                                                       
         BE    M971F                                                            
*                                                                               
         CLI   0(R4),3             MKTL CONFLICTS WITH MKT BELOW                
         BNH   *+12                                                             
         EX    RE,MKTLOCK                                                       
         BE    M971F                                                            
*                                                                               
         EX    RE,MLOCK                                                         
         BE    M971F                                                            
*                                                                               
         LA    RF,SVOPT2_SLK                                                    
         EX    RE,STALOCK                                                       
         BE    M971F                                                            
*                                                                               
         LA    RF,SVOPT2_SLK                                                    
         EX    RE,STLOCK                                                        
         BE    M971F                                                            
*                                                                               
         LA    RF,SVOPT2_2DEC                                                   
         EX    RE,DECLOCK                                                       
         BE    M971F                                                            
*                                                                               
         LA    RF,SVOPT2_SLK                                                    
         EX    RE,QSLOCK                                                        
         BNE   M971G                                                            
M971F    EX    RF,*+8                                                           
         B     M980                                                             
         OI    SVOPT2,0  **EXECUTED**                                           
OPTLOCK  CLC   12(0,R4),=C'LOCKIN'                                              
MLOCK    CLC   12(0,R4),=C'MLOCK'                                               
MKTLOCK  CLC   12(0,R4),=C'MKTLOCK'                                             
QSLOCK   CLC   12(0,R4),=C'SLOCK'                                               
STALOCK  CLC   12(0,R4),=C'STALOCK'                                             
STLOCK   CLC   12(0,R4),=C'STLOCK'                                              
DECLOCK  CLC   12(0,R4),=C'2DEC'                                                
*                                                                               
M971G    CLC   =C'MKT',12(R4)      TEST MARKET ENTERED                          
         BNE   M972                                                             
         XC    SVMKT,SVMKT                                                      
         MVC   SVBMKTOV,10(R4)     SAVE BINARY MARKET OVERRIDE                  
         MVI   ERRCD,17            STATION NOT IN THIS MARKET                   
         TM    3(R4),X'80'         MARKET IS VALID NUMERIC?                     
         BNO   MISERR                                                           
         LR    RE,R0                                                            
         EDIT  (2,10(R4)),(4,SVMKT),FILL=0                                      
         LR    R0,RE                                                            
         NI    MISSTAH+4,X'FF'-X'20'                                            
         BRAS  RE,VALSTA                                                        
         BE    M980                                                             
         MVI   ERRCD,78            STATION NOT IN THIS MARKET                   
         B     MISERR                                                           
*                                                                               
M972     CLC   =C'NTX',12(R4)      TEST 'NO TAX' OPTION                         
         BE    M972A               NO                                           
         CLC   =C'NOTAX',12(R4)    OR ALTERNATE                                 
         BE    M972A                                                            
         CLC   =C'TAX',12(R4)      OR TAX=Y/N                                   
         BNE   M972B                                                            
         CLI   22(R4),C'N'                                                      
         BE    M972A                                                            
         CLI   22(R4),C'Y'                                                      
         BE    M980                                                             
         B     M990                                                             
M972A    OI    SVOPTS,SVOPTNTX                                                  
         B     M980                                                             
*                                                                               
M972B    CLC   =C'TRD',12(R4)      TEST TRD=Y TO MERGE CASH/TRD PRDS            
         BNE   M973                NO                                           
         OI    SVOPT2,SVOPT2_TRD                                                
         CLI   22(R4),C'Y'                                                      
         BE    M980                                                             
         NI    SVOPT2,X'FF'-SVOPT2_TRD                                          
         CLI   22(R4),C'N'                                                      
         BE    M980                                                             
         B     M990                                                             
*                                                                               
M973     CLI   1(R4),0             NO SUB-OPTIONS ALLOWED                       
         BNE   M977                EXCEPT FOR NET                               
*                                                                               
         CLI   0(R4),4             TEST LENGTH 4                                
         BNE   M974                                                             
         CLC   =C'COS2',12(R4)                                                  
         BNE   *+12                                                             
         OI    SVOPTS,SVOPT$2                                                   
         B     M980                                                             
*                                                                               
M974     CLI   0(R4),3             MOST OPTIONS ARE 3 CHARACTERS LONG           
         BE    M976                                                             
         CLI   0(R4),2             XCHRATE OPTION IS 2 CHARS. LONG              
         BNE   M990                                                             
*                                                                               
         CLC   =C'C2',12(R4)       TEST COST2 WANTED                            
         BNE   *+12                                                             
         OI    SVOPTS,SVOPT$2                                                   
         B     M980                                                             
*                                                                               
         CLC   =C'$2',12(R4)       TEST COST2 WANTED                            
         BNE   *+12                                                             
         OI    SVOPTS,SVOPT$2                                                   
         B     M980                                                             
*                                                                               
         CLC   =C'$U',12(R4)       TEST USA CURRENCY XCHRATE                    
         BNE   *+12                                                             
         OI    SVOPTS,SVOPTUSA                                                  
         B     M980                                                             
*                                                                               
         CLC   =C'$C',12(R4)       TEST CANADA CURRENCY XCHRATE                 
         BNE   *+12                                                             
         OI    SVOPTS,SVOPTCAN                                                  
         B     M980                                                             
*                                                                               
         CLC   =C'PD',12(R4)       TEST POST BUY DATA WANTED                    
         BNE   *+12                                                             
         OI    SVOPTS,SVOPTPD                                                   
         B     M980                                                             
*                                                                               
M975     B     M990                ERROR                                        
*                                                                               
M976     CLC   =C'NSP',12(R4)      TEST 'NO SPILL' OPTION                       
         BNE   M977                NO                                           
         OI    SVOPTS,SVOPTNSP                                                  
         B     M980                                                             
*                                                                               
M977     CLC   =C'NET',12(R4)      TEST NET DOLLARS OPTION                      
         BNE   M990                NO                                           
         CLI   1(R4),0             BUT IT MIGHT BE THE NET(WORK) OPT            
         BNE   M978                                                             
         OI    SVOPT2,SVOPT2_NET                                                
         B     M980                                                             
*                                                                               
M978     DS    0H                  VALIDATE NET=(CANNET)                        
         CLI   1(R4),3                                                          
         BL    M990                                                             
         CLI   1(R4),4                                                          
         BH    M990                                                             
         MVC   WORK,SPACES                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),22(R4) *EXECUTED*                                        
         BRAS  RE,SETBITS                                                       
         BNE   M990                                                             
*                                                                               
M980     LA    R4,32(R4)           NEXT SCANNER ENTRY                           
         ZIC   R1,NUMFLD                                                        
         LA    R1,1(R1)                                                         
         STC   R1,NUMFLD                                                        
         BCT   R0,M970                                                          
*                                                                               
         CLI   0(R4),0             TEST EXCESS OPTIONS                          
         BE    M1000               NO                                           
*                                                                               
M990     MVI   ERRAREA,X'FF'       HANDLE ERROR CONDITION                       
         XC    MISMSG,MISMSG                                                    
         MVC   MISMSG(L'INVOPMSG),INVOPMSG                                      
         OI    MISMSGH+6,X'80'     XMIT                                         
         LA    R4,MISMSG+17                                                     
         EDIT  (B1,NUMFLD),(1,(R4)) OPTION NUMBER                               
         B     MISERR                                                           
INVOPMSG DC    CL60'* ERROR * OPTION X INVALID *'                               
         EJECT                                                                  
*============================================================                   
* GET CANADIAN NETWORK BITS                                                     
*============================================================                   
         SPACE 1                                                                
SETBITS  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),WORK       WORK HAS NTWK                                
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   SBNEQX                                                           
*                                                                               
         GOTO1 GETREC                                                           
         USING NDEFRECD,R9                                                      
*                                                                               
         LA    R6,NDEFEL                                                        
         SR    R0,R0                                                            
*                                                                               
SETBITS2 CLI   0(R6),2                                                          
         BE    SETBITS4                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   SETBITS2                                                         
         DC    H'0'                                                             
*                                                                               
SETBITS4 MVC   SAVBKEY+8(1),2(R6)                                               
         B     SBX                                                              
*                                                                               
SBNEQX   LTR   RB,RB                                                            
         B     SBX                                                              
SBEQX    CR    RB,RB                                                            
*                                                                               
SBX      J     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
*================================================================               
* READ PW RECORD                                                                
*================================================================               
         SPACE 1                                                                
GETPW    NTR1                                                                   
         OC    SVEPWPCT,SVEPWPCT   MAKE SURE HAVE A PW CLIENT                   
         BZ    GETPWX                                                           
         XC    SVPWTAX,SVPWTAX                                                  
         XC    SVPWPCT(SVPWPCTL),SVPWPCT                                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PWRECD,R3                                                        
*                                                                               
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD(4),SAVBKEY    AGMD/CLT/PRD                               
         MVC   PWKEST,SAVBKEY+9                                                 
         MVC   PWKMKT,SAVBKEY+4                                                 
         DROP  R3                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETPWX                                                           
*                                                                               
         USING PWRECD,R9                                                        
GETPW20  GOTO1 GETREC                                                           
*                                                                               
         MVC   SVPWTAX,PWGNTAX     SAVE SF TAX                                  
*                                                                               
         LA    R3,PWEL             POINT TO FIRST ELEMENT                       
         LA    R1,SVPWPCT                                                       
         MVI   ELCDLO,X'05'                                                     
         MVI   ELCDHI,X'05'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   GETPWX                                                           
         B     GETPW32                                                          
*                                                                               
GETPW30  BAS   RE,NEXTEL                                                        
         BNE   GETPW34                                                          
*                                                                               
GETPW32  MVC   0(L'SVPWPCT,R1),2(R3)                                            
         LA    R1,L'SVPWPCT(R1)                                                 
         B     GETPW30                                                          
*                                                                               
GETPW34  MVC   0(2,R1),=X'FFFF'    SET HIGH DATE AT END                         
*                                                                               
GETPWX   J     XIT                                                              
         EJECT                                                                  
*======================================================================         
* SUBROUTINE EDITS DEMO OVERRIDE, LIMITED TO ONE DEMO IN ESTHDR                 
*                                                                               
* DEMOVAL USED FOR NSI DEMO ONLY.                                               
* COMSCORE DEMO VALIDATED VIA DEMO NAME LIST FROM ESTHDR                        
*======================================================================         
         SPACE 1                                                                
         USING DESTHDR,R9            DBLOCK FOLLOWS ESTHDR                      
VALDEM   NTR1                                                                   
         XC    DBLOCK(256),DBLOCK                                               
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAGPRF7,C'C'       TEST CANADIAN AGY                            
         BNE   VALDEM2                                                          
         CLI   SAVCLXTR,C'U'       TEST USING US DEMOS                          
         BE    VALDEM2                                                          
         MVI   DBSELMED,C'C'                                                    
*                                                                               
* NEED TO CONSTRUCT A DUMMY FLDHDR                                              
*                                                                               
VALDEM2  XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         MVI   0(RE),16            SET FLDHDR LEN                               
         MVI   5(RE),7             SET INPUT LEN                                
         MVC   8(8,RE),22(R4)      DATA                                         
*                                                                               
         CLI   22(R4),C'X'         COMSCORE IMP                                 
         JE    VALDEM10                                                         
         CLC   22(2,R4),=C'RX'     COMSCORE RATING                              
         JE    VALDEM10                                                         
*                                                                               
         XC    DMCB(8),DMCB                                                     
         GOTO1 VCALLOV,DMCB,,X'D9000AD9'  DEMOVAL                               
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,WORK),(1,WORK+20),(C'S',DBLOCK),WORK2               
         CLI   DMCB+4,1                                                         
         BNE   VALDEMX             EXIT WITH CC NEQ IF INVALID                  
*                                                                               
* DEMO NAME IS VALID                                                            
*                                                                               
VALDEM4  XC    SVDEMLST,SVDEMLST                                                
         MVC   SVDEMLST(3),WORK+20                                              
*                                                                               
* NOW NEED TO GET NAME IN STANDARD FORMAT                                       
*                                                                               
         XC    DMCB(8),DMCB                                                     
         GOTO1 VCALLOV,DMCB,,X'D9000AE0'  DEMOCON                               
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,SVDEMLST),(2,SVDEMNMS),DBLOCK                       
         CR    RE,RE               SET CC =                                     
         J     VALDEMX                                                          
*                                                                               
VALDEM10 L     RE,ASVNTDMS         POINT TO NONT DEMO NAMES                     
         LA    RF,20               SET COUNTER                                  
*                                                                               
VALDEM12 CLC   0(7,RE),22(R4)      NAME MATCH INPUT                             
         JE    VALDEM14                                                         
*                                                                               
         LA    RE,8(RE)                                                         
         JCT   RF,VALDEM12                                                      
         LTR   RB,RB               DEMO NOT VALID                               
         J     VALDEMX                                                          
*                                                                               
VALDEM14 MVC   SVDEMNMS,0(RE)      MOVE DEMO NAME                               
         XC    SVDEMLST(3),SVDEMLST                                             
         LA    R0,21                                                            
         SR    R0,RF               GIVES DEMO NUMBER IN LIST                    
         STC   R0,SVDEMLST+1       SET DEMO CODE (00NN00)                       
*                                                                               
         CR    RB,RB               SET CC EQ                                    
VALDEMX  J     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
*========================================================*                      
* SUBROUTINE EDITS DEMO MENU NAME                        *                      
*========================================================*                      
         SPACE 1                                                                
VALMENU  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D26'                                                  
         MVC   KEY+2(1),SAVBKEY                                                 
         MVC   KEY+3(4),22(R4)     MENU CODE                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VALMENX             EXIT WITH CC NEQ                             
         GOTO1 GETREC                                                           
*                                                                               
         LA    RE,SVDEMLST                                                      
         LA    RF,SVDEMNMS                                                      
         ZAP   HALF,=P'0'          CLEAR COUNTER                                
         LA    R1,24(R9)           POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
*                                                                               
VALMEN2  ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    VALMENX             NB - EXIT WITH CC EQ                         
         CLI   0(R1),5                                                          
         BNE   VALMEN2                                                          
         MVC   0(3,RE),2(R1)       MOVE DEMO CODE                               
         MVC   0(7,RF),5(R1)       MOVE DEMO NAME                               
         LA    RE,3(RE)                                                         
         LA    RF,7(RF)                                                         
         AP    HALF,=P'1'                                                       
         CP    HALF,=P'4'                                                       
         BL    VALMEN2                                                          
         CR    RE,RE               SET CC EQ                                    
VALMENX  J     XIT                                                              
         EJECT                                                                  
M1000    DS    0H                                                               
         OI    MISOPTH+4,X'20'     FLAG OPTIONS FIELD VALID                     
         BAS   RE,GETPW            GET PW INFO                                  
* KILL CPP GUIDES IF NOT APPROPRIATE                                            
         CLC   SAVGKEY(8),SAVGKEY2       A-M/C/PRD/MKT/EST                      
         BE    *+8                                                              
         MVI   POLGIND,0                                                        
         SPACE 1                                                                
* CHECK IF BUFFER HAS REQUESTED BUCKETS ALREADY                                 
         SPACE 1                                                                
         L     RE,ABUCKETS                                                      
         OC    0(256,RE),0(RE)     TEST ANY BUCKETS                             
         BZ    M1025                                                            
         CLC   SAVBKEY(3),SAVBKEY2      SAME AGY-MED-CLT?                       
         BNE   M1025                                                            
         CLC   SAVBKEY+4(6),SAVBKEY2+4  SAME MKT-STA-EST?                       
         BNE   M1025                                                            
         OC    SAVBKEY+6(3),SAVBKEY+6   IF STATION = ALL                        
         BNZ   M1010                                                            
         OC    SAVBKEY2+6(3),SAVBKEY2+6    & PREVIOUS STATION = ALL             
         BZ    M1025                       PROCESS AGAIN (ALL -> ALL/)          
*                                                                               
M1010    CLC   SAVPER(12),SAVPER2       SAME REQUESTED PERIOD?                  
         BE    *+12                                                             
         MVI   POLGIND,0                KILL CPP GUIDE                          
         B     M1025                                                            
*                                                                               
         CLC   WEEKIND,WEEKIND2         SAME WEEK INDICATOR?                    
         BNE   M1025                                                            
*                                                                               
         CLI   WEEKIND,C'W'             WEEKS?                                  
         BNE   M1020                                                            
*                                                                               
M1015    CLC   SAVBKEY+3(1),SAVBKEY2+3 SAME PRODUCT?                            
         BNE   M1025                                                            
         B     M1050               SKIP PROCESSING                              
*                                                                               
M1020    DS    0H                  NO WEEKS REQUESTED                           
         CLI   SAVBKEY2+3,X'FF'    OLD BUCKETS HAVE POOL (ALL) PRDS?            
         BNE   M1015                                                            
         CLC   SVDEMLST,SVDEMLS2                                                
         BNE   M1025                                                            
         B     M1050                                                            
*                                                                               
M1025    CLC   SAVPER(12),SAVPER2       SAME REQUESTED PERIOD?                  
         BE    *+8                                                              
         MVI   POLGIND,0                KILL CPP GUIDE                          
*                                                                               
         GOTO1 VCALLOV,DMCB,(1,0),(RA)   CALL T20B01 TO PROCESS RECS            
*                                                                               
         CLI   DMCB,X'FF'          CALLOV ERROR?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC)      GO TO PROCESS GOALS AND BUYS                 
         LA    R2,MISMEDH                                                       
         CLI   ERRAREA,X'FF'       ERROR?                                       
         BE    EXIT                                                             
*                                                                               
M1050    DS    0H                                                               
         GOTO1 VCALLOV,DMCB,(2,0),(RA)  CALL T20B02 OVERLAY                     
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC)      DISPLAY BUCKETS ON SCREEN                    
*                                                                               
M1075    DS    0H                                                               
         LA    R2,MISMEDH                                                       
         CLI   ERRAREA,X'FF'       ERROR?                                       
         BE    EXIT                                                             
*       - - - - - - - - - - - - - -                                             
         LA    R2,MISFORH                CURSOR                                 
         MVC   MISMSG,SPACES                                                    
         TM    PCDRIVEN,PCGRIDQ          GRIDS?                                 
         BZ    *+12                      . NO                                   
         BRAS  RE,SETGMSG                . YES, SET GRID MESSAGE                
         BE    *+10                                                             
         MVC   MISMSG,MIS                STANDARD MIS MESSAGE                   
         FOUT  MISMSGH                                                          
*       - - - - - - - - - - - - - -                                             
*                                                                               
EXIT     OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
EXXMOD   DS    0H                                                               
         CLC   SVXFRSYS(6),=C'SPOBUC'                                           
         BE    EXXMOD4                                                          
         CLC   SVXFRSYS(6),=C'SPOSDE'                                           
         BE    EXXMOD4                                                          
         CLC   SVXFRSYS(6),=C'SPOINC'                                           
         BE    EXXMOD4                                                          
         CLC   SVXFRSYS(6),=C'SPONWS'                                           
         BNE   EXXMODX                                                          
         MVC   MISMSG,TOTACC       RETURN TOTACCS IN MSG FIELD                  
EXXMOD4  DS    0H                                                               
         L     RF,VCOMFACS         SET SCREEN MSG FIELD IN GLOBBER              
         L     RF,CGLOBBER-COMFACSD(RF)  (FOR $BUCH)                            
         GOTO1 (RF),DMCB,=C'PUTD',MISMSG,60,GLVSMSG                             
EXXMODX  DS    0H                                                               
         J     XIT                                                              
*                                                                               
PRDIDROP MVI   ERRCD,NEWERRS       CANNOT USE A 2ND PRD 2/ IDR OPTION           
         MVC   NERRCD,=AL2(NPRDIDR)                                             
*                                                                               
MISERR   GOTO1 ERROR               (DOES NOT RETURN)                            
*                                                                               
ERRNODPT EQU   859                 ESTIMATE'S DAYPART MENU NOT FOUND            
*                                                                               
MIS      DC    28C' '                                                           
         DC    CL32'MEDIA INFORMATION SYSTEM'                                   
SPACES   DC    60C' '                                                           
*                                                                               
*----------------------------------------------------------------------         
* CHECK RUNNINGS WITH THE GRIDS                                                 
*----------------------------------------------------------------------         
CHKGRIDS NTR1                                                                   
         TM    PCDRIVEN,PCMODEQ    RUNNING UNDER PC SPOTPAK                     
         BZ    CGX                                                              
*                                  IF INPUT FIELDS CHANGED                      
         TM    MISMEDH+4,X'20'     MEDIA                                        
         BZ    CG4                                                              
         TM    MISCLTH+4,X'20'     CLIENT                                       
         BZ    CG4                                                              
         TM    MISPRDH+4,X'20'     PRODUCT                                      
         BZ    CG4                                                              
         TM    MISESTH+4,X'20'     ESTIMATE                                     
         BZ    CG4                                                              
         TM    MISMKTH+4,X'20'     MARKET                                       
         BZ    CG4                                                              
         CLC   =C'LIST',MISMKT     LOOK IF MARKET LIST CALL                     
         BE    CGX                                                              
         TM    MISSTAH+4,X'20'     STATION                                      
         BZ    CG4                                                              
         TM    MISDPTH+4,X'20'     DAYPART                                      
         BZ    CG4                                                              
         TM    MISLENH+4,X'20'     LENGTH                                       
         BZ    CG4                                                              
         TM    MISPERH+4,X'20'     PERIOD                                       
         BZ    CG4                                                              
         TM    MISFORH+4,X'20'     FORMAT                                       
         BZ    CG4                                                              
         TM    MISOPTH+4,X'20'     OPTIONS                                      
         BZ    CG4                                                              
         B     CGX                                                              
*                                                                               
CG4      XC    SVGLCTR,SVGLCTR            RESET SAVED LINE COUNTER              
         OI    PCDRIVEN,PCIL1Q+PCGDEFQ    INFO LINE ONE DISPLAY+C DEFS          
         NI    PCDRIVEN,X'FF'-PCGFINQ     START OVER WITH GRIDS                 
         CLC   =C'GRID',MISMED     TEST CONNECT TO GRID?                        
         BE    CG10                                                             
         CLC   =C'GOFF',MISMED     TEST DISCONNECT FROM GRID?                   
         BNE   CGX                                                              
         NI    PCDRIVEN,X'FF'-PCGRIDQ                                           
         B     *+8                                                              
CG10     OI    PCDRIVEN,PCGRIDQ    PASS GRID SCREEN LINE                        
         MVC   MISMED,SPACES       CLEAR AND ENSURE TRANSIMT FLD                
         FOUT  MISMEDH                                                          
         MVC   MISMSG,MIS                                                       
         FOUT  MISMSGH                                                          
         LTR   RB,RB               EXIT NOT EQUAL                               
         B     *+6                                                              
CGX      CR    RB,RB                                                            
         J     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
NEXTEL   SR    R0,R0                                                            
         CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         ICM   R0,1,1(R3)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         CLC   0(1,R3),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R3),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE               SET CC EQUAL                                 
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  SET CC NOT EQ                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,R8                                                            
         USING *,RF                                                             
SPCOMMON NTR1  BASE=BASERB                                                      
         DROP  RF                                                               
         USING T20B00,RB,R8                                                     
*                                                                               
         L     R8,BASER8           RESTORE SECOND BASE REG                      
         SRL   RF,24                                                            
         B     SPCOMTAB(RF)                                                     
*                                                                               
SPCOMTAB B     SPERROR             X'00'                                        
         B     SPANY               X'04'                                        
         B     SPMOVE              X'08'                                        
         B     SPPACK              X'0C'                                        
         B     SPREAD              X'10'                                        
         B     SPSEQ               X'14'                                        
         B     SPHIGH              X'18'                                        
         B     SPADD               X'1C'                                        
         B     SPDIR               X'20'                                        
         B     SPRDSTA             X'24'                                        
         B     SPSTA               X'28'                                        
         B     SPGETREC            X'2C'                                        
         B     SPPUTREC            X'30'                                        
         B     SPADDREC            X'34'                                        
         B     SPFIL               X'3C'                                        
SPCOMUSR DC    5AL4(0)   ** USER ROUTINES 10 - 14 ORIGIN HERE **                
         DC    9AL4(0)   ** USER ROUTINES 01 - 09 ORIGIN HERE **                
SPCOMCNT EQU   (*-SPCOMTAB)/4      NUMBER OF ENTRIES                            
         SPACE 1                                                                
SPCOMXIT J     XIT                                                              
         SPACE 2                                                                
SPERROR  L     R4,ERRAREA                                                       
         CLI   ERRAREA,0           TEST FOR PRESET MESSAGE                      
         BNE   SPERRORX                                                         
         CLI   ERRCD,NEWERRS       IS THIS A 2 CHAR ERROR                       
         BNE   SPERR10                                                          
*                                                                               
         XC    WORK,WORK           DEFINE CONTROL BLOCK                         
         LA    R1,WORK                                                          
         USING GETTXTD,R1                                                       
         MVI   GTMTYP,GTMERR       SET MESSAGE TYPE TO ERRO                     
         MVC   GTMSGNO,NERRCD      AND MESSAGE NUMBER                           
         MVI   GTMSYS,2            AND MESSAGE SYSTEM                           
         DROP  R1                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         GOTO1 (RF),WORK           PUT OUT SYSTEM MESSAGE                       
         B     SPERR20                                                          
*                                                                               
SPERR10  MVI   ERRAREA,X'FF'                                                    
* READ SYSTEM ** 2 ** MESSAGES                                                  
         GOTO1 VGETMSG,DMCB+12,(ERRCD,8(R4)),(X'FF',DMCB),(2,0)                 
*                                                                               
SPERR20  FOUT  (R4)                                                             
         CLC   SVXFRSYS(6),=C'SPOBUC'                                           
         BE    SPERR22                                                          
         CLC   SVXFRSYS(6),=C'SPOSDE'                                           
         BE    SPERR22                                                          
         CLC   SVXFRSYS(6),=C'SPOINC'                                           
         BNE   SPERRORX                                                         
SPERR22  DS    0H                                                               
         L     RF,VCOMFACS         SET SCREEN MSG FIELD IN GLOBBER              
         L     RF,CGLOBBER-COMFACSD(RF)  (FOR $BUCH OR $INCH)                   
         GOTO1 (RF),DMCB,=C'PUTD',MISMSG,60,GLVSMSG                             
*                                                                               
SPERRORX OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
         L     RD,BASERD           EXIT PROGRAM                                 
         L     RD,4(RD)            POINT BACK TO MONITORS REGS                  
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
SPANY    CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         MVI   ERRCD,1                                                          
         B     SPERROR                                                          
*                                                                               
ANY2     TM    4(R2),X'10'                                                      
         BZ    SPCOMXIT                                                         
         MVI   ERRCD,3                                                          
         SPACE 2                                                                
SPPACK   SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1               EXIT ON ZERO LENGTH                          
         BZ    PACKX                                                            
         TM    4(R2),X'08'         OR NON-NUMERIC                               
         BZ    PACKX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+12                                                          
         CVB   R0,DUB                                                           
         B     PACKX                                                            
         PACK  DUB,8(0,R2)   * EXECUTED *                                       
*                                                                               
PACKX    XIT1  REGS=(R0,R1)                                                     
         SPACE 2                                                                
SPMOVE   MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    SPCOMXIT                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     PACKX                                                            
*                                                                               
         MVC   WORK(0),8(R2) * EXECUTED *                                       
         EJECT                                                                  
SPREAD   MVC   COMMAND,=C'DMREAD'                                               
         B     SPDIR                                                            
SPSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         B     SPDIR                                                            
SPHIGH   MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     SPDIR                                                            
SPADD    MVC   COMMAND,=C'DMADD'                                                
         B     SPDIR                                                            
SPWRITE  MVC   COMMAND,=C'DMWRT'                                                
*                                                                               
SPDIR    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTDIR',KEY,KEY               
*                                                                               
SPDIRX   TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         BZ    SPDIRX2             NO ERROR                                     
         CLI   COMMAND+2,C'R'      TEST READ COMMAND                            
         BE    *+6                                                              
         DC    H'0'                FORCE RECOVERY ON ADD/WRITE ERROR            
*                                                                               
SPDIRX2  MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,8(R1)                                                       
         BZ    SPCOMXIT                                                         
* DATAMGR ERROR HAS OCCURRED                                                    
         MVI   ERRCD,0                                                          
         B     SPERROR                                                          
*                                                                               
SPRDSTA  MVC   COMMAND,=C'DMREAD'                                               
*                                                                               
SPSTA    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'STATION',KEY,AIO              
         B     SPDIRX                                                           
*                                                                               
SPGETREC MVC   COMMAND,=C'GETREC'                                               
         B     SPFIL                                                            
*                                                                               
SPPUTREC MVC   COMMAND,=C'PUTREC'                                               
         B     SPFIL                                                            
*                                                                               
SPADDREC MVC   COMMAND,=C'ADDREC'                                               
*                                                                               
SPFIL    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'SPTFILE',KEY+14,AIO, X        
               DMWORK                                                           
         TM    8(R1),X'D0'         TEST EOF OR ERROR                            
         BZ    SPDIRX2                                                          
         DC    H'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
INITL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         MVC   VDATAMGR(80),0(R4)  FACILITY LIST                                
         MVC   VCOMFACS,16(R1)     ** SAVE COMFACS ADDRESS **                   
         LM    R2,R3,0(R1)                                                      
         ST    R2,VTIOB                                                         
         ST    R3,VTWA                                                          
*                                                                               
         MVC   AGYALPHA,14(R3)                                                  
         LA    R3,64(R3)           PRESET ERROR MSG ADDRESS                     
         ST    R3,ERRAREA                                                       
         MVI   DMINBTS,X'C0'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         LA    R0,IOAREA                                                        
         ST    R0,AIO                                                           
* SET UP COMMON FACILITY LINKAGES                                               
         LA    R3,SPCOMMON                                                      
         SR    R4,R4                                                            
         LA    R5,ERROR                                                         
         LA    R0,SPCOMCNT                                                      
*                                                                               
INIT10   ST    R3,0(R5)            SET A(SPCOMMON)                              
         STC   R4,0(R5)            SET BRANCH TABLE DSPL                        
         LA    R4,4(R4)            BUMP DSPL                                    
         LA    R5,4(R5)            NEXT SUBR                                    
         BCT   R0,INIT10                                                        
*                                 GET ADDRESS OF MISTWA2 INTO AMISTWA2          
         SR    R0,R0                                                            
         LH    R0,=AL2(MISTWA2-GENOLD)                                          
         AR    R0,RC                                                            
         ST    R0,AMISTWA2                                                      
*                                                                               
         MVC   TWASIZE,=H'12288'   LIMITED BY BUFFER LENGTH                     
*                                                                               
         NI    PCDRIVEN,X'FF'-PCMODEQ                                           
         XC    GLCTR,GLCTR         RESET LINE COUNTER                           
         L     RF,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)            RF=A(SYSTEM DEFINITION BLOCK)                
         TM    FATSTAT6-FACTSD(RF),X'80'                                        
         BZ    *+8                                                              
         OI    PCDRIVEN,PCMODEQ+PCGINIQ  (PC SPOTPAK)+(INIT DOWNLOAD)           
*                                                                               
         L     R1,VTIOB                                                         
         USING TIOBD,R1                                                         
         SR    R0,R0                                                            
         IC    R0,TIOBAID                                                       
         CHI   R0,12                                                            
         BNH   *+8                                                              
         AHI   R0,-12                                                           
         STC   R0,PFKEY                                                         
*                                                                               
         MVI   DMCB+7,QSTAPACK                                                  
         BAS   RE,MCALLOV                                                       
         MVC   VSTAPACK,0(R1)                                                   
*                                                                               
         MVI   DMCB+7,QPWCALC                                                   
         BAS   RE,MCALLOV                                                       
         MVC   VPWCALC,0(R1)                                                    
*                                                                               
         L     RE,=A(FORMGRID)                                                  
         A     RE,FULL                                                          
         ST    RE,VFORMGRD                                                      
*                                                                               
         L     RE,=A(CPP)                                                       
         A     RE,FULL                                                          
         ST    RE,VCPP                                                          
*                                                                               
         L     RE,=A(PERACHMT)                                                  
         A     RE,FULL                                                          
         ST    RE,VPRACHMT                                                      
*                                                                               
         LHI   RE,SVNETLST-T20BFFD                                              
         AR    RE,RA                                                            
         ST    RE,ANETLST                                                       
*                                                                               
         LHI   RE,DLCB-T20BFFD                                                  
         AR    RE,RA                                                            
         ST    RE,ADLCB                                                         
*                                                                               
         LHI   RE,SVNTDMS-T20BFFD                                               
         AR    RE,RA                                                            
         ST    RE,ASVNTDMS                                                      
*                                                                               
         LHI   RE,SVUSRNMS-T20BFFD                                              
         AR    RE,RA                                                            
         ST    RE,ASVUSRNMS                                                     
         XIT1                                                                   
*                                                                               
MCALLOV  LR    R0,RE                                                            
         XC    DMCB(4),DMCB                                                     
         BRAS  RE,*+8                                                           
         DC    X'D9000A00'                                                      
         MVC   DMCB+4(3),0(RE)                                                  
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* READ PROFILES                                                                 
*============================================================                   
                                                                                
RDPROFS  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK           READ 00 PROFILE                              
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),MISMED                                                 
         MVC   WORK+7(3),MISCLT                                                 
         OI    WORK+9,C' '                                                      
         MVC   WORK+16(6),WORK    SAVE KEY THROUGH AGY                          
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),(R1),WORK,WORK2,VDATAMGR                                    
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   SVSPPRF2,WORK2+2    SAVE BYTES 2/6-8                             
         MVC   SVSPPRF6(3),WORK2+6                                              
*                                                                               
         XC    WORK(16),WORK                                                    
         MVC   WORK(6),WORK+16     RESTORE KEY THRU AGY                         
         GOTO1 (RF),(R1),(X'90',WORK),WORK2   DO NOT READ UID PROFILES          
         MVC   SVSPPRF9,WORK2+9                                                 
         MVC   SVSPPRFA,WORK2+10                                                
*                                                                               
         XC    WORK,WORK           READ 00A PROFILE                             
         MVC   WORK(4),=C's00A'    THE s HAS TO BE LOWER CASE                   
         MVC   WORK+4(2),AGYALPHA                                               
         GOTO1 (RF),(R1),(X'90',WORK),WORK2   DO NOT READ UID PROFILES          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   SV00APRF6,WORK2+6    SAVE BYTE 6                                 
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0B0'    READ B0 PROFILE                              
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),MISMED                                                 
         MVC   WORK+7(3),MISCLT                                                 
         OI    WORK+9,C' '                                                      
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         XC    WORK2,WORK2         CLEAR IN CASE NOT FOUND                      
         GOTO1 (RF),(R1)                                                        
         MVC   SVB0PROF,WORK2                                                   
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*========================================================*                      
* VALIDATE STATION                                       *                      
*========================================================*                      
VALSTA   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MISSTAH                                                       
         MVI   ERRCD,18                                                         
         TM    4(R2),X'20'                                                      
         BO    VALSTAEQ                                                         
         XC    SAVBKEY+6(3),SAVBKEY+6                                           
         CLI   5(R2),0             TEST NO INPUT                                
         BNE   VALSTA2                                                          
         CLI   MISMKTH+5,0         TEST MARKET WAS INPUT                        
         BE    VALSTA2             IF NOT, ALL BE BAD                           
         MVC   8(3,R2),=C'ALL'     LISA BE THINKIN THAT ALL BE GOOD             
         MVI   5(R2),3                                                          
         FOUT  (R2)                                                             
*                                                                               
VALSTA2  CLC   MISSTA(3),=C'ALL'                                                
         BNE   VALSTA10                                                         
         CLI   MISSTA+3,C' '       ONLY ALLOW ALL,ALL- & ALL/                   
         BNH   VALSTAEQ                                                         
         CLI   MISSTA+3,C'-'                                                    
         BE    VALSTAEQ                                                         
         CLI   MISSTA+3,C'/'                                                    
         BE    VALSTAEQ                                                         
         B     MISERR                                                           
*                                                                               
VALSTA10 CLC   MISSTA(4),=C'LIST'                                               
         BNE   VALSTA12                                                         
         MVC   SAVBKEY+6(3),=3X'FF'                                             
         CLI   MISMED,C'N'                                                      
         BNE   VALSTAEQ                                                         
         CLI   SVAGPRF7,C'C'       TEST CANADIAN                                
         BNE   VALSTAEQ                                                         
* SEE IF WE NEED A NEW LIST                                                     
         L     RE,ANETLST                                                       
         OC    0(256,RE),0(RE)     TEST ANYTHING IN LIST ALREADY                
         BNZ   *+8                                                              
         BAS   RE,BLDNETS          BUILD LIST OF NETWORKS                       
         B     VALSTAEQ                                                         
*                                                                               
VALSTA12 XC    IOAREA(32),IOAREA                                                
         LA    R4,WORK3                                                         
         USING STABLKD,R4                                                       
         XC    0(STBLNQ,R4),0(R4)                                               
*                                                                               
         ST    R2,STBADDR          CALL STAVAL                                  
         MVC   STBMED,MISMED                                                    
         MVC   STBCTRY,SVAGPRF7                                                 
         MVC   STBACOM,VCOMFACS                                                 
         GOTO1 VCALLOV,DMCB,0,X'D9000A68'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),STABLKD                                                
*                                                                               
         CLI   STBSTA+4,C' '       TEST CABLE                                   
         BNE   *+8                                                              
         MVI   STBSTA+4,C'T'       TREAT CABLE AS SPOT TV                       
         MVC   DUB(8),STBSTA       SAVE STATION/NETWORK                         
*                                                                               
         CLI   STBERR,0                                                         
         BNE   VALSTANQ                                                         
*                                                                               
         CLI   MISMED,C'N'                                                      
         BNE   VALSTA30                                                         
         CLI   SVAGPRF7,C'C'       TEST CANADIAN                                
         BNE   VALSTA30                                                         
         CLI   STBNET,C' '         TEST FOR CABLE NETWORK                       
         BNH   VALSTA30                                                         
*                                                                               
         BRAS  RE,GETCBNET         GET CBLDEF FOR MARKET NUMBER                 
         BNE   MISERR                                                           
         LH    R0,HALF             GET RETURNED MARKET NUM                      
         XC    HALF,HALF           AND CLEAR FOR COMPARE BELOW                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVMKT,DUB                                                        
         MVC   DUB(8),STBSTA       RESTORE DUB                                  
         B     VALSTA32                                                         
         DROP  R4                                                               
*                                                                               
VALSTA30 MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),MISMED                                                  
         MVC   KEY+2(5),DUB                                                     
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   KEY+9(3),MISCLT                                                  
         OC    KEY+9(3),SPACES                                                  
*                                                                               
         GOTO1 RDSTA                                                            
         USING DSTAREC,R9                                                       
*                                                                               
         OC    SVMKT,SVMKT         MARKET OVERRIDE                              
         BNZ   *+10                                                             
         MVC   SVMKT,SMKT                                                       
         MVC   HALF,SAVBKEY+4    SAVE MARKET                                    
         DROP  R9                                                               
*                                                                               
VALSTA32 LA    R1,WORK3                                                         
         USING STAPACKD,R1                                                      
         XC    0(32,R1),0(R1)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,SVAGPRF7                                                
         MVC   STAPMED,MISMED                                                   
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPQMKT,SVMKT                                                   
         MVC   STAPQSTA(8),DUB                                                  
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    VALSTA34                                                         
         MVI   ERRCD,18            SET INVALID STATION                          
         LA    R2,MISSTAH                                                       
         NI    MISSTAH+4,X'DF'                                                  
         B     VALSTANQ                                                         
*                                                                               
VALSTA34 MVC   SAVBKEY+4(5),STAPMKST                                            
         DROP  R1                                                               
*                                                                               
         CLI   MISMED,C'C'                                                      
         BNE   VALSTA40                                                         
         MVI   SAVBKEY+8,0                                                      
*                                                                               
VALSTA40 MVC   SAVGKEY+5(2),SAVBKEY+4  MARKET TO GOAL KEY                       
         CLC   SAVBKEY+4(2),HALF       SAME MARKET?                             
         BE    VALSTAEQ                                                         
*                                                                               
         MVC   KEY+2(4),SVMKT                                                   
         FOUT  MISMKTH,SVMKT,4                                                  
*                                                                               
         LA    R2,MISMKTH                                                       
         BRAS  RE,GETMKT           GET MARKET RECORD                            
         BNE   VALSTANQ                                                         
*                                                                               
VALSTAEQ OI    MISSTAH+4,X'20'     STATION VALID                                
         CR    RE,RE               SET EQUAL CC                                 
         B     VALSTAX                                                          
*                                                                               
VALSTANQ LTR   RB,RB               SET CC NOT EQ                                
*                                                                               
VALSTAX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T20BFFD+6                                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,SVOFFC                                                    
         MVC   OFCCLT,MISCLT                                                    
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,SAVBKEY                                                 
         MVC   OFCLMT(4),T20BFFD+6                                              
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKTREC                   
         LR    R0,RA                                                            
         AHI   R0,SECBLK-T20BFFD                                                
         ST    R0,OFCSECD                                                       
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK2),VCOMFACS                                  
         MVI   ERRCD,ERRLOCK                                                    
         CLI   0(R1),0                                                          
         JE    EXXMODX                                                          
         GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* SUBROUTINE BUILDS LIST OF NETWORK CALL LETTERS                                
*=========================================================                      
         SPACE 1                                                                
K        USING NWKPASS,KEY                                                      
*                                                                               
BLDNETS  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVI   K.NWKPTYP,NWKPTYPQ                                               
         MVI   K.NWKPSUB,NWKPSUBQ                                               
         MVC   K.NWKPAGY,AGYALPHA                                               
         GOTO1 HIGH                                                             
         B     BLDNET4                                                          
*                                                                               
BLDNET2  GOTO1 SEQ                                                              
*                                                                               
BLDNET4  CLC   KEY(4),KEYSAVE                                                   
         BNE   BLDNETX                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,K.NWKPSEQ                                                     
         BCTR  RE,0                                                             
         SLL   RE,2                X 4                                          
         A     RE,ANETLST                                                       
         MVC   0(4,RE),K.NWKPNET                                                
         B     BLDNET2                                                          
*                                                                               
BLDNETX  J     XIT                                                              
         DROP  K                                                                
         LTORG                                                                  
         EJECT                                                                  
*========================================================*                      
* SUBROUTINE READS MARKET RECORD AND VALIDATES ACCESS *                         
*========================================================*                      
         SPACE 1                                                                
GETMKT   NTR1  BASE=*,LABEL=*                                                   
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),MISMED     MEDIA                                        
         SR    R0,R0                                                            
         ICM   R0,3,SAVBKEY+4      GET MARKET FROM BUY KEY                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),AGYALPHA                                                
         MVI   KEY+8,C'0'                                                       
         MVC   KEY+9(8),KEY+8                                                   
*                                                                               
         GOTO1 RDSTA                                                            
*                                                                               
         USING DMKTREC,R9                                                       
         FOUT  MISMKTNH,MKTNAME,23                                              
         MVC   SVMACCS(3),MKTLTACC SAVE LIMIT ACCESS CODES                      
*                                                                               
         CLI   T20BFFD+6,C'+'      TEST LIMIT ACCESS BY MKT                     
         BE    GETMKT10                                                         
         CLI   T20BFFD+8,C'+'      ALTERNATE LOCATION                           
         BE    GETMKT10                                                         
         B     GETMKTEQ                                                         
*                                                                               
GETMKT10 MVI   ERRCD,ERRLOCK                                                    
         BRAS  RE,CALLOFCR                                                      
*                                                                               
GETMKTEQ CR    RB,RB                                                            
*                                                                               
GETMKTX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* TEST FOR TRANSFER CONTROL AND GET INPUT FIELDS                                
*==========================================================                     
         SPACE 1                                                                
GETGLOB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,VCOMFACS                                                      
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    GETGLBX                                                          
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',WORK3,GLVXLENQ,GLVXCTL                        
         CLI   8(R1),0                                                          
         BNE   GETGLBX                                                          
*                                                                               
         MVC   SVXFRSYS,WORK3      SAVE FROM SYSTEM                             
         MVC   SVXFRPGM,WORK3+3          AND PROGRAM                            
* GET RID OF IT QUICKLY !                                                       
         GOTO1 (RF),(R1),=C'DELE'                                               
* IF ACTIVATED BY XFRCTL, MAKE SURE TO ALWAYS RETURN TO CALLER                  
         CLC   SVXFRCTL,=C'SPOBUC'                                              
         BE    GETGLB2                                                          
         CLC   SVXFRCTL,=C'SPOSDE'                                              
         BE    GETGLB2                                                          
         CLC   SVXFRCTL,=C'SPOINC'                                              
         BE    GETGLB2                                                          
         CLC   SVXFRCTL,=C'SPONWS'                                              
         BNE   GETGLB4                                                          
GETGLB2 DS     0H                                                               
         XC    WORK3,WORK3                                                      
         LA    R1,WORK3                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'MIS'                                                 
         MVC   GLVXTOSY,SVXFRSYS                                                
         MVC   GLVXTOPR,SVXFRPGM                                                
         OI    GLVXFLG1,GLV1RETN+GLV1RETG                                       
         DROP  R1                                                               
         GOTO1 (RF),DMCB,=C'PUTD',WORK3,GLVXLENQ,GLVXCTL                        
*                                                                               
GETGLB4 DS     0H                                                               
         GOTO1 (RF),(R1),=C'GETF',MISMEDH,,GLVSPMD                              
         GOTO1 (RF),(R1),,MISCLTH,,GLVSPCLT                                     
         GOTO1 (RF),(R1),,MISPRDH,,GLVSPPRD                                     
         GOTO1 (RF),(R1),,MISESTH,,GLVSPEST                                     
         GOTO1 (RF),(R1),,MISMKTH,,GLVSPMKT                                     
         GOTO1 (RF),(R1),,MISSTAH,,GLVSPSTA                                     
         GOTO1 (RF),(R1),,MISDPTH,,GLVSPDPT                                     
         GOTO1 (RF),(R1),,MISFORH,,GLVSPFMT                                     
         GOTO1 (RF),(R1),=C'DELE'        DELETE FORMAT REQUEST NOW              
*                                                                               
         LA    RE,=C'GETF'                                                      
         ST    RE,0(R1)            AND RESTORE GETF                             
*                                                                               
         CLC   SVXFRCTL,=C'SPONWS'                                              
         BNE   GETGLB6                                                          
*                                                                               
         LA    RE,MISSTAH                                                       
         MVC   8(3,RE),=C'ALL'                                                  
         NI    4(RE),X'DF'                                                      
         MVI   5(RE),3                                                          
         GOTO1 (RF),(R1),,MISPERH,,GLVSPPER   CAMPAIGN PERIOD                   
*                                                                               
GETGLB6 CLC    =C'SPOBUC',SVXFRSYS          IS GRANT CALLING                    
         BE     GETGLB8                                                         
         CLC   =C'SPOSDE',SVXFRSYS          SUPERDESK?                          
         BE     GETGLB8                                                         
         CLC   =C'SPOINC',SVXFRSYS                                              
         BNE    GETGLB10                                                        
*                                                                               
GETGLB8 DS     0H                                                               
*&&DO*&& GOTO1 (RF),(R1),,MISFORH,,GLVSPFMT   MOVED UP                          
         GOTO1 (RF),(R1),,MISPERH,,GLVSPPER                                     
         GOTO1 (RF),(R1),,MISOPTH,,GLVSPOPT                                     
         B     GETGLBX                                                          
*                                                                               
GETGLB10 LA    R1,MISPERH                                                       
         MVC   8(2,R1),=C'ES'                                                   
         NI    4(R1),X'DF'                                                      
         MVI   5(R1),2                                                          
*                                                                               
GETGLBX  L     RE,VTWA             SET CLEAR BEFORE/AFTER                       
         AHI   RE,64                                                            
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)                                                       
         BZ    *+8                                                              
         BXH   RE,R1,*-8                                                        
         MVC   0(3,RE),=X'000101'                                               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* SET UP XFRCTL ELEMENT FOR RETURN TO CALLING PROGRAM                           
*===================================================================            
         SPACE 1                                                                
SETRTRN  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK3,WORK3                                                      
         LA    R1,WORK3                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'MIS'                                                 
         MVC   GLVXTOSY,SVXFRSYS                                                
         MVC   GLVXTOPR,SVXFRPGM                                                
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS+GLV1RETG                              
         DROP  R1                                                               
         L     RF,VCOMFACS         SET SCREEN MSG FIELD IN GLOBBER              
         L     RF,CGLOBBER-COMFACSD(RF)  (FOR $BUCH)                            
         GOTO1 (RF),DMCB,=C'PUTD',WORK3,GLVXLENQ,GLVXCTL                        
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* READ CANADIAN CBLNET RECORD AND EXTRACT MARKET NUMBER INTO HALF               
* ON ENTRY DUB(4) IS STATION, DUB+5(2) IS MARKET                                
*===================================================================            
         SPACE 1                                                                
GETCBNET NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRCD,18         INVALID STATION                                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),DUB                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETCBNEQ                                                         
* TRY FOR CLIENT EXCEPTION REC                                                  
         MVC   WORK(20),KEY        SAVE DEFAULT KEY                             
         MVC   KEY+8(2),SAVBKEY+1                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GETCB2                                                           
         MVC   KEY,WORK            RESTORE KEY                                  
*                                                                               
         USING NDEFRECD,R8                                                      
GETCB2   GOTO1 GETREC                                                           
         SPACE 1                                                                
* RETRIEVE NTWKID ELEM                                                          
         SPACE 1                                                                
         LA    R6,IOAREA+24                                                     
         SR    R0,R0                                                            
GETCB10  CLI   0(R6),X'02'                                                      
         BE    GETCB12                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETCB10                                                          
         DC    H'0'                                                             
*                                                                               
GETCB12  CLI   2(R6),1             IS IT A CBLDEF                               
         BNE   GETCBNEQ                                                         
*                                                                               
         LA    R6,IOAREA+24                                                     
*                                                                               
GETCB20  CLI   0(R6),1                                                          
         BNE   GETCB22                                                          
         USING NDEFEL01,R6                                                      
*                                                                               
         CLC   DUB+5(2),NDEFMSUF   MATCH MARKET                                 
         BE    GETCB24                                                          
*                                                                               
GETCB22  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETCB20                                                          
         B     GETCBNEQ                                                         
*                                                                               
GETCB24  MVC   HALF(2),NDEFMNUM    RETURN MARKET NUMBER                         
         CR    RB,RB               SET CC EQ                                    
         B     GETCBX                                                           
*                                                                               
GETCBNEQ LTR   RB,RB                                                            
*                                                                               
GETCBX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* IF NO INPUT FIELDS CHANGED, HANDLE PFKEY REQUEST                              
*=============================================================                  
                                                                                
CHKPFKEY NTR1  BASE=*,LABEL=*                                                   
         CLI   PFKEY,1             TEST TOP                                     
         BE    CHKPFK2                                                          
         CLI   PFKEY,3             TEST NEXT                                    
         BE    CHKPFK2                                                          
         CLI   PFKEY,12            PF12=RETURN (NOT HANDLED HERE)               
         BE    CHKPFKX                                                          
         MVI   PFKEY,0                                                          
         B     CHKPFKX                                                          
*                                                                               
CHKPFK2  TM    MISMEDH+4,X'20'                                                  
         BZ    CHKPFK10                                                         
         TM    MISCLTH+4,X'20'                                                  
         BZ    CHKPFK10                                                         
         TM    MISPRDH+4,X'20'                                                  
         BZ    CHKPFK10                                                         
         TM    MISESTH+4,X'20'                                                  
         BZ    CHKPFK10                                                         
         TM    MISMKTH+4,X'20'                                                  
         BZ    CHKPFK10                                                         
         TM    MISSTAH+4,X'20'                                                  
         BZ    CHKPFK10                                                         
         TM    MISDPTH+4,X'20'                                                  
         BZ    CHKPFK10                                                         
         TM    MISLENH+4,X'20'                                                  
         BZ    CHKPFK10                                                         
         TM    MISPERH+4,X'20'                                                  
         BZ    CHKPFK10                                                         
         B     CHKPFK20                                                         
*                                                                               
CHKPFK10 MVI   PFKEY,0             SUPPRESS PFKEY                               
         B     CHKPFKX                                                          
*                                                                               
CHKPFK20 SR    R0,R0                                                            
         CLI   PFKEY,1             TEST TOP                                     
         BNE   CHKPFK22                                                         
         LHI   R0,1                                                             
         B     CHKPFK24                                                         
*                                                                               
CHKPFK22 IC    R0,PAGENUM                                                       
         AHI   R0,1                                                             
*                                                                               
CHKPFK24 DS    0H                                                               
         EDIT  (R0),(2,MISPAGE),ALIGN=LEFT                                      
         STC   R0,MISPAGEH+5                                                    
*&&DO                                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         XC    MISPAGE,MISPAGE                                                  
         UNPK  MISPAGE(2),DUB                                                   
         MVI   MISPAGEH+5,2                                                     
*&&                                                                             
         NI    MISPAGEH+4,X'DF'    UNSET PREV VALIDATED                         
         OI    MISPAGEH+4,X'08'    SET VALID NUMERIC                            
         OI    MISPAGEH+6,X'80'    VALIDATE IT                                  
*                                                                               
CHKPFKX  J     XIT                                                              
         EJECT                                                                  
*======================================================================         
* PERCENTAGE ACHIEVED ROUTINE'                                                  
*   P1=A(PURCHASED PNTS OR DOLLARS)                                             
*   P2=A(GOAL PNTS OR DOLLARS)                                                  
*   P3=A(3 BYTE OUTPUT AREA) (FOR GRIDS UP TO 8 BYTES)                          
*======================================================================         
PERACHMT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,8(R1)            OUTPUT                                       
         LM    RE,RF,0(R1)         GET A(PURCHASED),A(GOAL)                     
*                                                                               
         ICM   R7,15,0(RF)         GET GOALS (ALWAYS 2-DEC NOW)                 
         N     R7,=X'3FFFFFFF'     DROP FLAGS                                   
         LTR   R7,R7                                                            
         BZ    PER100                                                           
*                                                                               
         L     R5,0(RE)            GET PURCHASED                                
         N     R5,=X'3FFFFFFF'     DROP FLAGS                                   
         M     R4,=F'200'          X 100 X 2                                    
*                                                                               
         TM    0(RE),X'40'         TEST PURCH TO 2-DEC                          
         BZ    PERACH20                                                         
         TM    0(RF),X'40'         YES - TEST GOAL TO 2-DEC                     
         BO    PERACH50                                                         
         MHI   R7,10               THEN SCALE UP GOAL                           
         B     PERACH50                                                         
*                                                                               
PERACH20 TM    0(RF),X'40'         PURCH IS 1-DEC, TEST GOAL                    
         BZ    PERACH50                                                         
         MHI   R5,10               THEN SCALE UP PURCH                          
*                                                                               
PERACH50 DR    R4,R7                                                            
         AHI   R5,1                                                             
         SRL   R5,1                                                             
*                                                                               
         LTR   R5,R5                                                            
         BNZ   PER102                                                           
*                                                                               
PER100   TM    PCDRIVEN,PCGRIDQ                                                 
         JO    XIT                                                              
         MVI   3(R8),0                                                          
         J     XIT                                                              
*                                                                               
PER102   TM    PCDRIVEN,PCGRIDQ                                                 
         BO    PER104                                                           
         EDIT  (R5),(3,(R8))                                                    
         J     XIT                                                              
PER104   EDIT  (R5),(8,(R8)),ALIGN=LEFT                                         
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
* CPP CALCULATION AND PRINT                                                     
*  P1=A(POINTS OR IMPS -NO DECIMALS)                                            
*  P2=A(DOLLARS -NO DECIMALS)                                                   
*  P3=A(7 BYTE OUTPUT AREA  =   XXXX.XX)                                        
*======================================================================         
                                                                                
CPP      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R7,0(R1)          A(PNTS)                                        
         L     R3,4(R1)          A(DOLLARS)                                     
         L     R8,8(R1)          A(OUTPUT)                                      
*                                                                               
         MVC   FULL,0(R7)          MOVE POINTS                                  
         ICM   R7,15,FULL          PNTS                                         
         N     R7,=X'3FFFFFFF'     DROP FLAGS                                   
         LTR   R7,R7                                                            
         BZ    CPP100                                                           
*                                                                               
         L     R5,0(R3)          DOLLARS                                        
         LHI   R4,2000           SCALE UP TO TENTHS X 2                         
         TM    FULL,X'40'        TEST POINTS 2-DEC                              
         BZ    *+8                                                              
         LHI   R4,20000                                                         
* CHECK VALUES BEFORE DIVIDE                                                    
         LR    RE,R5                                                            
         CVD   RE,DUB                                                           
         ZAP   WORK(16),DUB                                                     
         LR    RF,R4                                                            
         CVD   RF,DUB                                                           
         MP    WORK(16),DUB                                                     
         CVD   R7,DUB                                                           
         DP    WORK(16),DUB                                                     
         CP    WORK(8),=P'2147483647'                                           
         BNH   CPP50                                                            
         XR    R5,R5                                                            
         B     CPP100                                                           
*                                                                               
CPP50    MR    R4,R4                                                            
         DR    R4,R7                                                            
         AHI   R5,1                                                             
         SRL   R5,1                                                             
*                                                                               
         LTR   R5,R5                                                            
         BNZ   CPP110                                                           
*                                                                               
CPP100   TM    PCDRIVEN,PCGRIDQ                                                 
         JO    XIT                                                              
         MVI   7(R8),X'0'                                                       
         J     XIT                                                              
*                                                                               
CPP110   TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    CPP120                                                           
         CLI   BYTE2,X'FF'                                                      
         BE    CPP115                                                           
         EDIT  (R5),(10,(R8)),2,ALIGN=LEFT                                      
         J     XIT                                                              
*                                                                               
CPP115   EDIT  (R5),(10,(R8)),2,ALIGN=LEFT,TRAIL=C'+'                           
         J     XIT                                                              
*                                                                               
CPP120   EDIT  (R5),(7,(R8)),2     CPP                                          
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*        FORMAT ROUTINE FOR GRIDS DISPLAY                                       
*              P1 = GRID DATA LINE TYPE                                         
*              P2 = A(MIS BUCKETS)                                              
*              P3 = A(MIS ACCUMULATORS)                                         
*======================================================================         
                                                                                
         USING GCTBLD,R3                                                        
         USING DLCBD,R4                                                         
         USING GFPARD,PARMS                                                     
FORMGRID NTR1  BASE=*,LABEL=*                                                   
         NI    PCDRIVE2,X'FF'-PC2SCRFL  RESET FLAG                              
*                                                                               
         MVC   PARMS(6*L'PARMS),DMCB                                            
         LA    R3,GCTBL            GRID COLUMN TABLE                            
         L     R4,ADLCB            DOWNLOAD CONTROL BLOCK                       
*                                                                               
         TM    PCDRIVEN,PCGINIQ    NEED TO INITIALIZE?                          
         BZ    FGR3                . NO                                         
         GOTOR DWNL,DWNINIT        INITIALIZE THE DOWNLOAD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    PCDRIVEN,X'FF'-PCGINIQ                                           
*                                                                               
FGR3     TM    PCDRIVEN,PCGDEFQ    GRID COLUMN DEFINITIONS NEEDED?              
         BZ    *+8                 . NO                                         
         BRAS  RE,GRHEAD           . YES, WELL, DISPLAY THEM                    
*----------------                                                               
* END OF RECORDS                                                                
*----------------                                                               
FGR4     CLI   GFLSTY,GFEOR        END OF RECORDS?                              
         BE    *+12                . YES                                        
         TM    PCDRIVEN,PCGFINQ    FINISHED PROCESSING GRIDS DATA?              
         BZ    FGR6                . NO                                         
         CLI   MISL8,C':'          ALL READY OUTPUT THE END?                    
         JE    EQXIT               . YES, JUST EXIT                             
         OI    PCDRIVEN,PCGFINQ                                                 
         GOTOR DWNL,DWNEOR                                                      
         J     EQXIT                                                            
*                                                                               
FGR6     CLC   SVGLCTR,GLCTR       SHOULD WE DISPLAY THIS LINE?                 
         BH    FGRX                . NO                                         
         OI    PCDRIVEN,PCFCFRQ    FIRST COLUMN FOR ROW                         
*----------------------------------                                             
* PROCESS GRID COLUMN TABLE                                                     
*----------------------------------                                             
FGR10    CLI   GCCOID,X'FF'        END OF TABLE?                                
         BE    FGR200              . YES                                        
         BRAS  RE,GRFCHK           CHECK COLUMN AND LINE FORMAT                 
         BNE   FGR100              COLUMN NOT WANTED, SKIP                      
*------------------                                                             
* SET DATA ADDRESS                                                              
*------------------                                                             
FGR15    LA    R2,DLCBFLD          R2=A(OUTPUT)                                 
         TM    PCDRIVEN,PCFCFRQ    FIRST COLUMN FOR ROW?                        
         BZ    *+8                 . NO                                         
         LA    R2,1(R2)            . YES, LEAVE A SPACE                         
*                                                                               
         LA    R5,GENOLD           ASSUME IN STORAGE                            
         TM    GCIND,GCIBUC        IN BUCKETS?                                  
         BZ    *+8                 . NO                                         
         L     R5,GFABUC           . YES                                        
         TM    GCIND,GCIACC        IN ACCUMULATORS?                             
         BZ    *+8                 . NO                                         
         L     R5,GFAACC           . YES                                        
         AH    R5,GCDDIS           DISPLACEMENT TO DATA                         
*-------------------                                                            
* PROCESS DATA TYPE                                                             
*-------------------                                                            
         SR    RF,RF                                                            
         IC    RF,GCDTYP           GRID COLUMN DATA TYPE                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     FGRTXT              1 - GCDTXT  - TEXT                           
         B     FGRNUM              2 - GCDNUM  - NUMBER                         
         B     FGRLEN              3 - GCDLEN  - LENGTH                         
         B     FGRPNTS             4 - GCDPNTS - POINTS                         
         B     FGRWEEK             5 - GCDWEEK - WEEK                           
         B     FGRCPP              6 - GCDCPP  - CPP                            
         B     FGRAVGP             7 - GCDAVGP - AVERAGE POINTS                 
         B     FGRPCTA             8 - GCDPCTA - PCT ACHMT                      
         B     FGRYN               9 - GCDYN   - BOOLEAN                        
*---------                                                                      
* TEXT                                                                          
*---------                                                                      
FGRTXT   SR    R6,R6                                                            
         ICM   R6,3,GCXTRA         LENGTH OF TEXT FIELD                         
         BNP   FGR90               NOT POSTITIVE, THEN OUTPUT BLANK             
         CHI   R6,L'DLCBFLD-1      BIGGER THAN THE OUTPUT FIELD?                
         BNH   *+8                 . NO, WE'RE GOOD                             
         LHI   R6,L'DLCBFLD-1      . YES, OUTPUT WHAT WE CAN                    
         AR    R6,R5                                                            
FGRT2    BCTR  R6,0                                                             
         CLI   0(R6),C' '          BACK UP PAST SPACES                          
         BE    *+12                                                             
         CLI   0(R6),0             AND BINARY ZERO                              
         BNE   FGRT4                                                            
         CR    R6,R5               ALL BLANK?                                   
         BNH   FGR90               . YES                                        
         B     FGRT2               . NO                                         
*                                                                               
FGRT4    SR    R6,R5               EXACT LENGTH OF TEXT                         
         EX    R6,FGRTMOVE                                                      
         B     FGR90                                                            
FGRTMOVE MVC   0(0,R2),0(R5)    MOVE INTO OUTPUT FIELD                          
*-------------------------------                                                
* NUMBER (DOLLARS, SPOTS, ETC.)                                                 
*-------------------------------                                                
FGRNUM   ICM   R6,15,0(R5)                                                      
         BZ    FGR90               PRINT BLANK IF ZERO                          
         B     FGR70               NO DECIMALS                                  
*---------                                                                      
* LENGTH                                                                        
*---------                                                                      
FGRLEN   CLI   GFLSTY,GCLSUB       SUB-TOTAL LINE?                              
         BE    FGR90               . YES, NOTHING                               
         CLI   GFLSTY,GCLTOT       TOTAL LINE?                                  
         BE    FGR90               . YES, NOTHING                               
         SR    R6,R6                                                            
         ICM   R6,1,0(R5)            PICK UP LENGTH AND OUTPUT                  
         BZ    FGR90               ITS ZERO, DISPLAY NOTHING                    
         B     FGR70                                                            
*---------                                                                      
* POINTS                                                                        
*---------                                                                      
FGRPNTS  ICM   R7,15,0(R5)                                                      
         TM    0(R5),X'40'         VALUE IS 2-DECIMAL?                          
         BZ    FGP020              NO, THIS VALUE IS 1-DECIMAL                  
         SR    R6,R6               YES, WE HAVE A 2-DECIMAL VALUE               
         N     R7,=X'3FFFFFFF'     DROP FLAG                                    
         TM    SVOPT2,SVOPT2_2DEC  USER WANTS 2 DECIMALS SHOWN?                 
         JZ    FGP030              NO                                           
*********                                                                       
* IGNORING WHAT IS IN THE PROFILE                                               
*&&DO                                                                           
         CLI   SVDEMNMS,C'R'       RATINGS?                                     
         JE    *+12                                                             
         CLI   SVDEMNMS,C'E'       EXTENDED RATINGS?                            
         JNE   FGP010              NEITHER, THEN IT IS IMPRESSIONS              
         CLI   SVSPPRF9,C'Y'       2 DECIMAL RATINGS?                           
         JNE   FGP030              NO, ONLY 1 DEC RTGS                          
         J     FGP015                                                           
                                                                                
FGP010   CLI   SV00APRF6,C'Y'      WE HAVE 2 DECIMAL IMPRESSIONS?               
         JNE   FGP030              NO, ONLY 1 DEC IMPS                          
*&&                                                                             
FGP015   LR    R6,R7                                                            
         J     FGR72               SHOW 2 DECIMALS                              
**                                                                              
* ONE DECIMAL VALUE                                                             
**                                                                              
FGP020   TM    SVOPT2,SVOPT2_2DEC  USER WANTS 2 DECIMALS SHOWN?                 
         JZ    FGP050              NO                                           
         MHI   R7,10               YES, ADJUST VALUE                            
         J     FGP015              AND SHOW AS 2 DECIMAL                        
*                                                                               
FGP030   M     R6,=F'2'            CHANGES 2 DECIMAL VALUE TO 1-DEC             
         D     R6,=F'10'                                                        
         AHI   R7,1                                                             
         SRL   R7,1                                                             
FGP050   LR    R6,R7                                                            
         B     FGR71               ONE DECIMAL                                  
*---------                                                                      
* WEEK                                                                          
*---------                                                                      
FGRWEEK  CLI   GFLSTY,GCLTOT       TOTAL LINE?                                  
         BNE   *+14                . NO                                         
         MVC   0(6,R2),=C'TOTAL*'                                               
         B     FGR90                                                            
         GOTO1 VDATCON,DMCB,(2,GFAMON),(4,(R2)),0 MMMDD FORMAT                  
         B     FGR90                                                            
*---------                                                                      
* CPP                                                                           
*---------                                                                      
FGRCPP   L     R6,GFAACC           R6=A(POINTS),R5=A(DOLLARS)                   
         AH    R6,GCXTRA           POINT TO POINTS                              
         CLI   BYTE2,X'FF'         SET FOR EQUIV?                               
         BNE   *+8                 . NO                                         
         AHI   R6,ACC10-ACC4       USE EQUIVALENCED                             
         GOTOR CPP,DMCB,(R6),(R5),(R2)     GOAL CPP CALC AND DISPLAY            
         CLI   0(R2),C' '                                                       
         BE    FGR90                                                            
         B     FGR92                                                            
*----------------                                                               
* AVERAGE POINTS                                                                
*----------------                                                               
FGRAVGP  L     R6,GFAACC           A(ACCUMULATORS)                              
         L     R6,ACC1-ACCUMD(R6)  SPOTS                                        
         LTR   R6,R6               NONE?                                        
         BZ    FGR90               . YES, THEN BLANK                            
         L     RE,0(R5)            PURCH POINTS                                 
         N     RE,=X'3FFFFFFF'                                                  
         AR    RE,RE               X 2                                          
         SRDA  RE,32                                                            
         TM    0(R5),X'40'                                                      
         BZ    *+8                                                              
         MHI   R6,10                                                            
         DR    RE,R6                                                            
         LTR   RF,RF                                                            
         BNP   *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         LR    R6,RF                                                            
         B     FGR71                                                            
*----------------                                                               
* PCT ACH                                                                       
*----------------                                                               
FGRPCTA  LR    R6,R5               R5=A(GOAL POINTS OR DOLLARS)                 
         AHI   R6,L'ACC2           R6=A(PURCHASE POINTS OR DOLLARS)             
         GOTOR PERACHMT,DMCB,(R6),(R5),(R2)                                     
         CLI   0(R2),C' '                                                       
         BE    FGR90                                                            
         B     FGR92                                                            
*----------------                                                               
* FORMAT YES OR NO                                                              
*----------------                                                               
FGRYN    CLI   0(R5),0                                                          
         BE    FGR90               NOTHING TO DISPLAY                           
         SR    R6,R6                                                            
         CLI   0(R5),C'G'                                                       
         BNE   *+8                                                              
         LA    R6,5                                                             
         CLI   0(R5),C'B'                                                       
         BNE   *+8                                                              
         LA    R6,4                                                             
         LA    R5,GCNTAB           GRID NAME TABLE                              
         AH    R5,GCHDIS           DISPLACEMENT TO COLUMN HEADING               
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R5)    MOVE INTO OUTPUT FIELD                          
         B     FGR90                                                            
*----------------                                                               
* FORMAT NUMBERS                                                                
*----------------                                                               
                                                                                
FGR70    EDIT  (R6),(14,0(R2)),COMMAS=YES,FLOAT=-,ALIGN=LEFT                    
         B     FGR92                                                            
*                                                                               
FGR71    EDIT  (R6),(10,0(R2)),1,COMMAS=YES,FLOAT=-,ALIGN=LEFT                  
         B     FGR92                                                            
*                                                                               
FGR72    EDIT  (R6),(10,0(R2)),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT                  
         B     FGR92                                                            
*--------------                                                                 
* DISPLAY DATA                                                                  
*--------------                                                                 
                                                                                
FGR90    LHI   R1,DWNTEXT                TEXT                                   
         B     *+8                                                              
FGR92    LHI   R1,DWNNUM                 NUMBER                                 
*                                                                               
         TM    PCDRIVEN,PCFCFRQ          FIRST COLUMN FOR ROW?                  
         BZ    FGR94                                                            
         CLI   GFLSTY,GCLSUB             SUB-TOTAL LINE?                        
         BNE   *+8                       . NO                                   
         MVI   DLCBFLD,C'S'              . YES                                  
         CLI   GFLSTY,GCLTOT             TOTAL LINE?                            
         BNE   *+8                       . NO                                   
         MVI   DLCBFLD,C'T'              . YES                                  
*                                                                               
FGR94    GOTOR DWNL,(R1)                                                        
         BNE   FGRHX                                                            
         NI    PCDRIVEN,X'FF'-PCFCFRQ    FIRST COLUMN FOR ROW                   
*                                                                               
FGR100   AHI   R3,GCTLNQ                 NEXT COLUMN                            
         B     FGR10                                                            
*----------------------------------------                                       
* END OF LINE AND EXIT                                                          
*----------------------------------------                                       
FGR200   GOTOR DWNL,DWNEOW                                                      
         BNE   FGRHX                                                            
FGRX     LH    RF,GLCTR                  GRID LINE CTR                          
         LA    RF,1(RF)                                                         
         STH   RF,GLCTR                                                         
         J     EQXIT                                                            
*                                                                               
FGRHX    J     NEQXIT                                                           
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
* GRID COLUMN TABLE                                                             
*----------------------------------------------------------------------         
GCTBL    DS    0F                                                               
*        -------------------------------------------                            
         DC    CL2'A1'                                                          
         DC    AL1(GCDTXT),AL1(0)                       DAYPART                 
         DC    AL1(GCLGVP+GCLDEM),AL1(GCLALL)                                   
         DC    AL2(GCNDAYP-GCNTAB),AL2(WORK2-GENOLD),AL2(L'WORK2)               
*                                                                               
         DC    CL2'A1'                                                          
         DC    AL1(GCDWEEK),AL1(GCIDTE+GCIRGT)          WEEK                    
         DC    AL1(GCLWEEK),AL1(GCLALL)                                         
         DC    AL2(GCNWEEK-GCNTAB),AL2(0),AL2(0)                                
*                                                                               
         DC    CL2'A1'                                                          
         DC    AL1(GCDTXT),AL1(0)                       STATION                 
         DC    AL1(GCLSLIS),AL1(GCLALL)                                         
         DC    AL2(GCNSTA-GCNTAB),AL2(WORK22-GENOLD),AL2(L'WORK22)              
*                                                                               
         DC    CL2'A1'                                                          
         DC    AL1(GCDTXT),AL1(0)                       BRAND                   
         DC    AL1(GCLPALL),AL1(GCLALL)                                         
         DC    AL2(GCNBRA-GCNTAB),AL2(WORK22-GENOLD),AL2(L'WORK22)              
*        -------------------------------------------                            
         DC    CL2'A2'                                                          
         DC    AL1(GCDLEN),AL1(GCIBUC+GCINUM+GCIRGT)    LENGTH                  
         DC    AL1(GCLGVP+GCLDEM),AL1(GCLALL)                                   
         DC    AL2(GCNLEN-GCNTAB),AL2(BUCLEN-MISBUCKS),AL2(0)                   
*       ---------------------------------------------------------------         
         DC    CL2'G1'                                                          
         DC    AL1(GCDPNTS),AL1(GCIACC+GCINUM+GCIRGT)   GOAL POINTS             
         DC    AL1(GCLGVPA),AL1(GCLALL)                                         
         DC    AL2(GCNVGPNT-GCNTAB),AL2(ACC4-ACCUMD),AL2(0)                     
*                                                                               
         DC    CL2'G2'                                                          
         DC    AL1(GCDNUM),AL1(GCIACC+GCINUM+GCIRGT)    GOAL DOLLARS            
         DC    AL1(GCLGVPA),AL1(GCLALL)                                         
         DC    AL2(GCNVDOL-GCNTAB),AL2(ACC2-ACCUMD),AL2(0)                      
*                                                                               
         DC    CL2'G3'                                                          
         DC    AL1(GCDCPP),AL1(GCIACC+GCIMUM+GCIRGT)    GOAL CPP                
         DC    AL1(GCLGVPA),AL1(GCLALL)                                         
         DC    AL2(GCNVGCPP-GCNTAB),AL2(ACC2-ACCUMD),AL2(ACC4-ACCUMD)           
*                                                                               
         DC    CL2'P1'                                                          
         DC    AL1(GCDPNTS),AL1(GCIACC+GCINUM+GCIRGT)   PURCHASE POINTS         
         DC    AL1(GCLGVPA),AL1(GCLALL)                                         
         DC    AL2(GCNVPPNT-GCNTAB),AL2(ACC5-ACCUMD),AL2(0)                     
*                                                                               
         DC    CL2'P2'                                                          
         DC    AL1(GCDNUM),AL1(GCIACC+GCINUM+GCIRGT)    PURCH DOLLARS           
         DC    AL1(GCLGVPA),AL1(GCLALL)                                         
         DC    AL2(GCNPDOL-GCNTAB),AL2(ACC3-ACCUMD),AL2(0)                      
*                                                                               
         DC    CL2'P3'                                                          
         DC    AL1(GCDCPP),AL1(GCIACC+GCIMUM+GCIRGT)    PURCHASED CPP           
         DC    AL1(GCLGVPA),AL1(GCLALL)                                         
         DC    AL2(GCNVPCPP-GCNTAB),AL2(ACC3-ACCUMD),AL2(ACC5-ACCUMD)           
*                                                                               
         DC    CL2'L1'                                                          
         DC    AL1(GCDNUM),AL1(GCIACC+GCINUM+GCIRGT)    LOCKIN SPOTS            
         DC    AL1(GCLGVPL),AL1(GCLALL)                                         
         DC    AL2(GCNLSPOT-GCNTAB),AL2(ACC9-ACCUMD),AL2(0)                     
*                                                                               
         DC    CL2'A3'                                                          
         DC    AL1(GCDNUM),AL1(GCIACC+GCINUM+GCIRGT)    SPOTS                   
         DC    AL1(GCLGVPA),AL1(GCLALL)                                         
         DC    AL2(GCNSPOT-GCNTAB),AL2(ACC1-ACCUMD),AL2(0)                      
*                                                                               
         DC    CL2'A4'                                                          
         DC    AL1(GCDAVGP),AL1(GCIACC+GCINUM+GCIRGT)   AVG POINTS              
         DC    AL1(GCLGVPA),AL1(GCLALL)                                         
         DC    AL2(GCNVAVG-GCNTAB),AL2(ACC5-ACCUMD),AL2(0)                      
*                                                                               
         DC    CL2'A5'                                                          
         DC    AL1(GCDPCTA),AL1(GCIACC+GCINUM+GCIRGT)   PCT-ACH-PNTS            
         DC    AL1(GCLGVPA),AL1(GCLALL)                                         
         DC    AL2(GCNPCAPN-GCNTAB),AL2(ACC4-ACCUMD),AL2(0)                     
*                                                                               
         DC    CL2'A6'                                                          
         DC    AL1(GCDPCTA),AL1(GCIACC+GCINUM+GCIRGT)   PCT-ACH-DOLS            
         DC    AL1(GCLGVPA),AL1(GCLALL)                                         
         DC    AL2(GCNPCADO-GCNTAB),AL2(ACC2-ACCUMD),AL2(0)                     
*       -------------------------------------------------------------           
         DC    CL2'D1'                                                          
         DC    AL1(GCDNUM),AL1(GCIACC+GCINUM+GCIRGT)    DOLLARS                 
         DC    AL1(GCLDEMA),AL1(GCLALL)                                         
         DC    AL2(GCNDDOL-GCNTAB),AL2(ACC3-ACCUMD),AL2(0)                      
*                                                                               
         DC    CL2'D2'                                                          
         DC    AL1(GCDPNTS),AL1(GCIACC+GCINUM+GCIRGT)   DEMO 1 POINTS           
         DC    AL1(GCLDEMA),AL1(GCLALL)                                         
         DC    AL2(GCNDEM1-GCNTAB),AL2(ACC5-ACCUMD),AL2(0)                      
*                                                                               
         DC    CL2'D3'                                                          
         DC    AL1(GCDCPP),AL1(GCIACC+GCIMUM+GCIRGT)    DEMO 1 CPP              
         DC    AL1(GCLDEMA),AL1(GCLALL)                                         
         DC    AL2(GCNCP1-GCNTAB),AL2(ACC3-ACCUMD),AL2(ACC5-ACCUMD)             
*                                                                               
         DC    CL2'D4'                                                          
         DC    AL1(GCDPNTS),AL1(GCIACC+GCINUM+GCIRGT)   DEMO 2 POINTS           
         DC    AL1(GCLDEMA),AL1(GCLALL)                                         
         DC    AL2(GCNDEM2-GCNTAB),AL2(ACC6-ACCUMD),AL2(0)                      
*                                                                               
         DC    CL2'D5'                                                          
         DC    AL1(GCDCPP),AL1(GCIACC+GCIMUM+GCIRGT)    DEMO 2 CPM              
         DC    AL1(GCLDEMA),AL1(GCLALL)                                         
         DC    AL2(GCNCP2-GCNTAB),AL2(ACC3-ACCUMD),AL2(ACC6-ACCUMD)             
*                                                                               
         DC    CL2'D6'                                                          
         DC    AL1(GCDPNTS),AL1(GCIACC+GCINUM+GCIRGT)   DEMO 3 POINTS           
         DC    AL1(GCLDEMA),AL1(GCLALL)                                         
         DC    AL2(GCNDEM3-GCNTAB),AL2(ACC7-ACCUMD),AL2(0)                      
*                                                                               
         DC    CL2'D7'                                                          
         DC    AL1(GCDCPP),AL1(GCIACC+GCIMUM+GCIRGT)    DEMO 3 CPP              
         DC    AL1(GCLDEMA),AL1(GCLALL)                                         
         DC    AL2(GCNCP3-GCNTAB),AL2(ACC3-ACCUMD),AL2(ACC7-ACCUMD)             
*                                                                               
         DC    CL2'D8'                                                          
         DC    AL1(GCDPNTS),AL1(GCIACC+GCINUM+GCIRGT)   DEMO 4 POINTS           
         DC    AL1(GCLDEMA),AL1(GCLALL)                                         
         DC    AL2(GCNDEM4-GCNTAB),AL2(ACC8-ACCUMD),AL2(0)                      
*                                                                               
         DC    CL2'D9'                                                          
         DC    AL1(GCDCPP),AL1(GCIACC+GCIMUM+GCIRGT)    DEMO 4 CPM              
         DC    AL1(GCLDEMA),AL1(GCLALL)                                         
         DC    AL2(GCNCP4-GCNTAB),AL2(ACC3-ACCUMD),AL2(ACC8-ACCUMD)             
*       ---------------------------------------------------------------         
GMCTBL   DS    0C                   GRID MARKET LIST TABLE                      
         DC    CL2'M1'                                                          
         DC    AL1(GCDTXT),AL1(0)                       MARKET                  
         DC    AL1(GCLMKTL),AL1(GCLALL)                                         
         DC    AL2(GCNMKT-GCNTAB),AL2(DSPWORK3+MLMKT-MKTLSTD)                   
         DC    AL2(L'MLMKT)                                                     
*                                                                               
         DC    CL2'M2'                                                          
         DC    AL1(GCDTXT),AL1(0)                       MARKET NAME             
         DC    AL1(GCLMKTL),AL1(GCLALL)                                         
         DC    AL2(GCNMKTNM-GCNTAB),AL2(DSPWORK3+MLMKTNM-MKTLSTD)               
         DC    AL2(L'MLMKTNM)                                                   
*                                                                               
         DC    CL2'M3'                                                          
         DC    AL1(GCDTXT),AL1(0)                       PURPOSE CODE            
         DC    AL1(GCLPURL),AL1(GCLALL)                                         
         DC    AL2(GCNPUR-GCNTAB),AL2(DSPWORK3+MLPUR-MKTLSTD)                   
         DC    AL2(L'MLPUR)                                                     
*                                                                               
         DC    CL2'M4'                                                          
         DC    AL1(GCDYN),AL1(0)                        GOALS                   
         DC    AL1(GCLPMLST),AL1(GCLALL)                                        
         DC    AL2(GCNGOAL-GCNTAB),AL2(DSPWORK3+MLGOAL-MKTLSTD)                 
         DC    AL2(L'MLGOAL)                                                    
*                                                                               
         DC    CL2'M5'                                                          
         DC    AL1(GCDYN),AL1(0)                        BUYS                    
         DC    AL1(GCLPMLST),AL1(GCLALL)                                        
         DC    AL2(GCNBUYS-GCNTAB),AL2(DSPWORK3+MLBUY-MKTLSTD)                  
         DC    AL2(L'MLBUY)                                                     
*       ---------------------------------------------------------------         
         DC    X'FF'                END OF TABLE                                
*                                                                               
         DROP  R3,R4                                                            
DSPWORK3 EQU   WORK3-GENOLD        DISPLACEMENT TO WORK3                        
         EJECT                                                                  
*======================================================================         
* FORMAT ROUTINE FOR GRID HEADINGS                                              
*======================================================================         
         USING GCTBLD,R3                                                        
         USING DLCBD,R4                                                         
GRHEAD   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ADLCB            DOWNLOAD CONTROL BLOCK                       
*                                                                               
         MVC   DLCBFLX(4),=C'***S'                                              
         TM    SVOPT2,SVOPT2_MLK+SVOPT2_SLK  ANY LOCKIN REQUESTED ?             
         BZ    GRH05A1                                                          
         MVC   DLCBFLX+4(19),=C'LOCKIN VS PURCHASED'                            
         B     GRH05A2                                                          
*                                                                               
GRH05A1  LA    R1,=C'GOAL VS. PURCHASED'                                        
         CLI   FORMIND,C'G'        GVP DISPLAY                                  
         BE    GRH05A2                                                          
         LA    R1,=C'LKIN VS. PURCHASED'                                        
         CLI   FORMIND,C'L'        LVP DISPLAY                                  
         BNE   GRH05A3                                                          
GRH05A2  MVC   DLCBFLX+4(18),0(R1)                                              
         CLI   WEEKIND,C'W'                                                     
         BNE   GRH05A3                                                          
         MVC   DLCBFLX+22(7),=C'-WEEKLY'                                        
GRH05A3  MVC   DLCBFLX+40(5),=C'DEMO='                                          
         MVC   DLCBFLX+45(7),SVDEMNMS                                           
         B     GRH05A10                                                         
*                                                                               
GRH05A5  CLI   FORMIND,C'D'        DEMO                                         
         BNE   GRH05A7                                                          
         MVC   DLCBFLX+4(22),=C'PURCHASED DEMOGRAPHICS'                         
         CLI   WEEKIND,C'W'                                                     
         BNE   GRH05A10                                                         
         MVC   DLCBFLX+26(7),=C'-WEEKLY'                                        
         B     GRH05A10                                                         
*                                                                               
GRH05A7  CLI   FORMIND,C'M'        MARKET LIST?                                 
         BNE   GRH05A8                                                          
         MVC   DLCBFLX+4(18),=C'ACTIVE MARKET LIST'                             
         B     GRH05A10                                                         
*                                                                               
GRH05A8  CLI   FORMIND,C'P'        PURPOSE LIST?                                
         BNE   GRH05A9                                                          
         MVC   DLCBFLX+4(24),=C'ACTIVE PURPOSE CODE LIST'                       
         B     GRH05A10                                                         
*                                                                               
GRH05A9  MVC   DLCBFLX+4(24),=C'MEDIA INFORMATION SYSTEM'                       
*                                                                               
GRH05A10 GOTOR DWNL,DWNEXTXT                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR DWNL,DWNEOL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*---------------------------                                                    
* PROCESS HEADER INFORMATION                                                    
*---------------------------                                                    
*                                                                               
         LA    R2,DLCBFLX                                                       
         MVC   DLCBFLX(6),=C'MEDIA:'                                            
         MVC   DLCBFLX+7(L'MISMED),MISMED                                       
         LHI   R5,7+L'MISMED                                                    
         BRAS  RE,PUTCOMMA                                                      
         MVC   0(7,R2),=C'CLIENT:'                                              
         MVC   8(L'MISCLT,R2),MISCLT                                            
         LHI   R5,8+L'MISCLT                                                    
         BRAS  RE,PUTCOMMA                                                      
         MVC   0(4,R2),=C'PRD:'                                                 
         MVC   5(L'MISPRD,R2),MISPRD                                            
         LHI   R5,5+L'MISPRD                                                    
         BRAS  RE,PUTCOMMA                                                      
         MVC   0(4,R2),=C'EST:'                                                 
         MVC   5(L'MISEST,R2),MISEST                                            
*                                                                               
         GOTOR DWNL,DWNEXTXT                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR DWNL,DWNEOL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,DLCBFLX                                                       
         MVC   0(4,R2),=C'MKT:'                                                 
         MVC   5(L'MISMKT,R2),MISMKT                                            
         LHI   R5,5+L'MISMKT                                                    
         BRAS  RE,PUTCOMMA                                                      
         MVC   0(4,R2),=C'STA:'                                                 
         MVC   5(4,R2),=C'ALL'                                                  
         OC    MISSTA,MISSTA                                                    
         BZ    *+10                                                             
         MVC   5(L'MISSTA,R2),MISSTA                                            
         LHI   R5,5+L'MISSTA                                                    
         BRAS  RE,PUTCOMMA                                                      
         MVC   0(4,R2),=C'DPT:'                                                 
         MVC   5(3,R2),=C'ALL'                                                  
         OC    MISDPT,MISDPT                                                    
         BZ    *+10                                                             
         MVC   5(L'MISDPT,R2),MISDPT                                            
         LHI   R5,5+L'MISDPT                                                    
         BRAS  RE,PUTCOMMA                                                      
         MVC   0(4,R2),=C'LEN:'                                                 
         MVC   5(3,R2),=C'ALL'                                                  
         OC    MISLEN,MISLEN                                                    
         BZ    *+10                                                             
         MVC   5(L'MISLEN,R2),MISLEN                                            
*                                                                               
         GOTOR DWNL,DWNEXTXT                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR DWNL,DWNEOL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,DLCBFLX                                                       
         MVC   0(7,R2),=C'PERIOD:'                                              
         MVC   8(2,R2),=C'ES'                                                   
         OC    MISPER,MISPER                                                    
         BZ    *+10                                                             
         MVC   8(L'MISPER,R2),MISPER                                            
         LHI   R5,8+L'MISPER                                                    
         BRAS  RE,PUTCOMMA                                                      
         MVC   0(7,R2),=C'FORMAT:'                                              
         MVC   8(4,R2),=C'NONE'                                                 
         OC    MISFOR,MISFOR                                                    
         BZ    *+10                                                             
         MVC   8(L'MISFOR,R2),MISFOR                                            
*                                                                               
         GOTOR DWNL,DWNEXTXT                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR DWNL,DWNEOL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DLCBFLX(8),=C'OPTIONS:'                                          
         MVC   DLCBFLX+9(4),=C'NONE'                                            
         CLC   MISMKT,=C'LIST'                                                  
         BE    GRH07                                                            
         OC    MISOPT,MISOPT                                                    
         BZ    GRH07                                                            
         MVC   DLCBFLX+9(L'MISOPT),MISOPT                                       
*                                                                               
GRH07    GOTOR DWNL,DWNEXTXT                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR DWNL,DWNEOL                                                      
         BE    GRH10                                                            
         DC    H'0'                                                             
*                                                                               
PUTCOMMA LA    R2,0(R2,R5)         POINT TO END OF INPUT                        
PUTCOM10 CLI   0(R2),C' '          HIGHER THEN SPACE?                           
         BH    PUTCOM20            YES                                          
         BCT   R2,PUTCOM10         NO, POINT TO PREV BYTE                       
PUTCOM20 MVI   1(R2),C','          MOVE IN A COMMA IN NEXT SPACE                
         LA    R2,4(R2)            LEAVE 2 SPACES BETWEEN FIELDS                
         BR    RE                                                               
*---------------------------                                                    
* PROCESS GRID COLUMN TABLE                                                     
*---------------------------                                                    
GRH10    CLI   GCCOID,X'FF'                                                     
         BE    GRH50                                                            
         BRAS  RE,GRFCHK           CHECK COLUMN AND LINE FORMAT                 
         BNE   GRH44               COLUMN NOT WANTED, SKIP                      
*---------------                                                                
* BUILD HEADING                                                                 
*---------------                                                                
         LA    R2,DLCBFLD                                                       
         LA    R5,GCNTAB           GRID NAME TABLE                              
         AH    R5,GCHDIS           DISPLACEMENT TO COLUMN HEADING               
*                                                                               
GRH15    SR    R1,R1                                                            
         IC    R1,0(R5)            LENGTH OF COLUMN HEADING                     
         LA    R5,1(R5)            TEXT FOR HEADING                             
         SHI   R1,1                DECR NAME LENGTH                             
         BNM   GRH28               MINUS MEANS VARIABLE HEADING                 
*                                                                               
         SR    R1,R1               HANDLE VARIABLE HEADING                      
         IC    R1,0(R5)            LENGTH OF HEADING (BUMPED ALREADY)           
         SHI   R1,1                                                             
         BNM   GRH20               MINUS MEANS SPECIAL HEADER-NEED CODE         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,1(R5)          PICK UP DISPLACEMENT TO ROUTINE              
         AR    RF,RB               ADD IT UP                                    
         BASR  RE,RF               GO GET NEW ENTRY                             
         B     GRH15                                                            
*                                                                               
GRH20    MVC   0(6,R2),=C'DEMO |'                                               
         MVC   4(1,R2),1(R5)                                                    
         LA    R2,6(R2)                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,2(R5)          DISPLACEMENT IN STORAGE                      
         LR    R5,RA               RA=A(SAVED STORAGE)                          
         AR    R5,RF               POINT TO SPOT IN SAVED AREA                  
         CHI   R1,2                                                             
         BH    GRH28                                                            
         MVC   0(3,R2),=C'CPP'     MOVE IN CPP                                  
         CLI   0(R5),0                                                          
         BE    GRH29                                                            
         CLI   0(R5),C'R'          RATING?                                      
         BE    GRH29                                                            
         CLI   0(R5),C'E'          EXTENDED RATING ?                            
         BE    GRH29                                                            
         MVI   2(R2),C'M'          CPM                                          
         B     GRH29                                                            
*                                                                               
GRH28    EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R5)       HEADING GOES TO OUTPUT LINE                  
GRH29    AR    R2,R1               LAST BYTE                                    
GRH30    CLI   0(R2),C' '          BUMP BACK PAST SPACES                        
         BH    GRH35                                                            
         SHI   R1,1                DECR LENGTH                                  
         BM    *+8                 . NEGATIVE,THEN NO HEADING                   
         BCT   R2,GRH30                                                         
         MVC   0(4,R2),=C'NONE'                                                 
         LA    R2,3(R2)                                                         
*---------------                                                                
* COLUMN FORMAT                                                                 
*---------------                                                                
GRH35    MVC   1(3,R2),=C'***'                                                  
         LA    R2,4(R2)                                                         
         MVC   0(L'GCCOID,R2),GCCOID                                            
         TM    GCIND,GCINUM+GCIDTE+GCIRGT+GCIMUM  COLUMN FORMAT?                
         BZ    GRH40                       . NO                                 
         MVI   2(R2),C'*'                                                       
         LA    R2,3(R2)                                                         
         MVI   0(R2),C'T'                  TEXT                                 
         MVI   1(R2),C'L'                  LEFT JUSTIFY                         
         TM    GCIND,GCIDTE                                                     
         BZ    *+8                                                              
         MVI   0(R2),C'D'                  DATE                                 
         TM    GCIND,GCINUM                                                     
         BZ    *+8                                                              
         MVI   0(R2),C'N'                  NUMERIC                              
         TM    GCIND,GCIMUM                                                     
         BZ    *+8                                                              
         MVI   0(R2),C'M'                  NUMERIC                              
         TM    GCIND,GCIRGT                                                     
         BZ    *+8                                                              
         MVI   1(R2),C'R'                  RIGHT JUSTIFY                        
*                                                                               
GRH40    GOTOR DWNL,DWNTEXT                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GRH44    AHI   R3,GCTLNQ               NEXT GRID COLUMN ENTRY                   
         B     GRH10                                                            
*                                                                               
GRH50    GOTOR DWNL,DWNEOL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    PCDRIVEN,X'FF'-PCGDEFQ  GRID COLUMN DEFINITIONS NEEDED?          
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
*---------------------------------------------                                  
* GRID COLUMN HEADING TABLE                                                     
*   . AL1(LENGTH),C'HEADING TEXT'                                               
*   . AL1(0),AL1(LENGTH),AL2(DISP IN STORAGE)                                   
*   . AL1(0),AL1(0),AL2(DISP TO ROUTINE)                                        
*---------------------------------------------                                  
GCNTAB   DS    0C                                                               
*                                                                               
GCNDAYP  DC    AL1(7),C'DAYPART'                                                
GCNWEEK  DC    AL1(4),C'WEEK'                                                   
GCNSTA   DC    AL1(7),C'STATION'                                                
GCNBRA   DC    AL1(5),C'BRAND'                                                  
GCNLEN   DC    AL1(6),C'LENGTH'                                                 
GCNDDOL  DC    AL1(7),C'DOLLARS'                                                
GCNGPNT  DC    AL1(11),C'GOAL|POINTS'                                           
GCNLPNT  DC    AL1(13),C'LOCKIN|POINTS'                                         
GCNGIMP  DC    AL1(9),C'GOAL|IMPS'                                              
GCNGDOL  DC    AL1(12),C'GOAL|DOLLARS'                                          
GCNLDOL  DC    AL1(14),C'LOCKIN|DOLLARS'                                        
GCNGCPP  DC    AL1(8),C'GOAL|CPP'                                               
GCNGCMP  DC    AL1(8),C'GOAL|CPM'                                               
GCNLCPP  DC    AL1(10),C'LOCKIN|CPP'                                            
GCNPPNT  DC    AL1(16),C'PURCHASED|POINTS'                                      
GCNPIMP  DC    AL1(14),C'PURCHASED|IMPS'                                        
GCNPDOL  DC    AL1(17),C'PURCHASED|DOLLARS'                                     
GCNPCPP  DC    AL1(13),C'PURCHASED|CPP'                                         
GCNPCMP  DC    AL1(13),C'PURCHASED|CPM'                                         
GCNSPOT  DC    AL1(5),C'SPOTS'                                                  
GCNLSPOT DC    AL1(12),C'LOCKIN|SPOTS'                                          
GCNPSPOT DC    AL1(15),C'PURCHASED|SPOTS'                                       
GCNAVGP  DC    AL1(14),C'AVERAGE|POINTS'                                        
GCNAVGI  DC    AL1(12),C'AVERAGE|IMPS'                                          
GCNPCAPN DC    AL1(16),C'% ACHVMNT|POINTS'                                      
GCNPCADO DC    AL1(17),C'% ACHVMNT|DOLLARS'                                     
GCNMKT   DC    AL1(6),C'MARKET'                                                 
GCNMKTNM DC    AL1(11),C'MARKET NAME'                                           
GCNPUR   DC    AL1(12),C'PURPOSE CODE'                                          
GCNGOAL  DC    AL1(5),C'GOALS'                                                  
GCNBUYS  DC    AL1(4),C'BUYS'                                                   
*                                                                               
GCNDEM1  DC    AL1(0,7),C'1',AL2(SVDEMNMS-T20BFFD)                              
GCNDEM2  DC    AL1(0,7),C'2',AL2(7+SVDEMNMS-T20BFFD)                            
GCNDEM3  DC    AL1(0,7),C'3',AL2(14+SVDEMNMS-T20BFFD)                           
GCNDEM4  DC    AL1(0,7),C'4',AL2(21+SVDEMNMS-T20BFFD)                           
GCNCP1   DC    AL1(0,3),C'1',AL2(SVDEMNMS-T20BFFD)                              
GCNCP2   DC    AL1(0,3),C'2',AL2(7+SVDEMNMS-T20BFFD)                            
GCNCP3   DC    AL1(0,3),C'3',AL2(14+SVDEMNMS-T20BFFD)                           
GCNCP4   DC    AL1(0,3),C'4',AL2(21+SVDEMNMS-T20BFFD)                           
GCNVGPNT DC    AL1(0),AL1(0),AL2(GRHSPEC-GRHEAD)                                
GCNVDOL  DC    AL1(0),AL1(0),AL2(GRHSPEC-GRHEAD)                                
GCNVGCPP DC    AL1(0),AL1(0),AL2(GRHSPEC-GRHEAD)                                
GCNVAVG  DC    AL1(0),AL1(0),AL2(GRHSPEC-GRHEAD)                                
GCNVPPNT DC    AL1(0),AL1(0),AL2(GRHSPEC-GRHEAD)                                
GCNVPCPP DC    AL1(0),AL1(0),AL2(GRHSPEC-GRHEAD)                                
         DC    X'FF'                                                            
*------------------------------------------------------------                   
* SPECIAL HEADING - ON EXIT R5=A(HEADING TO BE USED)                            
*------------------------------------------------------------                   
GRHSPEC  LA    R5,GCNTAB                     R5=A(GRID NAME TABLE)              
         LH    RF,GCHDIS                     RF=DISP OF ENTRY                   
*                                                                               
         TM    SVOPT2,SVOPT2_MLK+SVOPT2_SLK  ANY LOCKIN REQUESTED?              
         JZ    GRHSP20                       . NO                               
         CHI   RF,GCNVGPNT-GCNTAB            LOCKIN POINTS?                     
         JNE   *+8                                                              
         AHI   R5,GCNLPNT-GCNTAB                                                
         CHI   RF,GCNVDOL-GCNTAB             LOCKIN DOLLARS?                    
         JNE   *+8                                                              
         AHI   R5,GCNLDOL-GCNTAB                                                
         CHI   RF,GCNVGCPP-GCNTAB            LOCKIN CPP?                        
         JNE   *+8                                                              
         AHI   R5,GCNLCPP-GCNTAB                                                
         CHI   RF,GCNVPPNT-GCNTAB            PURCHASED POINTS?                  
         JNE   *+8                                                              
         AHI   R5,GCNPPNT-GCNTAB                                                
         CHI   RF,GCNVPCPP-GCNTAB            PURCHASED CPP?                     
         JNE   *+8                                                              
         AHI   R5,GCNPCPP-GCNTAB                                                
         CHI   RF,GCNVAVG-GCNTAB             AVERAGE POINTS?                    
         JNE   *+8                                                              
         AHI   R5,GCNAVGP-GCNTAB                                                
         BR    RE                            EXIT                               
*                                                                               
GRHSP20  CLI   SVDEMNMS,C'R'                 TEST RATING                        
         JE    GRHSP40                                                          
         CLI   SVDEMNMS,C'E'                 OR EXTENDED RATING                 
         JE    GRHSP40                                                          
         CHI   RF,GCNVGPNT-GCNTAB            GOAL IMPS?                         
         JNE   *+8                                                              
         AHI   R5,GCNGIMP-GCNTAB                                                
         CHI   RF,GCNVDOL-GCNTAB             GOAL DOLLARS?                      
         JNE   *+8                                                              
         AHI   R5,GCNGDOL-GCNTAB                                                
         CHI   RF,GCNVGCPP-GCNTAB            GOAL CMP?                          
         JNE   *+8                                                              
         AHI   R5,GCNGCMP-GCNTAB                                                
         CHI   RF,GCNVPPNT-GCNTAB            PURCHASED IMPS?                    
         JNE   *+8                                                              
         AHI   R5,GCNPIMP-GCNTAB                                                
         CHI   RF,GCNVPCPP-GCNTAB            PURCHASED CMP?                     
         JNE   *+8                                                              
         AHI   R5,GCNPCMP-GCNTAB                                                
         CHI   RF,GCNVAVG-GCNTAB             AVERAGE IMPS?                      
         JNE   *+8                                                              
         AHI   R5,GCNAVGI-GCNTAB                                                
         BR    RE                            EXIT                               
*                                                                               
GRHSP40  CHI   RF,GCNVGPNT-GCNTAB            GOAL POINTS?                       
         JNE   *+8                                                              
         AHI   R5,GCNGPNT-GCNTAB                                                
         CHI   RF,GCNVDOL-GCNTAB             GOAL DOLLARS?                      
         JNE   *+8                                                              
         AHI   R5,GCNGDOL-GCNTAB                                                
         CHI   RF,GCNVGCPP-GCNTAB            GOAL CPP?                          
         JNE   *+8                                                              
         AHI   R5,GCNGCPP-GCNTAB                                                
         CHI   RF,GCNVPPNT-GCNTAB            PURCHASED POINTS?                  
         JNE   *+8                                                              
         AHI   R5,GCNPPNT-GCNTAB                                                
         CHI   RF,GCNVPCPP-GCNTAB            PURCHASED CPP?                     
         JNE   *+8                                                              
         AHI   R5,GCNPCPP-GCNTAB                                                
         CHI   RF,GCNVAVG-GCNTAB             AVERAGE POINTS?                    
         JNE   *+8                                                              
         AHI   R5,GCNAVGP-GCNTAB                                                
         BR    RE                            EXIT                               
*                                                                               
         BR    RE                            EXIT                               
         DROP  R3,R4                                                            
         EJECT                                                                  
*======================================================================         
* CHECK FORMAT FOR GRID LINE AND COLUMN TYPES                                   
*   ON ENTRY R3=A(GRID COLUMN TABLE ENTRY)                                      
*======================================================================         
         USING GCTBLD,R3                                                        
GRFCHK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GCLTY,GCLMKTL       MARKET=LIST COLUMN?                          
         BZ    GRF05               . NO                                         
         CLI   FORMIND,C'M'        MARKET OPTION?                               
         BE    GRF70               . YES                                        
GRF05    CLI   FORMIND,C'M'        MARKET OPTION?                               
         BE    GRFCX               . YES                                        
*                                                                               
GRF07    TM    GCLTY,GCLPURL       PUR=LIST COLUMN?                             
         BZ    GRF09               . NO                                         
         CLI   FORMIND,C'P'        PUR=LIST OPTION?                             
         BE    GRF70               . YES                                        
         B     GRFCX               . NO                                         
GRF09    CLI   FORMIND,C'P'        PUR=LIST OPTION?                             
         BE    GRFCX               . YES                                        
         TM    GCLTY,GCLMKTL       MARKET=LIST COLUMN?                          
         BNZ   GRFCX               . YES                                        
*                                                                               
         TM    GCLTY,GCLGVP        GVP COLUMN?                                  
         BZ    GRF10               . NO                                         
         CLI   FORMIND,C'G'        GVP FORMAT?                                  
         BE    GRF30               . YES                                        
         CLI   FORMIND,C'L'        LVP FORMAT?                                  
         BE    GRF30               . YES                                        
         TM    GCLTY,GCLDEM        DEM COLUMN?                                  
         BZ    GRFCX               . NO                                         
*                                                                               
GRF10    TM    GCLTY,GCLDEM        DEM COLUMN?                                  
         BZ    GRF30               . NO                                         
         CLI   FORMIND,C'D'        DEM FORMAT?                                  
         BNE   GRFCX               . NO                                         
*                                                                               
GRF30    CLI   WEEKIND,C'W'        WEEK FORMAT?                                 
         BE    *+16                . YES                                        
         CLI   GCLTY,GCLWEEK       WEEKLY ONLY COLUMN?                          
         BE    GRFCX               . YES                                        
         B     *+12                . NO                                         
         TM    GCLTY,GCLWEEK       WEEK COLUMN?                                 
         BZ    GRFCX               . NO                                         
*                                                                               
         CLC   MISSTA(4),=C'LIST'  STATION LIST OPTION?                         
         BE    *+16                . YES                                        
         CLI   GCLTY,GCLSLIS       STATION LIST ONLY COLUMN?                    
         BE    GRFCX               . YES                                        
         B     *+16                . NO                                         
         TM    GCLTY,GCLSLIS       STATION LIST COLUMN?                         
         BZ    GRFCX               . NO                                         
         B     GRF60                                                            
*                                                                               
         CLC   MISPRD(3),=C'ALL'   PRODUCT=ALL OPTION?                          
         BE    *+16                                                             
         CLI   GCLTY,GCLPALL       PRODUCT=ALL ONLY COLUMN?                     
         BE    GRFCX               . YES                                        
         B     *+12                . NO                                         
         TM    GCLTY,GCLPALL       PRODUCT=ALL COLUMN?                          
         BZ    GRFCX               . NO                                         
*                                                                               
GRF60    CLI   FORMIND,C'G'        GVP FORMAT?                                  
         BE    GRF62               . YES, CONTINUE                              
         CLI   FORMIND,C'L'        LVP FORMAT                                   
         BE    GRF62                                                            
         TM    GCLTY,GCLLOCK       . NO, LOCKIN COLUMN?                         
         BO    GRFCX                     . YES, EXIT                            
         B     GRF70                     . NO, CONTINUE                         
GRF62    TM    GCLTY,GCLLOCK      LOCKIN COLUMN?                                
         BZ    GRF70                                                            
         TM    SVOPT2,SVOPT2_MLK+SVOPT2_SLK  ANY LOCKIN REQUESTED ?             
         BZ    GRFCX                         . NO, EXIT                         
*                                                                               
GRF70    CLI   GFLSTY,GCLDET       DETAIL LINE?                                 
         BNE   *+12                . NO                                         
         TM    GCLSTY,GCLDET       DETAIL COLUMN?                               
         BZ    GRFCX               . NO, SKIP                                   
         CLI   GFLSTY,GCLSUB       SUB-TOTAL LINE?                              
         BNE   *+12                . NO                                         
         TM    GCLSTY,GCLSUB       SUB-TOTAL COLUMN?                            
         BZ    GRFCX               . NO, SKIP                                   
         CLI   GFLSTY,GCLTOT       TOTAL LINE?                                  
         BNE   *+12                . NO                                         
         TM    GCLSTY,GCLTOT       TOTAL COLUMN?                                
         BZ    GRFCX               . NO, SKIP                                   
*                                                                               
GRFCYES  J     EQXIT                                                            
GRFCX    J     NEQXIT                                                           
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
*======================================================================         
* DOWNLOAD MODULE                                                               
*   ON ENTRY R1=DOWNLOAD MODE                                                   
*            DLCBLEN HAS DOWNLOAD FIELD LENGTH                                  
*            DLCBFLD HAS DOWNLOAD FIELD DATA                                    
*======================================================================         
         USING DLCBD,R4                                                         
DWNL     NTR1  BASE=*,LABEL=*                                                   
         L     R4,ADLCB            DOWNLOAD CONTROL BLOCK                       
*                                                                               
         CHI   R1,DWNINIT          INITIALIZE                                   
         BE    DWNL10                                                           
         CHI   R1,DWNEXTXT         DOWN-LOAD EXTENDED TEXT                      
         BE    DWNL20                                                           
         CHI   R1,DWNTEXT          DOWN-LOAD TEXT                               
         BE    DWNL25                                                           
         CHI   R1,DWNNUM           DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         CHI   R1,DWNEOW           END OF ROW                                   
         BE    DWNL35                                                           
         CHI   R1,DWNEOR           END OF OUTPUT                                
         BE    DWNL40                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CHI   R1,DWNEOL           END OF LINE                                  
         BE    DWNL50                                                           
         DC    H'0'                                                             
*                                                                               
*-----------------                                                              
* INITIALIZATION                                                                
*-----------------                                                              
DWNL10   XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RF,DWNHOOK          HOOK                                         
         ST    RF,DLCBAPR                                                       
         LA    RF,MISL8            STARTING OUTPUT LINE                         
         ST    RF,DLCBAPL                                                       
         MVC   DLCXMAXL,=Y(L'MISL9-1)                                           
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         L     RF,VCOMFACS                                                      
         L     RF,CDLFLD-COMFACSD(RF)                                           
         GOTO1 (RF),(R4)                                                        
         BNE   DWNLHX                                                           
         MVI   DLCBFLD,C' '        MUST CLEAR FIRST TIME IN                     
         MVC   DLCBFLD+1(L'DLCBFLD-1),DLCBFLD                                   
         XC    DLCBLEN,DLCBLEN                                                  
         MVI   DLCBFLX,C' '                                                     
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
         B     DWNLX               EXIT                                         
*                                                                               
*--------------------------                                                     
* DOWNLOAD A RECORD - (EXTENDED) TEXT                                           
*--------------------------                                                     
DWNL20   OI    DLCBFLG1,DLCBFXFL   USE EXTENDED FIELD                           
         XC    DLCBLEN,DLCBLEN                                                  
DWNL25   MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE IS TEXT                                 
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
*----------------------------                                                   
* DOWNLOAD A RECORD - NUMBER                                                    
*----------------------------                                                   
DWNL30   MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE IS NUMBER                               
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
*---------------                                                                
* END OF ROW                                                                    
*---------------                                                                
DWNL35   MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     NUMBER (SO NO DELIMITERS)                    
         MVI   DLCBFLD,C';'                                                     
         B     DWNL50                                                           
*---------------                                                                
* END OF OUTPUT                                                                 
*---------------                                                                
DWNL40   MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     NUMBER (SO NO DELIMITERS)                    
         MVI   DLCBFLD,C':'                                                     
*                                                                               
*----------------------                                                         
* OUTPUT DOWNLOAD DATA                                                          
*----------------------                                                         
DWNL50   L     RF,VCOMFACS                                                      
         L     RF,CDLFLD-COMFACSD(RF)                                           
         GOTO1 (RF),(R4)                                                        
*                                                                               
         NI    DLCBFLG1,X'FF'-DLCBFXFL                                          
         LA    RE,MISPFK           LAST LINE ON THE SCREEN                      
         L     RF,DLCBAPL          CURRENT DUMMY LINE USED FOR OUTPUT           
         CR    RE,RF               SCREEN FULL?                                 
         BH    DWNLX               . NO, EXIT                                   
*                                                                               
         OI    PCDRIVE2,PC2SCRFL   SCREEN IS FULL                               
*                                                                               
         MVI   MISPFK,C' '         . YES, CLEAR OUT CURRENT DATA                
         MVC   MISPFK+1(L'MISPFK-1),MISPFK                                      
DWNL52   SHI   RE,MISPFK-MISL23                                                 
         LA    R1,MISL8                                                         
         CR    RE,R1               MAKE SURE WE DON'T GO OFF THE SCREEN         
         BNL   *+6                                                              
         DC    H'0'                                                             
         LA    RF,L'MISPFK-1(RE)                                                
DWNL53   CLI   0(RF),C';'          HIT THE SEMI?                                
         BE    DWNLHX              . YES, WE CAN EXIT                           
         MVI   0(RF),C' '          . NO, CLEAR OUT                              
         CR    RF,RE               END OF THE LINE?                             
         BNH   DWNL52              . YES, BACK UP                               
         BCT   RF,DWNL53           . NO, KEEP GOING ON THIS LINE                
*                                                                               
DWNLHX   J     NEQXIT                                                           
DWNLX    J     EQXIT                                                            
*                                                                               
*---------------                                                                
* DOWNLOAD HOOK                                                                 
*---------------                                                                
DWNHOOK  LA    RF,MISL8                                                         
         C     RF,DLCBAPL             ARE WE ON THE FIRST LINE?                 
         L     RF,DLCBAPL             RF=(DISPLAY LINE)                         
         BNE   *+12                   . NO, NOT ON FIRST LINE                   
         LA    RF,MISL9-MISL8(,RF)    . YES, BUMP                               
         B     *+8                                                              
         LA    RF,MISL10-MISL9(,RF)                                             
         ST    RF,DLCBAPL                                                       
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*======================================================================         
* SET GRID OUTPUT MESSAGE                                                       
*======================================================================         
SETGMSG  NTR1  BASE=*,LABEL=*                                                   
         TM    PCDRIVEN,PCIL1Q           GRID INFO LINE ONE?                    
         BZ    SGM50                     . NO                                   
         MVC   MISMSG(GRMSG1LQ),GRMSG1   . YES                                  
*                                                                               
         MVI   MISMSG+GRDEIDQ,C'0'       SET EID BY FORMAT                      
         CLI   FORMIND,C'D'                                                     
         BNE   *+8                                                              
         MVI   MISMSG+GRDEIDQ,C'1'                                              
*                                                                               
         MVI   MISMSG+GRDEIDQ+1,C'0'     SET EID BY WEEK                        
         CLI   WEEKIND,C'W'                                                     
         BNE   *+8                                                              
         MVI   MISMSG+GRDEIDQ+1,C'1'                                            
*                                                                               
         MVI   MISMSG+GRDEIDQ+2,C'0'     STATION LIST                           
         CLC   MISSTA(4),=C'LIST'                                               
         BNE   *+12                                                             
         MVI   MISMSG+GRDEIDQ+2,C'1'                                            
         B     SGM40                                                            
         CLC   MISPRD(3),=C'ALL'         PRODUCT ALL                            
         BNE   *+12                                                             
         MVI   MISMSG+GRDEIDQ+2,C'2'                                            
         B     SGM40                                                            
         CLI   FORMIND,C'M'              MARKET=LIST                            
         BNE   *+12                                                             
         MVI   MISMSG+GRDEIDQ+2,C'3'                                            
         B     SGM40                                                            
         CLI   FORMIND,C'P'              PUR=LIST                               
         BNE   *+8                                                              
         MVI   MISMSG+GRDEIDQ+2,C'4'                                            
*                                                                               
SGM40    TM    SVOPT2,SVOPT2_MLK+SVOPT2_SLK  ANY LOCKIN REQUESTED ?             
         BZ    *+8                                                              
         MVI   MISMSG+GRDEIDQ+1,C'3'                                            
*                                                                               
         NI    PCDRIVEN,X'FF'-PCIL1Q                                            
         CLI   MISL8,C' '                FAILSAFE - NEVER OUTPUT A              
         JH    SGMX                      GRIDS MESSAGE, THEN EXIT WITH          
         J     SGMNEX                    A BLANK SCREEN                         
SGM50    MVC   MISMSG(L'GRMSG2),GRMSG2                                          
         CLI   MISL8,C' '                FAILSAFE - NEVER OUTPUT A              
         JH    SGMX                      GRIDS MESSAGE, THEN EXIT WITH          
         MVI   MISL8,C':'                A BLANK SCREEN                         
         FOUT  MISL8H                                                           
                                                                                
SGMX     CR    RB,RB                                                            
         B     *+6                                                              
SGMNEX   LTR   RB,RB                                                            
         J     XIT                                                              
*                                                                               
GRMSG1   DC    C'GRID NEXTL DOR 43 DATA 48 EID '                                
GRDEIDQ  EQU   *-GRMSG1                                                         
         DC    C'001 WR ',AL1(GCSEPQ)                                           
GRMSG1LQ EQU   *-GRMSG1                                                         
GRMSG2   DC    C'GRID NEXTL DATA 43'                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*======================================================================         
* CHECK D0 PROFILE FOR NO SPILL CONTROL                                         
*======================================================================         
CKNSPROF NTR1  BASE=*,LABEL=*                                                   
         XC    MISMED+1(3),MISMED+1                                             
         MVI   MISMEDH+5,1                                                      
         NI    MISMEDH+4,X'FF'-X'20'                                            
         OI    MISMEDH+6,X'80'     DEFAULT TO SPILL OPTION                      
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0D0'    READ D0 PROFILE                              
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),MISMED                                                 
         MVC   WORK+7(3),MISCLT                                                 
         OI    WORK+9,C' '                                                      
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         XC    WORK2,WORK2         INIT PROFILE WORK AREA                       
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),(R1),WORK,WORK2,VDATAMGR                                    
         CLI   WORK2+12,C'N'                                                    
         JNE   XIT                                                              
         MVC   MISMED+1(3),=C'-NS'                                              
         MVI   MISMEDH+5,4                                                      
         NI    MISMEDH+4,X'FF'-X'20'                                            
         OI    MISMEDH+6,X'80'     SET TO NO SPILL OPTION                       
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*======================================================================         
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
       ++INCLUDE SPMISWORK                                                      
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
GENOLD   DSECT                                                                  
*                                                                               
DAGYHDR  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
DCLTHDR  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
DPRDHDR  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
DESTHDR  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       EJECT                                                                    
* DBLOCK IS A CONTINUATION OF DESTHDR DSECT                                     
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
DMKTREC  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
DSTAREC  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
DMNREC   DSECT                                                                  
       ++INCLUDE SPGENDMN                                                       
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE SPGENNDEF                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPURP                                                      
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE FATWA                                                          
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FATIOB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079SPMIS00   11/19/19'                                      
         END                                                                    
