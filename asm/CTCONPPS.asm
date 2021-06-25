*          DATA SET CTCONPPS   AT LEVEL 004 AS OF 05/17/15                      
*PHASE CONPPSA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE QSORT                                                                  
         TITLE 'CTCONPPS - CONVERT PASSWORD/SYSTEM RECS FOR PPS'                
         PRINT NOGEN                                                            
CTPPS    CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,**CPPS**,=V(REGSAVE),RA,R9                           
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
         SHI   R1,8                                                             
         MVC   0(8,R1),=C'**IOA1**'                                             
*                                                                               
         L     R1,=A(IOAREA2-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO2                                                          
         SHI   R1,8                                                             
         MVC   0(8,R1),=C'**IOA2**'                                             
*                                                                               
         L     R1,=A(PIDTAB)                                                    
         ST    R1,APIDTAB                                                       
         ST    R1,APTNTRY                                                       
         SHI   R1,8                                                             
         MVC   0(8,R1),=C'*PIDTAB*'                                             
*                                                                               
ACD      USING CTAADD,ACDELEM                                                   
         USING PLINED,PLINE                                                     
*                                                                               
         BAS   RE,INIT             READ CARDS ECT                               
         BAS   RE,MAIN             MAIN LOOP                                    
         BAS   RE,DONE             CLOSE ALL                                    
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
EXITEQ   CR    RB,RB                                                            
         B     EXIT                                                             
EXITNE   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
*        READ DATA LINES AND WRITE TO WRKF                                      
***********************************************************************         
MAIN     NTR1                                                                   
*                                                                               
         BAS   RE,GETACCR          GET ACCESS RECORD                            
         BNE   MAINX                                                            
*                                                                               
MAIN010  BAS   RE,GETPASS          GET PASSWORD RECORD                          
         BNE   MAIN020                                                          
         BAS   RE,WRTPASS          CHANGE PASSWORD RECORD FOR PPS               
         B     MAIN010                                                          
MAIN020  BAS   RE,WRTACCR          CHANGE ACCESS RECORD TO PPS                  
         BAS   RE,PRTREP           PRINT REPORT                                 
*                                                                               
MAINX    B     EXITEQ                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        INITIALISE                                                             
***********************************************************************         
INIT     NTR1                                                                   
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
*                                                                               
         MVI   DMBITS,0                                                         
         XC    CUAGY,CUAGY                                                      
         ZAP   COUNT,=P'0'         COUNT FOR RECORDS CHANGED                    
         ZAP   COUNT2,=P'0'        COUNT FOR ERRORS                             
         NI    PGI,X'FF'-PGIPASS   READING PASSWORD RECORDS STARTED             
*                                                                               
         LA    R3,CARD                                                          
INIT010  GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
         CLC   =C'DDSIO=',0(R3)                                                 
         BNE   INIT014                                                          
         L     RF,=V(DDSIO)        OVERRIDE DDSIO NAME                          
         MVC   0(8,RF),6(R3)                                                    
         B     INIT010                                                          
*                                                                               
INIT014  CLC   =C'DSPACE=',0(R3)                                                
         BNE   INIT018                                                          
         L     RF,=A(SSB)          SET DSPACE ID IN SSB                         
         MVC   SSODSPAC-SSOOFF(1,RF),7(R3)                                      
         B     INIT010                                                          
*                                                                               
INIT018  GOTO1 VDATCON,DMCB,(5,0),(1,TODAY)                                     
         GOTO1 (RF),(R1),(5,0),(2,TODAYC)                                       
         GOTO1 (RF),(R1),(5,0),(0,TODAY0)                                       
*                                                                               
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(15),=C'PPS CONVERSION'                                     
         LA    R1,TITLE            PRINT PARAMETER CARDS TITLE                  
         BAS   RE,PRINTT                                                        
         LA    R3,CARD                                                          
         B     INIT032                                                          
*                                                                               
INIT030  GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
INIT032  CLC   =C'/*',0(R3)                                                     
         BE    INIT100                                                          
         CLC   =C'XX',0(R3)                                                     
         BE    INIT100                                                          
         MVC   PLINE+1(80),0(R3)                                                
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         CLC   =C'WRITE=',0(R3)                                                 
         BNE   INIT040                                                          
         CLC   6(3,R3),=C'YES'                                                  
         BNE   *+8                                                              
         MVI   RCWRITE,C'Y'        WRITE RECORD                                 
         B     INIT030                                                          
*                                                                               
INIT040  CLC   =C'AGENCY=',0(R3)                                                
         BNE   INIT050                                                          
         CLI   9(R3),C' '                                                       
         BNH   *+6                                                              
         DC    H'0'                AGENCY=(TWO CHARACTER ALPHA ID)              
         OC    CUAGY,CUAGY                                                      
         BZ    *+6                                                              
         DC    H'0'                PROGRAM RUNS ON 1 AGENCY                     
         MVC   CUAGY,7(R3)                                                      
         B     INIT030                                                          
*                                                                               
INIT050  CLC   =C'MODE=',0(R3)                                                  
         BNE   INIT060                                                          
         CLC   5(3,R3),=C'REV'                                                  
         BNE   INIT060                                                          
         MVI   PMODE,C'R'                                                       
         B     INIT030                                                          
*                                                                               
INIT060  CLC   =C'DUMPREC=',0(R3)                                               
         BNE   INIT070                                                          
         MVC   RDUMP,8(R3)                                                      
         B     INIT030                                                          
*                                                                               
INIT070  CLC   =C'KEEPHISTORY=',0(R3)                                           
         BNE   INIT080                                                          
         MVC   RHISTKP,12(R3)                                                   
         B     INIT030                                                          
*                                                                               
INIT080  B     INIT030                                                          
*                                                                               
INIT100  CLI   RCWRITE,C'Y'        WRITE=Y?                                     
         BNE   *+12                . NO                                         
         MVI   FCTFILE,C'U'        OPEN CTFILE FOR UPDATE                       
         MVI   DMBITS,X'80'        READ FOR UPDATE                              
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMOPEN  ',=C'CONTROL ',FLIST,AIO1               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RCWRITE,C'Y'        ENQUEUE CONTROL IF WRITING                   
         BNE   INIT110                                                          
         GOTO1 VDATAMGR,DMCB,=C'ENQCTL ',(C'E',=C'CTRL')                        
         TM    8(R1),X'04'                                                      
         BO    *+6                                                              
         DC    H'00'                                                            
*                                                                               
INIT110  B     EXITEQ                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        GET ACCESS RECORD AND ACCESS DETAIL ELEMENT                            
***********************************************************************         
DONE     NTR1                                                                   
         CLOSE SYSPRINT            CLOSE PRINT                                  
*                                                                               
         CLI   RCWRITE,C'Y'        ENQUEUE CONTROL IF WRITING                   
         BNE   DONE010                                                          
         GOTO1 VDATAMGR,DMCB,=C'ENQCTL ',(C'D',=C'CTRL')                        
*                                                                               
DONE010  GOTO1 VDATAMGR,DMCB,=C'DMCLSE ',=C'CONTROL ',FLIST,AIO1                
*                                                                               
         B     EXITEQ                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        GET ACCESS RECORD AND ACCESS DETAIL ELEMENT                            
***********************************************************************         
GETACCR  NTR1                                                                   
*                                                                               
         XC    ACDELEM,ACDELEM                                                  
         XC    KEY,KEY             FIND ID REC                                  
         LA    R2,KEY                                                           
         USING CT5REC,R2                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,CUAGY                                                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,KEY,AIO1                             
         CLI   8(R1),0                                                          
         BNE   GACNOAC             ACCESS RECORD? AGENCY DOES NOT EXIST         
*                                                                               
         L     R2,AIO1                                                          
         LA    R4,CT5DATA                                                       
GAC010   CLI   0(R4),0             END OF RECORD                                
         BE    GACNOAC             ACCESS DETAIL ELEMENT REQUIRED               
         CLI   0(R4),CTAADELQ                                                   
         BE    GAC020                                                           
         SR    RF,RF                                                            
         ICM   RF,1,1(R4)          NEXT                                         
         AR    R4,RF                                                            
         B     GAC010                                                           
*                                                                               
GAC020   MVC   ACDELEM,0(R4)       SAVE ACCESS DETAIL ELEMENT                   
*                                                                               
         MVC   PLAGY,CUAGY                                                      
         BAS   RE,PRINTL                                                        
*                                                                               
GACEX    B     EXITEQ                                                           
*                                                                               
GACNOAC  MVC   PLAGY,CUAGY                                                      
         MVC   PLMESS,MNOACC       NO ACCESS RECORD FOUND                       
         BAS   RE,PRINTL                                                        
*                                                                               
GACNX    B     EXITNE                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CHANGE ACCESS RECORD FOR PPS                                           
***********************************************************************         
WRTACCR  NTR1                                                                   
*                                                                               
         XC    KEY,KEY             FIND ID REC                                  
         LA    R2,KEY                                                           
         USING CT5REC,R2                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,CUAGY                                                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),CTFILE,KEY,AIO1                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                PROBLEM FINDING ONE PASSWORD RECORD          
*                                                                               
         L     R2,AIO1                                                          
         LA    R4,CT5DATA                                                       
WAC010   CLI   0(R4),0             END OF RECORD                                
         BNE   *+6                                                              
         DC    H'0'                ACCESS DETAIL ELEMENT REQUIRED               
         CLI   0(R4),CTAADELQ                                                   
         BE    WAC020                                                           
         SR    RF,RF                                                            
         ICM   RF,1,1(R4)          NEXT                                         
         AR    R4,RF                                                            
         B     WAC010                                                           
*                                                                               
         USING CTAADD,R4                                                        
WAC020   OI    CTAADFLG,CTAADPRQ         TURN ON PPS                            
         CLI   PMODE,C'R'                REVERSE MODE                           
         BNE   *+8                                                              
         NI    CTAADFLG,X'80'-CTAADPRQ   TURN OFF PPS IN REVERSE MODE           
                                                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   WAC050                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMWRT,CTFILE,AIO1,AIO1                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CTENQU,C'Y'                                                      
*                                                                               
WAC050   AP    COUNT,=P'1'                                                      
*                                                                               
         B     EXITEQ                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        GET PASSWORD RECORD                                                    
***********************************************************************         
GETPASS  NTR1                                                                   
         XC    CUPID,CUPID         CLEAR PID                                    
         XC    CUPIDN,CUPIDN       CLEAR PID NUMBER                             
         XC    CUPASS,CUPASS       CLEAR PASSWORD                               
         XC    CUPASSLN,CUPASSLN                                                
         NI    PGI,X'FF'-PGIERR    ERROR                                        
*                                                                               
         TM    PGI,PGIPASS                                                      
         BO    GPA006                                                           
         OI    PGI,PGIPASS         READING PASSWORD RECORDS STARTED             
*                                                                               
         BAS   RE,PRINTL                 HEADLINES                              
         MVC   PLINE(L'MWRITE),MWRITE                                           
         BAS   RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BAS   RE,PRINTL                                                        
         BAS   RE,PRINTL                                                        
         MVC   PLINE(L'H1),H1                                                   
         BAS   RE,PRINTL                                                        
         MVC   PLINE(L'H2),H2                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
         XC    KEY,KEY             FIND ID REC                                  
         LA    R2,KEY                                                           
         USING SA0REC,R2                                                        
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,CUAGY                                                    
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMRDHI),CTFILE,KEY,AIO2                    
         B     GPA010                                                           
GPA006   GOTO1 VDATAMGR,DMCB,(DMBITS,DMRSEQ),CTFILE,KEY,AIO2                    
GPA010   CLI   8(R1),0                                                          
         BNE   GPANX                                                            
         L     R2,AIO2                                                          
         CLI   SA0KTYP,SA0KTYPQ                                                 
         BNE   GPANX                                                            
         CLC   SA0KAGY,CUAGY                                                    
         BNE   GPANX                                                            
         OC    SA0KEYS(SA0KNUM-SA0KEYS),SA0KEYS                                 
         BNZ   GPANX               ONLY WANT PASSWORD NUMBER RECORDS            
         MVC   KEY,0(R2)                                                        
         MVC   CUPIDN,SA0KNUM                                                   
*                                                                               
         LA    R4,SA0DATA                                                       
GPA020   CLI   0(R4),0             END OF RECORD                                
         BE    GPA050                                                           
         CLI   0(R4),SAPASELQ      LOOK FOR PASSWORD TEXT                       
         BE    GPA030                                                           
         CLI   0(R4),SAPALELQ      LOOK FOR PID                                 
         BE    GPA040                                                           
GPA022   SR    R0,R0                                                            
         IC    R0,1(R4)            NEXT                                         
         AR    R4,R0                                                            
         B     GPA020                                                           
*                                                                               
         USING SAPASD,R4                                                        
GPA030   OC    CUPASS,CUPASS       ALREADY HAVE PASSWORD                        
         BNZ   GPA022              . YES, WE DON'T NEED ANOTHER ONE             
         CLI   SAPASDTA,C' '       SOMETHING MUST BE THERE                      
         BNH   GPA022                                                           
         MVC   CUPASS,SPACES       SAVE CURRENT PASSWORD                        
*                                                                               
         SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         LA    R5,0(R6,R4)                                                      
         AHI   R5,-1               REMOVE END SPACES                            
GPA032   CR    R5,R4                                                            
         BNH   GPA022                                                           
         CLI   0(R5),C' '                                                       
         BH    GPA034                                                           
         AHI   R5,-1                                                            
         AHI   R6,-1                                                            
         B     GPA032                                                           
*                                                                               
GPA034   AHI   R6,-2                                                            
         CHI   R6,L'SA0KCODE                                                    
         BNH   GPA035                                                           
         LHI   R6,L'SA0KCODE                                                    
         MVC   PLMESS,MPASSSH                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
GPA035   STC   R6,CUPASSLN                                                      
         AHI   R6,-1                                                            
         BM    GPA022              WHERE IS THE PASSWORD?                       
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   CUPASS(0),SAPASDTA                                               
         B     GPA022                                                           
         DROP  R4                                                               
*                                                                               
         USING SAPALD,R4                                                        
GPA040   MVC   CUPID,SAPALPID      SAVE CURRENT PID                             
         B     GPA022                                                           
         DROP  R4                                                               
*                                                                               
GPA050   OC    CUPASS,CUPASS                                                    
         BNZ   GPA060                                                           
         OI    PGI,PGIERR          ERROR                                        
         MVC   PLHEAD,MERROR                                                    
         MVC   PLMESS,MNOPASS      NO PASSWORD FOUND                            
         B     GPA070                                                           
*                                                                               
GPA060   OC    CUPID,CUPID                                                      
         BNZ   GPA070                                                           
         OI    PGI,PGIERR          ERROR                                        
         MVC   PLHEAD,MERROR                                                    
         MVC   PLMESS,MNOPID       NO PID ELEMENT                               
*                                                                               
GPA070   MVC   PLPID,CUPID                                                      
         MVC   PLAGY,CUAGY                                                      
         GOTO1 VHEXOUT,DMCB,CUPIDN,PLPIDN,2                                     
         BAS   RE,PRINTL                                                        
*                                                                               
         CLI   RDUMP,C'N'                                                       
         BE    GPAEX                                                            
         GOTOR DUMPREC,DMCB,(C'R',AIO2)                                         
*                                                                               
GPAEX    B     EXITEQ                                                           
GPANX    B     EXITNE                                                           
         EJECT                                                                  
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CHANGE PASSWORD RECORD FOR PPS                                         
***********************************************************************         
WRTPASS  NTR1                                                                   
         TM    PGI,PGIERR                                                       
         BZ    WPA002                                                           
         AP    COUNT2,=P'1'                                                     
         B     WPAEX                                                            
                                                                                
WPA002   L     R2,AIO2                                                          
         USING SA0REC,R2                                                        
         CLI   RHISTKP,C'Y'                                                     
         BE    WPA010                                                           
*                                  DELETE ANY SAPWHELQ (X'E4') ELEMENTS         
         GOTO1 VHELLO,DMCB,(C'D',CTFILE),('SAPWHELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BE    WPA010                                                           
         CLI   12(R1),X'06'        ELEMENT NOT FOUND, OK                        
         BE    WPA010                                                           
         DC    H'0'                ANOTHER PROBLEM WITH HELLO                   
*                                                                               
WPA010   CLI   PMODE,C'R'          REVERSE MODE, WE'RE DONE                     
         BE    WPA040                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING SAPWHD,R4                                                        
         MVI   SAPWHEL,SAPWHELQ    BUILD PASSWORD HISTORY ELEMENT               
         MVI   SAPWHLN,SAPWHLNQ                                                 
*                                  GET TODAY'S DATE (YYMMDD)                    
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         SR    RF,RF                                                            
         ICM   R1,3,SA0KNUM        PID#                                         
         ICM   RF,1,ACD.CTAADPTO   PASSWORD EXPIRES IN DAYS                     
         BP    *+8                                                              
         LHI   RF,90               NOTHING THERE, THEN USE 90                   
         DR    R0,RF               / PASSWORD EXPIRE DAYS                       
         SR    R0,RF               REMAINDER OF DIVISION - VALUE                
         ST    R0,DMCB+8                                                        
         GOTO1 VADDAY,DMCB,TODAY0,DATE0                                         
         GOTO1 VDATCON,DMCB,(0,DATE0),(2,SAPWHDTE)                              
*                                                                               
         TIME  BIN                                                              
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                END OF THE WORLD                             
         STCM  R0,7,SAPWHTME                                                    
*                                                                               
         SR    RE,RE                                                            
         IC    RE,CUPASSLN                                                      
         SR    RF,RF               UPDATE THE LENGTH                            
         IC    RF,SAPWHLN                                                       
         AR    RF,RE                                                            
         STC   RF,SAPWHLN                                                       
*                                                                               
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAPWHPWD(0),CUPASS                                               
*                                                                               
         OI    SAPWHFLG,SAPWHPPS   PASSWORD SET BY PPS CONVERSION               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',CTFILE),((R2)),ELEM,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'               COULD NOT ADD CONVERSION ELEMENT             
                                                                                
WPA040   CLI   RCWRITE,C'Y'                                                     
         BNE   WPA050                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMWRT,CTFILE,KEY,AIO2                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,KEY,AIO2  RESTORE SEQUENCE           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WPA050   AP    COUNT,=P'1'                                                      
         CLI   RDUMP,C'N'                                                       
         BE    WPA054                                                           
         GOTOR DUMPREC,DMCB,(C'W',AIO2)                                         
*                                                                               
WPA054   L     R1,APTNTRY                                                       
         USING PIDTABD,R1                                                       
         MVC   PTAGY,CUAGY                                                      
         MVC   PTPID,CUPID                                                      
         MVC   PTPIDN,CUPIDN                                                    
         AHI   R1,PIDTABLQ                                                      
         ST    R1,APTNTRY                                                       
*                                                                               
         S     R1,APIDTAB                                                       
         SR    R0,R0                                                            
         LHI   RF,PIDTABLQ                                                      
         DR    R0,RF                                                            
         C     R1,=A(MAXPIDS)                                                   
         BNH   *+6                                                              
         DC    H'0'                TOO MANY PIDS, INCREASE TABLE                
         DROP  R1                                                               
*                                                                               
WPAEX    B     EXITEQ                                                           
         B     EXITNE                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PRINT REPORT                                                           
***********************************************************************         
PRTREP   NTR1                                                                   
                                                                                
         L     RF,APTNTRY                                                       
         S     RF,APIDTAB                                                       
         SR    RE,RE                                                            
         LHI   R1,PIDTABLQ                                                      
         DR    RE,R1                                                            
*                                                                               
         GOTO1 VQSORT,DMCB,(0,APIDTAB),(RF),PIDTABLQ,PIDTABLQ,0                 
*                                                                               
         BAS   RE,PRINTL                 HEADLINES                              
         MVC   PLINE(L'MREPORT),MREPORT                                         
         BAS   RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BAS   RE,PRINTL                                                        
         BAS   RE,PRINTL                                                        
         MVC   PLINE(L'H1),H1                                                   
         BAS   RE,PRINTL                                                        
         MVC   PLINE(L'H2),H2                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
         L     R5,APIDTAB                PRINT PID TABLE CONTENTS               
         USING PIDTABD,R5                                                       
PR010    C     R5,APTNTRY                                                       
         BNL   PR050                                                            
         MVC   PLAGY,PTAGY                                                      
         MVC   PLPID,PTPID                                                      
         GOTO1 VHEXOUT,DMCB,PTPIDN,PLPIDN,2                                     
         BAS   RE,PRINTL                                                        
         LA    R5,PIDTABLQ(R5)                                                  
         B     PR010                                                            
*                                                                               
PR050    BAS   RE,PRINTL                 HEADLINES                              
         BAS   RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BAS   RE,PRINTL                                                        
         MVC   PLPID(5),=C'TOTAL'                                               
         BAS   RE,PRINTL                                                        
         MVC   PLPID,DASHES                                                     
         BAS   RE,PRINTL                                                        
*                                                                               
         EDIT  COUNT,PLPID,ZERO=NOBLANK                                         
         MVC   PLMESS,MRECNT                                                    
         BAS   RE,PRINTL                                                        
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BE    PREX                                                             
         MVC   PLMESS,MDRAFT                                                    
         BAS   RE,PRINTL                                                        
         BAS   RE,PRINTL                                                        
*                                                                               
         EDIT  COUNT2,PLPID,ZERO=NOBLANK                                        
         MVC   PLMESS,MERCNT                                                    
         BAS   RE,PRINTL                                                        
*                                                                               
PREX     B     EXITEQ                                                           
         B     EXITNE                                                           
         DROP  R5                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        DUMP RECORDS                                                           
***********************************************************************         
DUMPREC  NTR1                                                                   
*                                                                               
         L     R2,0(R1)                                                         
         LA    R2,0(R2)                                                         
         MVC   PLINE(4),=C'READ'                                                
         CLI   0(R1),C'R'                                                       
         BE    DR004                                                            
         MVC   PLINE(5),=C'WRITE'                                               
         CLI   RCWRITE,C'Y'                                                     
         BE    DR004                                                            
         MVC   PLINE+6(31),=C'** WRITE=NO, NO ACTUAL WRITE **'                  
DR004    BAS   RE,PRINTL                                                        
         LHI   R5,SAPEDATA-SAPEKEY                                              
         GOTO1 VHEXOUT,DMCB,(R2),PLINE,(R5)                                     
         BAS   RE,PRINTL                                                        
*                                                                               
         LA    R2,SAPEDATA-SAPEKEY(R2)                                          
DR010    CLI   0(R2),0                                                          
         BE    DR020                                                            
         SR    R4,R4                                                            
         IC    R4,1(R2)                                                         
*                                                                               
         CLI   RDUMP,C'P'          ONLY DUMP PASSWORD/PID RELATED ELEMS         
         BNE   DR012                                                            
         CLI   0(R2),X'01'                                                      
         BE    DR012                                                            
         CLI   0(R2),X'03'                                                      
         BE    DR012                                                            
         CLI   0(R2),X'C3'                                                      
         BE    DR012                                                            
         CLI   0(R2),X'E4'                                                      
         BE    DR012                                                            
         B     DR014                                                            
*                                                                               
DR012    LR    R5,R4                                                            
         CHI   R5,L'PLINE/2                                                     
         BNH   *+8                                                              
         LHI   R5,L'PLINE/2                                                     
         GOTO1 VHEXOUT,DMCB,(R2),PLINE,(R5)                                     
         BAS   RE,PRINTL                                                        
DR014    AR    R2,R4                                                            
         B     DR010                                                            
*                                                                               
DR020    MVC   PLINE(2),=C'00'                                                  
         BAS   RE,PRINTL                                                        
         BAS   RE,PRINTL                                                        
*                                                                               
DREX     B     EXITEQ                                                           
         B     EXITNE                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PRINT ROUTINES                                                         
***********************************************************************         
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
*        CONSTANTS & LTORG                                                      
***********************************************************************         
VDATAMGR DC    V(DATAMGR)                                                       
VCARDS   DC    V(CARDS)                                                         
VPERVAL  DC    V(PERVAL)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VHELLO   DC    V(HELLO)                                                         
VDATCON  DC    V(DATCON)                                                        
VADDAY   DC    V(ADDAY)                                                         
VQSORT   DC    V(QSORT)                                                         
*                                                                               
DMREAD   DC    CL8'DMREAD '                                                     
DMRDHI   DC    CL8'DMRDHI '                                                     
DMRSEQ   DC    CL8'DMRSEQ '                                                     
DMWRT    DC    CL8'DMWRT  '                                                     
CTFILE   DC    CL8'CTFILE '                                                     
SPACES   DC    166C' '                                                          
DASHES   DC    166C'-'                                                          
DOTS     DC    CL16'................'                                           
MAXLINE  DC    P'60'                                                            
*                                                                               
CTENQU   DC    C'N'                                                             
*                                                                               
RCWRITE  DC    C'N'                                                             
PMODE    DC    C'C'                                                             
RDUMP    DC    C'N'                                                             
RHISTKP  DC    C'N'                                                             
*                                                                               
         DC    0D                                                               
         DC    C'**FLST**'                                                      
FLIST    DS    0CL8                                                             
FCTFILE  DC    C'NCTFILE '                                                      
         DC    C'X       '                                                      
*                                                                               
MERROR   DC    CL10'**ERROR***'                                                 
MNOPASS  DC    CL40'NO PASSWORD FOUND ON PASSWORD RECORD   '                    
MNOPID   DC    CL40'NO PID FOUND ON PASSWORD RECORD        '                    
MNOACC   DC    CL40'NO ACCESS RECORD/ACCESS DETAILS FOUND  '                    
MRECNT   DC    CL40'RECORDS CHANGED TOTAL                  '                    
MERCNT   DC    CL40'RECORDS IN ERROR                       '                    
MDRAFT   DC    CL40'**WRITE=NO, NO RECORDS ACTUALLY CHANGED'                    
MEMPTYP  DC    CL40'PASSWORD ELEMENT FOUND, BUT NO PASSWORD'                    
MWRITE   DC    CL40'RECORDS IN NUMBER ORDER - READ/WRITE   '                    
MREPORT  DC    CL40'RECORDS IN PID ORDER - REPORT          '                    
MPASSSH  DC    CL40'PASSWORD SHORTENED TO MAX (10)         '                    
*                                                                               
H1       DC    C'           AGY   PID         PIDN      NOTE'                   
H2       DC    C'           ---   --------    ----      -------------'          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        DCBS & ADCONS                                                          
***********************************************************************         
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
         DC    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    F'0',X'0A',XL3'00',XL252'00'                                     
         DC    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
SSB      DC    X'0000'                                                          
         DC    X'FF'                                                            
         DC    AL1(SSOSNRCV)                                                    
         DC    1024X'00'                                                        
*                                                                               
       ++INCLUDE FATABSDEQU                                                     
*                                                                               
         DS    16D                                                              
*                                                                               
         DS    CL8                                                              
PIDTAB   DS    (MAXPIDS)XL(PIDTABLQ) TABLE OF PIDS THAT WERE CHANGED            
MAXPIDS  EQU   20000                                                            
PTLENQ   EQU   MAXPIDS*PIDTABLQ                                                 
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
*                                                                               
AIO1     DS    A                   A(IO AREA 1)                                 
AIO2     DS    A                   A(IO AREA 2)                                 
*                                                                               
APIDTAB  DS    A                   A(PERSON ID TABLE)                           
APTNTRY  DS    A                   A(NEXT AVAILABLE PIDTAB ENTRY)               
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
DMBITS   DS    X                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
COUNT    DS    PL8                                                              
COUNT2   DS    PL8                                                              
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
*                                                                               
TODAY    DS    CL3                 YYMMDD PWOS                                  
TODAYC   DS    H                   TODAY COMP                                   
TODAY0   DS    CL6                 TODAY YYMMDD                                 
DATE0    DS    CL6                 DATE YYMMDD                                  
*                                                                               
PGI      DS    X                   PROGRAM INDICATOR                            
PGIPASS  EQU   X'80'               . STARTED READING PASSWORD RECS              
PGIERR   EQU   X'40'               . ERROR                                      
*                                                                               
CARD     DS    CL80                                                             
KEY      DS    CL40                                                             
         DS    CL40                                                             
*                                                                               
CUAGY    DS    CL2                 AGENCY                                       
CUPASSLN DS    X                   PASSWORD LENGTH                              
CUPASS   DS    CL10                PASSWORD                                     
CUPID    DS    CL8                 PERSIN ID                                    
CUPIDN   DS    XL2                 PERSIN ID NUMBER                             
*                                                                               
ACDELEM  DS    XL(CTAADLNQ)        ACCESS DETAILS ELEMENT                       
ELEM     DS    XL256               GENERAL USE ELEMENT                          
*                                                                               
         DS    CL8                                                              
IOAREA1  DS    2048C               IO AREA 1                                    
         DS    CL8                                                              
IOAREA2  DS    2048C               IO AREA 1                                    
*                                                                               
*                                                                               
SPARE    DS    1024X                                                            
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
PIDTABD  DSECT                                                                  
PTAGY    DS    CL2                                                              
PTPID    DS    CL8                                                              
PTPIDN   DS    XL2                                                              
PIDTABLQ EQU   *-PIDTABD                                                        
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
PLHEAD   DS    CL10                                                             
         DS    CL1                                                              
PLAGY    DS    CL2                                                              
         DS    CL4                                                              
PLPID    DS    CL8                                                              
         DS    CL4                                                              
PLPIDN   DS    CL4                                                              
         DS    CL6                                                              
PLMESS   DS    CL40                                                             
         DS    CL40                                                             
         EJECT                                                                  
         DCBD    DSORG=QS,DEVD=DA                                               
*                                                                               
* DDPERVALD                                                                     
* CTGENFILE                                                                     
* SEACSFILE                                                                     
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
                                                                                
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTCONPPS  05/17/15'                                      
         END                                                                    
