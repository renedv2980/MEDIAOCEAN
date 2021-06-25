*          DATA SET DDCOMINT   AT LEVEL 010 AS OF 11/18/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE T00A5AE                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE NSIWEEK                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE COMSTACN                                                               
*                                                                               
         TITLE 'COMSCORE INTERFACE'                                             
COMINTER CSECT                                                                  
         NMOD1 WORKL,**COMINTER**,RR=RE,CLEAR=YES                               
         USING WORKD,RC            RC=A(WORKING STORAGE)                        
         ST    RE,RELO                                                          
         ST    R1,APARMS                                                        
*                                                                               
         BAS   RE,GETVALS          GET VALUES FROM INPUT                        
* PASS 1 MODES                                                                  
         CLI   MODE,ALLOCQ         ALLOCATE & OPEN DATASET                      
         JE    PROCALLO                                                         
         CLI   MODE,PUTQ           BUILD DEMO REQUEST & PUT TO DATASET          
         JE    PROCPUT                                                          
         CLI   MODE,CLOSEQ         CLOSE DATASET                                
         JE    PROCCLSE                                                         
* PASS 2 MODES                                                                  
         CLI   MODE,STARTQ         OPEN DATASET & BUILD BINSRCH TABLE           
         JE    PROCSTRT                                                         
         CLI   MODE,GETQ           GET DEMOS FROM BINSRCH TABLE                 
         JE    PROCGET                                                          
         CLI   MODE,ENDQ           DEL BINSRCH TABLE & DEALLOC DATASET          
         JE    PROCEND                                                          
         J     PARMERR                                                          
*                                                                               
* TRAP WAS ASKED TO BE PUT IN BUT IT IS CHANGING THE CONDITION CODE             
* ON EXIT - SCHT 11/17                                                          
*                                                                               
*EXIT     OC    VMASTC,VMASTC                                                   
*         JZ    EXITX                                                           
*         L     RF,VMASTC                                                       
*         OC    0(100,RF),0(RF)     TRAP                                        
*         JZ    *+2                                                             
*         CLC   0(100,RF),=CL100' '                                             
*         JE    *+2                                                             
*EXITX    XMOD1 ,                                                               
*                                                                               
EXIT     XMOD1 ,                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         XIT1                                                                   
***********************************************************************         
* ALLOCATE & OPEN DATASET FOR DEMO REQUEST RECORDS                              
***********************************************************************         
PROCALLO BRAS  RE,GETTOK           READ TOKEN RECORD INFO                       
         JNE   TOKERR                                                           
*                                                                               
         GOTO1 VDYN,DMCB,(X'80',=CL8'FILEOUT'),(X'45',FILESIZE),       *        
               (X'80',DSN),0,0                                                  
*                                                                               
         L     R2,=A(FILEOUT)      OPEN DATASET                                 
         OPEN  ((R2),(OUTPUT))                                                  
         LTR   RF,RF                                                            
         JNZ   OPENERR                                                          
*                                                                               
         BRAS  RE,PUTTOK           PUT TOKEN RECORD INFO TO DATASET             
*                                                                               
         BRAS  RE,PUTHDR           PUT HEADER RECORD TO DATASET                 
*                                                                               
PALLOX   J     EXIT                                                             
***********************************************************************         
* CLOSE DEMO REQUEST RECORD DATASET & PUT MQ MESSAGE                            
***********************************************************************         
PROCCLSE CLOSE FILEOUT             CLOSE DATASET                                
         LTR   RF,RF                                                            
         JNZ   CLSEERR                                                          
*                                                                               
         BRAS  RE,PUTMQ            PUT MQ MESSAGE                               
         JNE   MQERR                                                            
         J     EXIT                                                             
***********************************************************************         
* CLOSE DEMO VALUES DATASET & RELEASE BINSRCH TABLE                             
***********************************************************************         
PROCEND  CLOSE FILEIN              CLOSE DATASET                                
PEND10   LTR   RF,RF                                                            
         BNZ   CLSEERR                                                          
*                                                                               
         SAM31                                                                  
         OC    ABINTAB,ABINTAB     OBTAINED STORAGE FOR BINSRCH TABLE?          
         JZ    PENDX                                                            
         L     R3,BINTABLN         RELEASE STORAGE                              
         AHI   R3,8                ADD 8 BYTES FOR EYECATCHER                   
         AHI   R3,4                ADD 4 BYTES FOR # ENTRIES IN TABLE           
         L     R2,ABINTAB                                                       
         SHI   R2,4                BUMP BACK BEFORE # ENTRIES IN TABLE          
         SHI   R2,8                BUMP BACK BEFORE EYECATCHER                  
         STORAGE RELEASE,LENGTH=(R3),ADDR=(R2),COND=YES                         
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
PENDX    SAM24                                                                  
         J     EXIT                                                             
***********************************************************************         
* GET DEMOS FROM BINSRCH TABLE                                                  
***********************************************************************         
PROCGET  CLI   SYSTEM,SPOTQ        SPOT?                                        
         JNE   *+12                                                             
         BRAS  RE,PGETS            GET SPOT RECORD                              
         JNE   FAILERR                                                          
*                                                                               
         CLI   SYSTEM,NETQ         NET?                                         
         JNE   *+12                                                             
         BRAS  RE,PGETN            GET NET RECORD                               
         JNE   FAILERR                                                          
*                                                                               
PROCGETX J     EXIT                                                             
***********************************************************************         
* BUILD DEMO REQUEST & PUT TO DATASET                                           
***********************************************************************         
PROCPUT  CLI   SYSTEM,SPOTQ        SPOT?                                        
         JNE   *+16                                                             
         BRAS  RE,PPUTS            PUT SPOT REQUEST RECORD                      
         JNE   FAILERR                                                          
         J     PPUT10                                                           
*                                                                               
         CLI   SYSTEM,NETQ         NET?                                         
         JNE   PROCPUTX                                                         
         BRAS  RE,PPUTN            PUT NET REQUEST RECORD                       
         JNE   FAILERR                                                          
         J     PPUT10                                                           
*                                                                               
PPUT10   GOTOR SQUASHIT,DMCB,DSNREC    REMOVE NULLS & SPACES                    
         PUT   FILEOUT,OUTREC      PUT DEMO REQUEST RECORD TO DATASET           
*                                                                               
PROCPUTX J     EXIT                                                             
***********************************************************************         
* OPEN DEMO VALUES DATASET & OBTAIN SPACE FOR BINSRCH TABLE                     
***********************************************************************         
PROCSTRT GOTO1 VDYN,DMCB,(X'FF',=CL8'FILEIN'),(X'05',DSN),0,0,0                 
*                                                                               
         L     R2,=A(FILEIN)       OPEN DATASET                                 
         OPEN  ((R2),(INPUT))                                                   
         LTR   RF,RF                                                            
         BNZ   OPENERR                                                          
*                                                                               
         L     R1,=A(FILEIN)                                                    
         LA    R0,OUTREC                                                        
         GET   (1),(0)             GET HEADER RECORD                            
*                                                                               
         LA    RE,DSNREC           SKIP FIRST 4 BYTES FROM DATASET              
         LA    RF,DSNRECLN                                                      
         LA    R0,OUTREC+4                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   DSNREC,C'H'         MUST BE HEADER                               
         JNE   DSNERR                                                           
         BAS   RE,GETBINLN         GET SIZE FOR BINSRCH TABLE                   
*                                                                               
         SAM31                                                                  
         L     R3,BINTABLN         SIZE OF BINSRCH TABLE                        
         AHI   R3,8                FOR EYECATCHER                               
         AHI   R3,4                FOR NUMBER OF ENTRIES IN THE TABLE           
*                                                                               
         STORAGE OBTAIN,LENGTH=(R3),BNDRY=PAGE,COND=YES,LOC=31                  
         LTR   RF,RF                                                            
         JNZ   *+2                 ERROR                                        
*                                                                               
         MVC   0(8,R1),=C'*BINTAB*'                                             
         AHI   R1,8                FOR EYECATCHER                               
         MVC   0(4,R1),SVNUMREC                                                 
         AHI   R1,4                FOR NUMBER OF ENTRIES IN THE TABLE           
         ST    R1,ABINTAB          A(BINSRCH TABLE)                             
         SAM24                                                                  
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         L     RF,MCAEXTRA                                                      
         USING MCEXTRA,RF                                                       
         MVC   MC31BUFF,ABINTAB    SET MC31BUFF                                 
         DROP  RF                                                               
*                                                                               
         CLI   SYSTEM,SPOTQ        SPOT?                                        
         JNE   *+12                                                             
         BRAS  RE,PSTRTS                                                        
         J     PSTRTX                                                           
*                                                                               
         CLI   SYSTEM,NETQ         NET?                                         
         JNE   *+12                                                             
         BRAS  RE,PSTRTN                                                        
         J     PSTRTX                                                           
*                                                                               
PSTRTX   J     EXIT                                                             
***********************************************************************         
* GET SIZE FOR BINSRCH TABLE FROM DATASET RECORD                                
***********************************************************************         
GETBINLN NTR1                                                                   
         XC    SVNUMREC,SVNUMREC                                                
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
*                                                                               
         LA    R3,DSNREC                                                        
         AHI   R3,2                BUMP PAST RECORD TYPE AND DELIMITER          
*                                                                               
         CLI   SYSTEM,NETQ         NET?                                         
         JNE   GBINLN02                                                         
         GOTOR BLDFLD,DMCB,(R3),C'~',NUMERICQ  BLD FLD HDR                      
         JNE   DSNERR                                                           
         J     GBINLN04                                                         
*                                                                               
GBINLN02 GOTOR BLDFLD,DMCB,(R3),C',',NUMERICQ  BLD FLD HDR                      
         JNE   DSNERR                                                           
*                                                                               
GBINLN04 LA    R2,FLDH                                                          
         LLC   RE,5(R2)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         ST    RE,SVNUMREC         SET NUMBER OF RECORDS                        
*                                                                               
         SR    R2,R2               CALC SPACE NEEDED FOR BINSRCH TABLE          
         SR    R3,R3                                                            
         L     R3,SVNUMREC         TOTAL NUMBER OF RECORDS                      
*                                                                               
         LA    R4,BINRECLN         L'BINSRCH REC LENGTH                         
         CLI   SVCALLER,SVCNNPQ    NPOD?                                        
         JNE   *+8                                                              
         LA    R4,BRNRECLN         L'BINSRCH REC LENGTH                         
         CLI   SVCALLER,SVCNSDQ    NET SEED?                                    
         JNE   *+8                                                              
         LA    R4,BNDRECLN         L'BINSRCH REC LENGTH                         
*                                                                               
         MR    R2,R4                                                            
         ST    R3,BINTABLN         SET SIZE FOR BINSRCH TABLE                   
*                                                                               
GBINLNX  J     EXIT                                                             
***********************************************************************         
* GET REQUIRED VALUES                                                           
***********************************************************************         
GETVALS  NTR1                                                                   
         MVI   MODE,0                                                           
*                                                                               
         L     RF,=V(BINSRCH)                                                   
         A     RF,RELO                                                          
         ST    RF,VBINSRCH                                                      
*                                                                               
         L     RF,=V(GETBROAD)                                                  
         A     RF,RELO                                                          
         ST    RF,VGETBROD                                                      
*                                                                               
         L     RF,=V(NSIWEEK)                                                   
         A     RF,RELO                                                          
         ST    RF,VNSIWEEK                                                      
*                                                                               
         L     RF,=V(SQUASHER)                                                  
         A     RF,RELO                                                          
         ST    RF,VSQUASHR                                                      
*                                                                               
         L     R2,APARMS                                                        
         USING PARAMSD,R2                                                       
         MVI   PARAM1,0            INIT ERROR RETURN CODE                       
         OC    PARAM1,PARAM1       ACTION                                       
         JZ    PARMERR                                                          
*                                                                               
         OC    PARAM2,PARAM2       A(DBLOCK)                                    
         JZ    PARMERR                                                          
         MVC   ADBLOCK,PARAM2                                                   
*                                                                               
         L     RF,ADBLOCK          A(DBLOCK)                                    
         USING DBLOCKD,RF                                                       
         MVC   ACOMFACS,DBCOMFCS   A(COMFACS)                                   
         MVC   SVDEMTYP,DBDEMTYP   TYPE OF DEMO LIST                            
         MVI   SVCALLER,0          CALLING PROGRAM                              
         MVC   VTYPE,=C'RL'        VIEWING TYPE LIVE - DEFAULT                  
*                                                                               
         OC    DBEXTEND,DBEXTEND   ANY EXTENSION BLOCK?                         
         JZ    GVAL08                                                           
*                                                                               
         L     RF,DBEXTEND         FIND COMSCORE EXTENSION BLOCK                
GVAL02   CLC   =C'CLOC',0(RF)                                                   
         JNE   *+12                                                             
         ST    RF,ADBXCLOC         A(DBEXTEND - CLOC EXTENSION)                 
         MVI   SVCALLER,SVCSPTQ    SPOT                                         
*                                                                               
         CLC   =C'CSRN',0(RF)                                                   
         JNE   *+12                                                             
         ST    RF,ADBXCSRN         A(DBEXTEND - CSRN EXTENSION)                 
         MVI   SVCALLER,SVCNNPQ    NPOD                                         
*                                                                               
         CLC   =C'NSED',0(RF)                                                   
         JNE   *+12                                                             
         ST    RF,ADBXNSED         A(DBEXTEND - NSED EXTENSION)                 
         MVI   SVCALLER,SVCNSDQ    NET SEED                                     
*                                                                               
         CLC   =C'NWRI',0(RF)                                                   
         JNE   GVAL04                                                           
         ST    RF,ADBXNWRI         A(DBEXTEND - NWRI EXTENSION)                 
         MVI   SVCALLER,SVCNWRQ    NET WRITER                                   
*                                                                               
         USING DBCNWD,RF                                                        
         CLI   DBCNWVT,NPKVTRCQ    PAV LIVE COMMERCIAL                          
         JNE   *+10                                                             
         MVC   VTYPE,=C'RC'                                                     
         CLI   DBCNWVT,NPKVTR3Q    PAV LIVE+3 COMMERCIAL                        
         JNE   *+10                                                             
         MVC   VTYPE,=C'R3'                                                     
         CLI   DBCNWVT,NPKVTR7Q    PAV LIVE+7 COMMERCIAL                        
         JNE   *+10                                                             
         MVC   VTYPE,=C'R7'                                                     
*                                                                               
GVAL04   CLC   =C'NEXT',0(RF)                                                   
         JNE   *+12                                                             
         ST    RF,ADBXNEXT         A(DBEXTEND - NEXT EXTENSION)                 
         MVI   SVCALLER,SVCNEXQ    NET EXTRACT                                  
*                                                                               
         CLC   =C'NLIV',0(RF)      NETWORK LIVE INDICATOR                       
         JNE   GVAL06                                                           
         USING DBXLIVD,RF                                                       
         CLI   DBXLIVE,DBXLCLCQ    PAV LIVE COMMERCIAL                          
         JNE   *+10                                                             
         MVC   VTYPE,=C'RC'                                                     
         CLI   DBXLIVE,DBXLCL3Q    PAV LIVE+3 COMMERCIAL                        
         JNE   *+10                                                             
         MVC   VTYPE,=C'R3'                                                     
         CLI   DBXLIVE,DBXLCL7Q    PAV LIVE+7 COMMERCIAL                        
         JNE   *+10                                                             
         MVC   VTYPE,=C'R7'                                                     
*                                                                               
GVAL06   OC    4(4,RF),4(RF)       ANY NEXT EXTENSION BLOCK?                    
         JZ    GVAL08                                                           
         ICM   RF,15,4(RF)         GO TO NEXT EXTENSION BLOCK                   
         J     GVAL02                                                           
         DROP  RF                                                               
*                                                                               
GVAL08   L     RF,PARAM1                                                        
         CLC   =C'ALLOC',0(RF)     ALLOCATE & OPEN DATASET                      
         JNE   *+12                                                             
         MVI   MODE,ALLOCQ                                                      
         J     GVAL10                                                           
         CLC   =C'PUT',0(RF)       BUILD DEMO REQUEST & PUT TO DATASET          
         JNE   *+12                                                             
         MVI   MODE,PUTQ                                                        
         J     GVAL10                                                           
         CLC   =C'CLOSE',0(RF)     CLOSE DATASET                                
         JNE   *+12                                                             
         MVI   MODE,CLOSEQ                                                      
         J     GVAL10                                                           
         CLC   =C'START',0(RF)     OPEN DATASET & BUILD BINSRCH TABLE           
         JNE   *+12                                                             
         MVI   MODE,STARTQ                                                      
         J     GVAL10                                                           
         CLC   =C'GET',0(RF)       GET DEMOS FROM BINSRCH TABLE                 
         JNE   *+12                                                             
         MVI   MODE,GETQ                                                        
         J     GVAL10                                                           
         CLC   =C'END',0(RF)       DEL BINSRCH TABLE & DEALLOC DATASET          
         JNE   PARMERR                                                          
         MVI   MODE,ENDQ                                                        
*                                                                               
GVAL10   L     RF,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,RF                                                      
         MVC   VMASTC,CMASTC-COMFACSD(RF)                                       
         MVC   VDMGR,CDATAMGR-COMFACSD(RF)                                      
         MVC   VDATCON,CDATCON-COMFACSD(RF)                                     
         MVC   VGETDAY,CGETDAY-COMFACSD(RF)                                     
         MVC   VADDAY,CADDAY-COMFACSD(RF)                                       
         MVC   VDYN,CDYNALOC-COMFACSD(RF)                                       
         MVC   VDEMTABS,CDEMTABS-COMFACSD(RF)                                   
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         MVC   VMQRPT,MCVMQRPT     A(MQ)                                        
         MVC   SYSTEM,MCOVSYS      SYSTEM                                       
         MVC   USER,MCUSER         ALPHA AGENCY                                 
*                                                                               
         CLI   SYSTEM,NETQ         NET?                                         
         JNE   GVAL20                                                           
         CLC   =C'*325',MCLOAD     NPOD?                                        
         JNE   *+10                                                             
         CLC   =C'PD',MCPROG                                                    
         JNE   *+8                                                              
         MVI   SVCALLER,SVCNNPQ    NPOD                                         
*                                                                               
         CLC   =C'*31E',MCLOAD     SEED?                                        
         JNE   *+8                                                              
         MVI   SVCALLER,SVCNSDQ    NET SEED                                     
*                                                                               
         CLC   =C'*320',MCLOAD     WRITER?                                      
         JNE   GVAL20                                                           
         CLC   =C'WR',MCPROG                                                    
         JE    *+10                                                             
         CLC   =C'W2',MCPROG                                                    
         JE    *+10                                                             
         CLC   =C'W3',MCPROG                                                    
         JNE   *+8                                                              
         MVI   SVCALLER,SVCNWRQ    NET WRITER                                   
*                                                                               
GVAL20   L     RF,MCAEXTRA                                                      
         USING MCEXTRA,RF                                                       
         MVC   DSN,MCMVSDSN        DATASET NAME                                 
         MVC   ABINTAB,MC31BUFF    A(BINSRCH TAB): MODES END/GET/START          
*                                                                               
GETVALSX J     EXIT                                                             
         DROP  R2,RF                                                            
***********************************************************************         
* READ AND PUT TOKEN INFO TO DEMO REQUEST DATASET                               
***********************************************************************         
GETTOK   NTR1                                                                   
         GOTO1 VDMGR,DMCB,=CL8'DMOPEN',=CL8'CONTROL',GENFILES,WORK              
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY              GET SECURITY AGENCY ALPHA                    
         USING CT5KEY,R3                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ                                                 
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,USER       ALPHA AGENCY                                 
         MVC   HALF,USER           SET SECURITY AGENCY ALPHA                    
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,IO                    
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
*                                                                               
         USING CTSEAD,R3                                                        
         LA    R3,IO                                                            
         MVI   ELCODE,CTSEAELQ                                                  
         MVC   DATADISP,=H'28'                                                  
         BAS   RE,GETEL                                                         
         JNE   *+10                                                             
         MVC   HALF,CTSEAAID       SECURITY AGENCY ALPHA                        
         DROP  R3                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING TOKRECD,R3          BUILD TOKEN RECORD KEY                       
         MVI   TOKKMIN,TOKKMINQ                                                 
         MVI   TOKKTYP,TOKKRTRK                                                 
*                                                                               
         MVC   TOKKSAGY,HALF       SECURITY AGENCY                              
         MVC   TOKKAAGY,USER       ALPHA AGENCY                                 
         MVC   TOKKSYS,SYSTEM      SYSTEM                                       
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEY,KEY                   
         CLC   KEY(L'TOKKEY),KEYSAVE                                            
         JNE   NO                                                               
*                                                                               
         GOTO1 VDMGR,DMCB,=CL8'GETREC',=CL8'GENFIL',KEY+36,IO,WORK              
         CLI   DMCB+8,0                                                         
         JNE   NO                                                               
*                                                                               
         LA    R3,IO                                                            
         AHI   R3,TOKFIRST                                                      
         USING RTAUTHD,R3                                                       
         CLI   0(R3),RTAUTELQ      X'0A' ELEMENT                                
         JNE   NO                                                               
         OC    RTAUTID(L'RTAUTID+L'RTAUTSEC),RTAUTID                            
         JZ    NO                                                               
         DROP  R3                                                               
*                                                                               
GETTOKX  J     YES                                                              
*                                                                               
GENFILES DS    0D                                                               
         DC    CL8' GENDIR'                                                     
         DC    CL8' GENFIL'                                                     
         DC    C'X'                END OF LIST                                  
***********************************************************************         
* READ AND PUT TOKEN INFO TO DEMO REQUEST DATASET                               
***********************************************************************         
PUTTOK   NTR1                                                                   
         LA    R3,IO                                                            
         AHI   R3,TOKFIRST                                                      
         USING RTAUTHD,R3                                                       
         MVI   DSNREC,C'A'         TOKEN RECORD                                 
         MVI   DSNREC+1,C','                                                    
         MVC   DSNREC+2(L'RTAUTID),RTAUTID    USER ID TOKEN                     
         PUT   FILEOUT,DSNREC                                                   
*                                                                               
         MVI   DSNREC,C'B'         TOKEN RECORD                                 
         MVI   DSNREC+1,C','                                                    
         MVC   DSNREC+2(L'RTAUTSEC),RTAUTSEC  SECRET CODE TOKEN                 
         PUT   FILEOUT,DSNREC                                                   
         DROP  R3                                                               
*                                                                               
PUTTOKX  J     YES                                                              
***********************************************************************         
* ERROR HANDLING                                                                
***********************************************************************         
CLSEERR  L     RF,APARMS                                                        
         USING PARAMSD,RF                                                       
         MVI   PARAM1,OPCLERRQ     UNABLE TO CLOSE DATASET                      
         J     EXIT                                                             
*                                                                               
DSNERR   L     RF,APARMS                                                        
         USING PARAMSD,RF                                                       
         MVI   PARAM1,DSNERRQ      DATASET ERROR                                
         J     EXIT                                                             
*                                                                               
FAILERR  SAM24                                                                  
         L     RF,APARMS                                                        
         USING PARAMSD,RF                                                       
         MVI   PARAM1,FAILERRQ     DEMO REQUEST FAILED                          
         J     EXIT                                                             
*                                                                               
MQERR    L     RF,APARMS                                                        
         USING PARAMSD,RF                                                       
         MVI   PARAM1,MQERRQ       UNABLE TO PUT MQ MESSAGE OUT                 
         J     EXIT                                                             
*                                                                               
OPENERR  L     RF,APARMS                                                        
         USING PARAMSD,RF                                                       
         MVI   PARAM1,OPCLERRQ     UNABLE TO OPEN DATASET                       
         J     EXIT                                                             
*                                                                               
PARMERR  L     RF,APARMS                                                        
         USING PARAMSD,RF                                                       
         MVI   PARAM1,PARMERRQ     INVALID PARAMETER                            
         J     EXIT                                                             
*                                                                               
PUTERR   L     RF,APARMS                                                        
         USING PARAMSD,RF                                                       
         MVI   PARAM1,PUTERRQ      UNABLE TO PUT RECORD TO DATASET              
         J     EXIT                                                             
*                                                                               
TOKERR   L     RF,APARMS                                                        
         USING PARAMSD,RF                                                       
         MVI   PARAM1,TOKERRQ      TOKEN RECORD MISSING                         
         J     EXIT                                                             
         DROP  RF                                                               
***********************************************************************         
* CONTANTS, LITERALS, ETC.                                                      
***********************************************************************         
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,LRECL=400,MACRF=PM              
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,LRECL=550,MACRF=GM,     *        
               EODAD=EXIT                                                       
FILESIZE DC    AL3(100,100)                                                     
*                                                                               
SVNUMREC DC    A(0)                GET NUMBER OF RECORDS FROM DATASET           
*                                                                               
         GETEL (R3),DATADISP,ELCODE                                             
         LTORG                                                                  
***********************************************************************         
* PUT HEADER RECORD TO DEMO REQUEST DATASET                                     
***********************************************************************         
PUTHDR   NTR1                                                                   
         CLI   SYSTEM,SPOTQ        SPOT?                                        
         JE    PHDRS00                                                          
         CLI   SYSTEM,NETQ         NET?                                         
         JE    PHDRN00                                                          
         J     PUTHDRX                                                          
*                                                                               
PHDRS00  MVC   DSNREC(112),=C'H,STATION,MARKET,SYSCODE,BOOK,START_DATE,*        
               END_DATE,ROTATION,START_TIME,END_TIME,HASHKEY,PRECISION,*        
               SPOT_DATE,DEMOS'                                                 
         J     PUTHDRX                                                          
*                                                                               
PHDRN00  CLI   SVCALLER,SVCNNPQ    NPOD?                                        
         JNE   PHDRN02                                                          
         MVC   DSNREC(112),=C'H,NETWORK,STARTDATE,ENDDATE,ROTATION,STAR*        
               TTIME,ENDTIME,STARTOFDAYTIME,PRECISION,VIEWINGTYPE,TP/PA*        
               V,DEMOS        '                                                 
         J     PUTHDRX                                                          
*                                                                               
PHDRN02  CLI   SVCALLER,SVCNSDQ    NET SEED?                                    
         JNE   PHDRN04                                                          
         MVC   DSNREC(112),=C'H,NETWORKNUMBER,STARTDATE,ENDDATE,STARTTI*        
               ME,ENDTIME,HASHKEY                                      *        
                              '                                                 
         J     PUTHDRX                                                          
*                                                                               
PHDRN04  CLI   SVCALLER,SVCNWRQ    NET WRITER?                                  
         JNE   PUTHDRX                                                          
         MVC   DSNREC(121),=C'H,HASHKEY,NETWORK#,STARTDATE,ENDDATE,ROTA*        
               TION,STARTTIME,ENDTIME,STARTOFDAYTIME,PRECISION,VIEWINGT*        
               YPE,TP/PAV,SERIES#,DEMOS'                                        
         J     PUTHDRX                                                          
*                                                                               
PUTHDRX  PUT   FILEOUT,DSNREC                                                   
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
IO       DS    XL2000              IO AREA                                      
*                                                                               
***********************************************************************         
* BUILD BINSRCH RECORD FROM DSNREC                                              
*   ON EXIT: FIELD 'BINREC' IS SET                                              
***********************************************************************         
BLDBIN   NTR1  BASE=*,LABEL=*                                                   
         LA    RE,BINREC                                                        
         LA    RF,BINRECLN                                                      
         XCEF                                                                   
*                                                                               
         CLI   SYSTEM,SPOTQ        SPOT?                                        
         JE    BBINS00                                                          
         CLI   SYSTEM,NETQ         NET?                                         
         JE    BBINN00                                                          
         J     BLDBINX                                                          
*                                                                               
* SPOT                                                                          
*                                                                               
BBINS00  LA    R2,DSNREC                                                        
         AHI   R2,2                BUMP PAST RECORD TYPE & DELIMITER            
         LA    R3,BINREC                                                        
         USING BINRECD,R3                                                       
*                                                                               
         GOTOR UNSQUASH,DMCB,=C'HASHKEY',0(R2)   UNSQUASH HASHKEY               
         MVC   BINHASH,OUTREC      SET HASHKEY                                  
*                                                                               
         L     R2,ANXTFLD          A(DEMO CATEGORIES)                           
         LA    R4,BINCATS          SET DEMO CATEGORIES                          
BBINS20  CLI   0(R2),0             END OF RECORD?                               
         JE    BLDBINX                                                          
         CLI   0(R2),C' '          END OF RECORD?                               
         JE    BLDBINX                                                          
         CLI   0(R2),C','          DONE WITH DEMO CATEGORIES?                   
         JE    BBINS25                                                          
*                                                                               
         MVC   0(1,R4),0(R2)       SET DEMO CATEGORY                            
         AHI   R2,1                                                             
         AHI   R4,1                                                             
         J     BBINS20                                                          
*                                                                               
BBINS25  AHI   R2,1                BUMP PAST DELIMITER                          
         LA    R4,BINVALS          SET DEMO VALUES                              
BBINS30  CLI   0(R2),C','          END OF DEMO VALUES?                          
         JE    BBINS40                                                          
         CLC   =C'PASS',0(R2)                                                   
         JE    BBINS40                                                          
         CLC   =C'FAIL',0(R2)                                                   
         JE    BBINS40                                                          
*                                                                               
         GOTOR BLDFLD,DMCB,(R2),C'|',NUMERICQ  BLD FLD HDR                      
         JNE   BLDBINX                                                          
*                                                                               
         CLC   =C'null',FLD                                                     
         JNE   *+14                                                             
         MVC   0(4,R4),=X'FFFFFFFF'   MARK IT NULL                              
         J     BBINS35                                                          
*                                                                               
         LA    RF,FLDH                                                          
         LLC   RE,5(RF)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,RF)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,15,0(R4)         SET DEMO VALUE IN BINREC                     
*                                                                               
BBINS35  ZIC   RF,FLDH+5           L'ALPHA VALUE                                
         AR    R2,RF               BUMP PAST IT                                 
         AHI   R2,1                BUMP PAST DELIMITER                          
         AHI   R4,DEMVALLQ         BUMP TO NEXT DEMO VALUE SLOT                 
         J     BBINS30                                                          
*                                                                               
BBINS40  MVC   BINSTAT,0(R2)       PASS/FAIL                                    
*                                                                               
         AHI   R2,L'BINSTAT+1                                                   
         MVC   BINPNAM,0(R2)       COMSCORE PROGRAM NAME                        
*                                                                               
         MVI   BINSEQ,0            INIT SEQUENCE NUMBER                         
         OC    PREVKEY,PREVKEY     FIRST TIME THROUGH?                          
         JNZ   *+14                                                             
         MVC   PREVKEY,BINHASH     SAVE HASHKEY + SEQUENCE                      
         J     BLDBINX                                                          
*                                                                               
         CLC   BINHASH,PREVHKEY    SAME HASHKEY?                                
         JNE   BBINS50                                                          
         ZIC   RF,PREVHSEQ         BUMP UP SEQUENCE NUMBER                      
         AHI   RF,1                                                             
         STC   RF,BINSEQ                                                        
BBINS50  MVC   PREVKEY,BINHASH     SAVE HASHKEY + SEQUENCE                      
         J     BLDBINX                                                          
*                                                                               
* NET                                                                           
*                                                                               
BBINN00  CLI   SVCALLER,SVCNNPQ    NPOD?                                        
         JE    BBINNP00                                                         
         CLI   SVCALLER,SVCNSDQ    SEED?                                        
         JE    BBINND00                                                         
         CLI   SVCALLER,SVCNWRQ    WRITER?                                      
         JE    BBINNR00                                                         
         J     BLDBINX                                                          
*                                                                               
* NET RESEARCH WRITER (NPOD) - DELIMITER IS SEMICOLON                           
*                                                                               
BBINNP00 LA    RE,BRNREC                                                        
         LA    RF,BRNRECLN                                                      
         XCEF                                                                   
*                                                                               
         LA    R2,DSNREC                                                        
         AHI   R2,2                BUMP PAST RECORD TYPE & DELIMITER            
         LA    R3,BRNREC                                                        
         USING BRNRECD,R3                                                       
*                                                                               
         LA    R3,BRNSTA           NETWORK                                      
         MVC   BRNSTA,=CL10' '                                                  
BBINNP10 CLI   0(R2),C'~'                                                       
         JE    BBINNP12                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP10                                                         
BBINNP12 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNSTAN          NETWORK NAME                                 
BBINNP13 CLI   0(R2),C'~'                                                       
         JE    BBINNP14                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP13                                                         
BBINNP14 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNPROG          PROGRAM                                      
BBINNP15 CLI   0(R2),C'~'                                                       
         JE    BBINNP16                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP15                                                         
BBINNP16 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNDATE          DATE                                         
BBINNP18 CLI   0(R2),C'~'                                                       
         JE    BBINNP20                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP18                                                         
BBINNP20 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         MVC   BRNROT,0(R2)        ROTATION                                     
         AHI   R2,L'BRNROT+1                                                    
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNSTIM          START TIME                                   
BBINNP22 CLI   0(R2),C'~'                                                       
         JE    BBINNP24                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP22                                                         
BBINNP24 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNETIM          END TIME                                     
BBINNP26 CLI   0(R2),C'~'                                                       
         JE    BBINNP28                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP26                                                         
BBINNP28 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         CLC   =C'0000',BRNSTIM                                                 
         BNE   *+10                                                             
         MVC   BRNSTIM,=C'2400'                                                 
         CLC   =C'0000',BRNETIM                                                 
         BNE   *+10                                                             
         MVC   BRNETIM,=C'2400'                                                 
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNVTYPE         VIEWING TYPE                                 
BBINNP30 CLI   0(R2),C'~'                                                       
         JE    BBINNP32                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP30                                                         
BBINNP32 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNSN            SERIES NUMBER                                
BBINNP34 CLI   0(R2),C'~'                                                       
         JE    BBINNP36                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP34                                                         
BBINNP36 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNEN            EPISODE NUMBER                               
BBINNP38 CLI   0(R2),C'~'                                                       
         JE    BBINNP40                                                         
         CLC   =C'NULL',0(R2)                                                   
         JE    *+14                                                             
         CLC   =C'null',0(R2)                                                   
         JNE   *+12                                                             
         AHI   R2,4                                                             
         J     BBINNP40                                                         
*                                                                               
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP38                                                         
BBINNP40 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNET            EPISODE TITLE                                
BBINNP42 CLI   0(R2),C'~'                                                       
         JE    BBINNP44                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP42                                                         
BBINNP44 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNIRA           IS REPEAT AIRING?                            
         MVI   0(R3),C'N'                                                       
BBINNP46 CLI   0(R2),C'~'                                                       
         JE    BBINNP48                                                         
         CLC   =C'NULL',0(R2)                                                   
         JE    *+14                                                             
         CLC   =C'null',0(R2)                                                   
         JNE   *+12                                                             
         AHI   R2,4                                                             
         J     BBINNP48                                                         
*                                                                               
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP46                                                         
BBINNP48 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNISP           IS SEASON PREMIER?                           
         MVI   0(R3),C'N'                                                       
BBINNP50 CLI   0(R2),C'~'                                                       
         JE    BBINNP52                                                         
         CLC   =C'NULL',0(R2)                                                   
         JE    *+14                                                             
         CLC   =C'null',0(R2)                                                   
         JNE   *+12                                                             
         AHI   R2,4                                                             
         J     BBINNP52                                                         
*                                                                               
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP50                                                         
BBINNP52 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNISA           IS SPECIAL AIRING?                           
         MVI   0(R3),C'N'                                                       
BBINNP54 CLI   0(R2),C'~'                                                       
         JE    BBINNP56                                                         
         CLC   =C'NULL',0(R2)                                                   
         JE    *+14                                                             
         CLC   =C'null',0(R2)                                                   
         JNE   *+12                                                             
         AHI   R2,4                                                             
         J     BBINNP56                                                         
*                                                                               
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP54                                                         
BBINNP56 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNNETN          NETWORK NUMBER                               
BBINNP58 CLI   0(R2),C'~'                                                       
         JE    BBINNP60                                                         
         CLC   =C'NULL',0(R2)                                                   
         JE    *+14                                                             
         CLC   =C'null',0(R2)                                                   
         JNE   *+12                                                             
         AHI   R2,4                                                             
         J     BBINNP60                                                         
*                                                                               
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP58                                                         
BBINNP60 AHI   R2,1                                                             
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNCATS          SET DEMO CATEGORIES                          
BBINNP90 CLI   0(R2),0             END OF RECORD?                               
         JE    BLDBINX                                                          
         CLI   0(R2),C' '          END OF RECORD?                               
         JE    BLDBINX                                                          
         CLI   0(R2),C'~'          DONE WITH DEMO CATEGORIES?                   
         JE    BBINNP92                                                         
*                                                                               
         MVC   0(1,R3),0(R2)       SET DEMO CATEGORY                            
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNP90                                                         
BBINNP92 AHI   R2,1                BUMP PAST DELIMITER                          
*                                                                               
         LA    R3,BRNREC                                                        
         LA    R3,BRNVALS          SET DEMO VALUES                              
BBINNP94 CLI   0(R2),C'~'          END OF DEMO VALUES?                          
         JE    BBINNP98                                                         
         CLC   =C'PASS',0(R2)                                                   
         JE    BBINNP98                                                         
         CLC   =C'FAIL',0(R2)                                                   
         JE    BBINNP98                                                         
*                                                                               
         GOTOR BLDFLD,DMCB,(R2),C'|',NUMERICQ  BLD FLD HDR                      
         JNE   BLDBINX                                                          
*                                                                               
         CLC   =C'null',FLD                                                     
         JNE   *+14                                                             
         MVC   0(4,R3),=X'00000000'   MARK IT NULL                              
         J     BBINNP96                                                         
*                                                                               
         LA    RF,FLDH                                                          
         LLC   RE,5(RF)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,RF)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,15,0(R3)         SET DEMO VALUE IN BRNREC                     
*                                                                               
BBINNP96 ZIC   RF,FLDH+5           L'ALPHA VALUE                                
         AR    R2,RF               BUMP PAST IT                                 
         AHI   R2,1                BUMP PAST DELIMITER                          
         AHI   R3,DEMVALLQ         BUMP TO NEXT DEMO VALUE SLOT                 
         J     BBINNP94                                                         
*                                                                               
BBINNP98 LA    R3,BRNREC                                                        
         MVC   BRNSTAT,0(R2)       PASS/FAIL                                    
         J     BLDBINX                                                          
*                                                                               
* NET SEED REPORT (NPOD) - DELIMITER IS SEMICOLON                               
*                                                                               
BBINND00 LA    RE,BNDREC                                                        
         LA    RF,BNDRECLN                                                      
         XCEF                                                                   
*                                                                               
         LA    R2,DSNREC                                                        
         AHI   R2,2                BUMP PAST RECORD TYPE & DELIMITER            
         LA    R3,BNDREC                                                        
         USING BNDRECD,R3                                                       
*                                                                               
         LA    R3,BNDHASH          HASHKEY                                      
         MVC   BNDHASH,=CL50' '                                                 
BBINND10 CLI   0(R2),C'~'                                                       
         JE    BBINND12                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINND10                                                         
BBINND12 AHI   R2,1                                                             
*                                                                               
         LA    R3,BNDREC                                                        
         LA    R3,BNDSNUM          SERIES NUMBER                                
BBINND14 CLI   0(R2),C'~'                                                       
         JE    BBINND16                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINND14                                                         
BBINND16 AHI   R2,1                                                             
*                                                                               
         LA    R3,BNDREC                                                        
         LA    R3,BNDSNAME         SERIES NAME                                  
BBINND18 CLI   0(R2),C'~'                                                       
         JE    BBINND20                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINND18                                                         
BBINND20 AHI   R2,1                                                             
*                                                                               
         LA    R3,BNDREC                                                        
         LA    R3,BNDDAY           DAY                                          
BBINND22 CLI   0(R2),C'~'                                                       
         JE    BBINND24                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINND22                                                         
BBINND24 AHI   R2,1                                                             
*                                                                               
         LA    R3,BNDREC                                                        
         LA    R3,BNDSTIM          START TIME                                   
BBINND26 CLI   0(R2),C'~'                                                       
         JE    BBINND28                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINND26                                                         
BBINND28 AHI   R2,1                                                             
*                                                                               
         LA    R3,BNDREC                                                        
         LA    R3,BNDETIM          END TIME                                     
BBINND30 CLI   0(R2),C'~'                                                       
         JE    BBINND32                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINND30                                                         
BBINND32 AHI   R2,1                                                             
*                                                                               
         LA    R3,BNDREC                                                        
         USING BNDRECD,R3                                                       
         MVI   BNDSEQ,0            INIT SEQUENCE NUMBER                         
         OC    PHNDKEY,PHNDKEY     FIRST TIME THROUGH?                          
         JNZ   *+14                                                             
         MVC   PHNDKEY,BNDHASH     SAVE HASHKEY + SEQUENCE                      
         J     BLDBINX                                                          
*                                                                               
         CLC   BNDHASH,PHNDHKEY    SAME HASHKEY?                                
         JNE   BBINND34                                                         
         ZIC   RF,PHNDHSEQ         BUMP UP SEQUENCE NUMBER                      
         AHI   RF,1                                                             
         STC   RF,BNDSEQ                                                        
BBINND34 MVC   PHNDKEY,BNDHASH     SAVE HASHKEY + SEQUENCE                      
         J     BLDBINX                                                          
*                                                                               
* NET WRITER - DELIMITER IS SEMICOLON                                           
*                                                                               
BBINNR00 LA    RE,BNWREC                                                        
         LA    RF,BNWRECLN                                                      
         XCEF                                                                   
*                                                                               
         LA    R2,DSNREC                                                        
         AHI   R2,2                BUMP PAST RECORD TYPE & DELIMITER            
         LA    R3,BNWREC                                                        
         USING BNWRECD,R3                                                       
*                                                                               
         LA    R3,BNWHASH          HASHKEY                                      
         MVC   BNWHASH,=CL100' '                                                
BBINNR10 CLI   0(R2),C'~'                                                       
         JE    BBINNR12                                                         
         MVC   0(1,R3),0(R2)                                                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNR10                                                         
BBINNR12 AHI   R2,1                                                             
*                                                                               
         LA    R3,BNWREC                                                        
         LA    R3,BNWCATS          SET DEMO CATEGORIES                          
BBINNR14 CLI   0(R2),C'~'          DONE WITH DEMO CATEGORIES?                   
         JE    BBINNR16                                                         
         MVC   0(1,R3),0(R2)       SET DEMO CATEGORY                            
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BBINNR14                                                         
BBINNR16 AHI   R2,1                BUMP PAST DELIMITER                          
*                                                                               
         LA    R3,BNWREC                                                        
         LA    R3,BNWVALS          SET DEMO VALUES                              
BBINNR18 CLI   0(R2),C'~'          END OF DEMO VALUES?                          
         JE    BBINNR22                                                         
         CLC   =C'PASS',0(R2)                                                   
         JE    BBINNR22                                                         
         CLC   =C'FAIL',0(R2)                                                   
         JE    BBINNR22                                                         
*                                                                               
         GOTOR BLDFLD,DMCB,(R2),C'|',NUMERICQ  BLD FLD HDR                      
         JNE   BLDBINX                                                          
*                                                                               
         CLC   =C'null',FLD                                                     
         JNE   *+14                                                             
         MVC   0(4,R3),=X'00000000'   MARK IT NULL                              
         J     BBINNR20                                                         
*                                                                               
         LA    RF,FLDH                                                          
         LLC   RE,5(RF)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,RF)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,15,0(R3)         SET DEMO VALUE IN BRNREC                     
*                                                                               
BBINNR20 ZIC   RF,FLDH+5           L'ALPHA VALUE                                
         AR    R2,RF               BUMP PAST IT                                 
         AHI   R2,1                BUMP PAST DELIMITER                          
         AHI   R3,DEMVALLQ         BUMP TO NEXT DEMO VALUE SLOT                 
         J     BBINNR18                                                         
*                                                                               
BBINNR22 LA    R3,BNWREC                                                        
         MVC   BNWSTAT,0(R2)       PASS/FAIL                                    
*                                                                               
         LA    R3,BNWREC                                                        
         AHI   R2,L'BNWSTAT+1                                                   
         MVC   BNWPNAM,0(R2)       COMSCORE PROGRAM NAME                        
*                                                                               
         LA    R3,BNWREC                                                        
         XC    BNWSEQ,BNWSEQ       INIT SEQUENCE NUMBER                         
         OC    PHNWKEY,PHNWKEY     FIRST TIME THROUGH?                          
         JNZ   *+14                                                             
         MVC   PHNWKEY,BNWHASH     SAVE HASHKEY + SEQUENCE                      
         J     BLDBINX                                                          
*                                                                               
         CLC   BNWHASH,PHNWHKEY    SAME HASHKEY?                                
         JNE   BBINNR24                                                         
         SR    RF,RF               BUMP UP SEQUENCE NUMBER                      
         ICM   RF,15,PHNWHSEQ                                                   
         AHI   RF,1                                                             
         STCM  RF,15,BNWSEQ                                                     
BBINNR24 MVC   PHNWKEY,BNWHASH     SAVE HASHKEY + SEQUENCE                      
         J     BLDBINX                                                          
         DROP  R3                                                               
*                                                                               
BLDBINX  J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* BUILD FLD HEADER FROM ALPHA VALUES                                            
*   ON ENTRY: PARAM 1 - A(ALPHA VALUES)                                         
*             PARAM 2 - DELIMITER (BYTE 0)                                      
*             PARAM 3 - TYPE OF INPUT (BYTE 0)                                  
*                       X'01' - NUMERIC FIELD                                   
*   ON EXIT: FLDH + FLD IS A FIELD HEADER WITH ALPHA VALUES                     
*            ANXTFLD HAS A(NEXT FIELD)                                          
***********************************************************************         
BLDFLD   NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            A(ALPHA VALUES)                              
         ZIC   RF,4(R1)            DELIMITER                                    
         STC   RF,DELIMITR                                                      
         ZIC   RF,11(R1)           FLAGS                                        
         STC   RF,BYTE2                                                         
*                                                                               
         LA    R2,FLD                                                           
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
*                                                                               
BLDFLD10 CLI   0(R3),0             END OF RECORD?                               
         JE    BLDFLD20                                                         
*                                                                               
         TM    BYTE2,NUMERICQ      NUMERIC INPUT?                               
         JZ    BLDFLD14                                                         
*                                                                               
         CLC   =C'null',0(R3)                                                   
         JNE   BLDFLD12                                                         
         MVC   FLD(4),=C'null'                                                  
         MVI   FLDH+5,4                                                         
         J     BLDFLD30                                                         
*                                                                               
BLDFLD12 CLI   0(R3),C'0'          NUMERIC VALUE?                               
         BL    BLDFLD20            NO - EXIT                                    
*                                                                               
BLDFLD14 CLI   0(R3),C' '          END OF RECORD?                               
         JE    BLDFLD20                                                         
         CLI   0(R3),C','          END OF RECORD?                               
         JE    BLDFLD20                                                         
         CLI   0(R3),C';'          END OF RECORD?                               
         JE    BLDFLD20                                                         
         CLI   0(R3),C'~'          END OF RECORD?                               
         JE    BLDFLD20                                                         
         CLC   0(1,R3),DELIMITR    REACHED DELIMITER?                           
         JE    BLDFLD20                                                         
         MVC   0(1,R2),0(R3)                                                    
*                                                                               
         ZIC   RF,FLDH+5                                                        
         AHI   RF,1                                                             
         STC   RF,FLDH+5           UPDATE INPUT FIELD LENGTH                    
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     BLDFLD10                                                         
*                                                                               
BLDFLD20 AHI   R3,1                                                             
         ST    R3,ANXTFLD          A(NEXT FIELD                                 
         CLI   FLDH+5,0                                                         
         JE    NO                                                               
*                                                                               
BLDFLD30 ZIC   RF,FLDH+5           SET TOTAL FIELD LENGTH                       
         AHI   RF,8                L'FLD HDR                                    
         STC   RF,FLDH                                                          
*                                                                               
BLDFLDX  J     YES                                                              
*                                                                               
EBCDICQ  EQU   X'00'               EBCDIC FILED                                 
NUMERICQ EQU   X'01'               NUMERIC FIELD                                
BYTE2    DS    X                                                                
*                                                                               
         LTORG                                                                  
***********************************************************************         
* BUILD DEMO CATEGORY LIST                                                      
***********************************************************************         
BLDDCAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,DEMOCATS         A(DEMO CATEGORY LIST)                        
         XC    DEMOCATS,DEMOCATS                                                
*                                                                               
         CLI   SYSTEM,SPOTQ        SPOT?                                        
         JE    BCATS00                                                          
         CLI   SYSTEM,NETQ         NET?                                         
         JE    BCATN00                                                          
         J     BLDDCATX                                                         
*                                                                               
* SPOT                                                                          
*                                                                               
BCATS00  L     R3,ADBXCLOC         A(DBEXTEND - CLOC EXTENSION)                 
         USING DBCMINTD,R3                                                      
         L     R4,DBDEMOL          A(REQUESTED DEMO LIST)                       
*                                                                               
BCATS10  CLI   0(R4),X'FF'         ANY MORE DEMOS?                              
         JE    BCATS30                                                          
*                                                                               
         CLI   SVDEMTYP,C'4'       4 BYTE DEMO CATEGORIES?                      
         JNE   BCATS12                                                          
         OC    1(2,R4),1(R4)       COMSCORE DEMO?                               
         JNZ   BCATS25                                                          
         ZIC   RE,3(R4)            THE N'TH DEMO IN THE LIST                    
         J     BCATS14                                                          
*                                                                               
BCATS12  CLI   2(R4),0             COMSCORE DEMO?                               
         JNE   BCATS25                                                          
         ZIC   RE,1(R4)            THE N'TH DEMO IN THE LIST                    
*                                                                               
BCATS14  SHI   RE,1                                                             
         MH    RE,=H'9'            DISPLACEMENT INTO BUY DEMO LIST              
*                                                                               
         L     RF,DBCMAEST         A(COMSCORE DEMOS FROM BUY)                   
         USING NTDELEM,RF                                                       
         LA    RF,NTDDMONM                                                      
         AR    RF,RE                                                            
*                                                                               
         LA    RE,8                L'ENONTDMS                                   
BCATS16  CLI   0(RF),C' '                                                       
         JE    BCATS20                                                          
         CLI   0(RF),0                                                          
         JE    BCATS20                                                          
         MVC   0(1,R2),0(RF)       SET DEMO CATEGORY                            
         AHI   R2,1                                                             
         AHI   RF,1                                                             
         BCT   RE,BCATS16                                                       
*                                                                               
BCATS20  MVI   0(R2),C'|'          DELIMITER                                    
         AHI   R2,1                                                             
BCATS25  AHI   R4,3                BUMP TO NEXT REQUESTED DEMO                  
         CLI   SVDEMTYP,C'4'       4 BYTE DEMO CATEGORIES?                      
         JNE   *+8                                                              
         AHI   R4,1                                                             
         J     BCATS10                                                          
*                                                                               
BCATS30  SHI   R2,1                REMOVE LAST DELIMITER                        
         CLI   0(R2),C'|'                                                       
         JNE   *+8                                                              
         MVI   0(R2),0                                                          
         J     BLDDCATX                                                         
*                                                                               
BCATN00  CLI   SVCALLER,SVCNNPQ    NPOD?                                        
         JE    BCATNP00                                                         
         CLI   SVCALLER,SVCNWRQ    NET WRITER?                                  
         JE    BCATNR00                                                         
         J     BLDDCATX                                                         
*                                                                               
* NET                                                                           
*                                                                               
*                                                                               
* NET RESEARCH WRITER (NPOD)                                                    
*                                                                               
BCATNP00 L     R3,ADBXCSRN         A(DBEXTEND - CSRN EXTENSION)                 
         USING DBCSRND,R3                                                       
*                                                                               
         LA    RE,8                                                             
         L     RF,DBCSRDNL         A(DEMO NAME LIST)                            
*                                                                               
BCATNP10 CLI   0(RF),0                                                          
         JE    BCATNP20                                                         
         MVC   0(8,R2),0(RF)       SET DEMO CATEGORY                            
         MVI   8(R2),C'|'          DELIMITER                                    
         AHI   R2,9                                                             
         AHI   RF,8                                                             
         J     BCATNP10                                                         
*                                                                               
BCATNP20 SHI   R2,1                REMOVE LAST DELIMITER                        
         CLI   0(R2),C'|'                                                       
         JNE   *+8                                                              
         MVI   0(R2),0                                                          
         J     BLDDCATX                                                         
*                                                                               
* NET WRITER                                                                    
* ALWAYS LOOK UP RATINGS AND IMPRESSIONS (IN THAT ORDER)                        
*                                                                               
BCATNR00 L     R3,ADBXNWRI         A(DBEXTEND - NWRI EXTENSION)                 
         USING DBCNWD,R3                                                        
*                                                                               
         LA    RE,8                                                             
         L     RF,DBCNWDNL         A(DEMO NAME LIST)                            
*                                                                               
BCATNR10 CLI   0(RF),0                                                          
         JE    BCATNR20                                                         
*                                                                               
         MVC   DUB,0(RF)                                                        
         CLI   0(RF),C'X'          REMOVE ANY MODIFIER                          
         JE    *+10                ALWAYS LOOK UP IMPS & RTGS                   
         MVC   DUB(7),1(RF)                                                     
*                                                                               
         TM    DBCNWFLG,DBCNWUQ    GET UNIVERSES?                               
         JZ    *+12                                                             
         MVI   0(R2),C'U'                                                       
         J     BCATNR15                                                         
*                                                                               
         MVI   0(R2),C'R'                                                       
         MVC   1(7,R2),DUB                                                      
         MVI   8(R2),C'|'          DELIMITER                                    
         AHI   R2,9                                                             
         MVI   0(R2),C'I'          RATING                                       
BCATNR15 MVC   1(7,R2),DUB                                                      
         MVI   8(R2),C'|'          DELIMITER                                    
         AHI   R2,9                                                             
         AHI   RF,8                                                             
         J     BCATNR10                                                         
*                                                                               
BCATNR20 SHI   R2,1                REMOVE LAST DELIMITER                        
         CLI   0(R2),C'|'                                                       
         JNE   *+8                                                              
         MVI   0(R2),0                                                          
         J     BLDDCATX                                                         
*                                                                               
BLDDCATX J     EXIT                                                             
         DROP  R3,RF                                                            
         LTORG                                                                  
***********************************************************************         
* BUILD HASHKEY FROM DBLOCK                                                     
*   ON EXIT: FIELD 'HASHKEY' IS SET                                             
***********************************************************************         
BLDHASH  NTR1  BASE=*,LABEL=*                                                   
         CLI   SYSTEM,SPOTQ        SPOT?                                        
         JE    BHASHS00                                                         
         CLI   SYSTEM,NETQ         NET?                                         
         JE    BHASHN00                                                         
         J     BLDHASHX                                                         
*                                                                               
* SPOT                                                                          
*                                                                               
BHASHS00 XC    HASHKEY,HASHKEY                                                  
*                                                                               
         L     R2,ADBLOCK                                                       
         USING DBLOCKD,R2                                                       
         LA    R3,HASHKEY                                                       
         USING HASHKEYD,R3                                                      
         L     R4,ADBXCLOC         A(DBEXTEND - CLOC EXTENSION)                 
         USING DBCMINTD,R4                                                      
*                                                                               
         MVC   HASHSTA(4),DBSELSTA STATION                                      
         CLI   DBSELSTA+3,C'+'     PARENT+?                                     
         JE    BHASHS02                                                         
         CLI   DBSELSTA+4,C'+'     PARENT+                                      
         JNE   *+10                                                             
         MVC   HASHSTA,DBSELSTA    STATION                                      
*                                                                               
BHASHS02 OC    DBCMMKT,DBCMMKT                                                  
         JZ    BHASHS10                                                         
         EDIT  DBCMMKT,HASHMKT,ALIGN=LEFT      MARKET                           
*                                                                               
BHASHS10 OC    DBSELSYC,DBSELSYC                                                
         JZ    BHASHS12                                                         
         EDIT  DBSELSYC,HASHSYS,ALIGN=LEFT     SYSCODE                          
*                                                                               
BHASHS12 BRAS  RE,CALCSYBK         CALC START/END DATES & SYSCODE BOOK          
         MVC   HASHSYBK,SVSYBK     BOOK                                         
         MVC   HASHSDTE,SVSDTE     START DATE                                   
         MVC   HASHEDTE,SVEDTE     END DATE                                     
*                                                                               
         GOTOR SETROT,DMCB,DBSELDAY,HASHROT    SET ROTATION                     
*                                                       START/END TIME          
         EDIT  (2,DBSELTIM),(4,HASHSTIM),ALIGN=RIGHT,FILL=0                     
         MVC   HASHETIM,HASHSTIM                                                
         OC    DBSELTIM+2(2),DBSELTIM+2                                         
         JZ    BHASHS14                                                         
         EDIT  (2,DBSELTIM+2),(4,HASHETIM),ALIGN=RIGHT,FILL=0                   
*                                                                               
BHASHS14 CLC   =C'2400',HASHSTIM                                                
         JNE   *+10                                                             
         MVC   HASHSTIM,=C'0000'                                                
         CLC   =C'2400',HASHETIM                                                
         JNE   *+10                                                             
         MVC   HASHETIM,=C'0000'                                                
*                                                                               
         GOTO1 VDATCON,DMCB,(2,DBCMSPDT),(23,HASHSDAT)  SPOT DATE               
         MVI   HASHSTA+L'HASHSTA,C'|'                                           
         MVI   HASHMKT+L'HASHMKT,C'|'                                           
         MVI   HASHSYS+L'HASHSYS,C'|'                                           
         MVI   HASHSDTE+L'HASHSDTE,C'|'                                         
         MVI   HASHEDTE+L'HASHEDTE,C'|'                                         
         MVI   HASHSYBK+L'HASHSYBK,C'|'                                         
         MVI   HASHROT+L'HASHROT,C'|'                                           
         MVI   HASHSTIM+L'HASHSTIM,C'|'                                         
         MVI   HASHETIM+L'HASHETIM,C'|'                                         
         DROP  R2,R3,R4                                                         
*                                                                               
         OC    HASHKEY,=CL100' '                                                
         J     BLDHASHX                                                         
*                                                                               
* NET                                                                           
*                                                                               
BHASHN00 CLI   SVCALLER,SVCNWRQ    NET WRITER?                                  
         JE    BHNWR00                                                          
         J     BLDHASHX                                                         
*                                                                               
BHNWR00  XC    BHNWKEY,BHNWKEY                                                  
*                                                                               
         L     R2,ADBLOCK                                                       
         USING DBLOCKD,R2                                                       
         LA    R3,BHNWKEY                                                       
         USING HNWKEYD,R3                                                       
         L     R4,ADBXNWRI         A(DBEXTEND - NWRI EXTENSION)                 
         USING DBCNWD,R4                                                        
*                                                                               
         MVC   HNWNET#,DBCNWNET    COMSCORE NETWORK #                           
         MVC   HNWSER#,DBCNWSN     COMSCORE SERIES #                            
*                                  START/END TIME                               
         EDIT  (2,DBCNWST),(4,HNWSTIM),ALIGN=RIGHT,FILL=0                       
         MVC   HNWETIM,HNWSTIM                                                  
         OC    DBCNWET,DBCNWET     ANY END TIME?                                
         JZ    BHNWR10                                                          
*                                                                               
         CLC   DBCNWET,=H'2400'    AFTER MIDNIGHT?                              
         BL    BHNWR05                                                          
         SR    RF,RF               YES                                          
         ICM   RF,3,DBCNWET                                                     
         SH    RF,=H'2400'                                                      
         STCM  RF,3,DBCNWET                                                     
*                                                                               
BHNWR05  EDIT  (2,DBCNWET),(4,HNWETIM),ALIGN=RIGHT,FILL=0                       
*                                                                               
BHNWR10  CLC   =C'2400',HNWSTIM                                                 
         JNE   *+10                                                             
         MVC   HNWSTIM,=C'0000'                                                 
         CLC   =C'2400',HNWETIM                                                 
         JNE   *+10                                                             
         MVC   HNWETIM,=C'0000'                                                 
*                                                                               
         OC    DBCNWSD,DBCNWSD                                                  
         JZ    BHNWR20                                                          
         GOTO1 VDATCON,DMCB,(2,DBCNWSD),(23,HNWSDTE)  START DATE                
         OC    DBCNWED,DBCNWED                                                  
         JZ    BHNWR20                                                          
         GOTO1 VDATCON,DMCB,(2,DBCNWED),(23,HNWEDTE)  END DATE                  
*                                                                               
BHNWR20  MVC   HNWVTYPE,VTYPE      VIEWING TYPE                                 
         GOTOR SETROT,DMCB,DBCNWROT,HNWROT     SET ROTATION                     
*                                                                               
         MVI   HNWSDTE-1,C'|'                                                   
         MVI   HNWEDTE-1,C'|'                                                   
         MVI   HNWSTIM-1,C'|'                                                   
         MVI   HNWETIM-1,C'|'                                                   
         MVI   HNWVTYPE-1,C'|'                                                  
         MVI   HNWROT-1,C'|'                                                    
         MVI   HNWSER#-1,C'|'                                                   
         BRAS  RE,SQSHBHNW         REMOVE NULLS & SPACES                        
*                                                                               
         BRAS  RE,BLDDCAT          BUILD DEMO CATEGORY LIST                     
         BRAS  RE,SQSHDEMO         REMOVE NULLS & SPACES                        
*                                                                               
         DROP  R2,R3,R4                                                         
*                                                                               
BLDHASHX J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* CALCULATE START/END DATES & SYSCODE BOOK                                      
***********************************************************************         
CALCSYBK NTR1  BASE=*,LABEL=*                                                   
         XC    SVSYBK,SVSYBK                                                    
         XC    SVSDTE,SVSDTE                                                    
         XC    SVEDTE,SVEDTE                                                    
*                                                                               
         L     R2,ADBLOCK                                                       
         USING DBLOCKD,R2                                                       
         L     R3,ADBXCLOC         A(DBEXTEND - CLOC EXTENSION)                 
         USING DBCMINTD,R4                                                      
*                                                                               
         OC    DBCMSTDT,DBCMSTDT                                                
         JZ    CSYBK10                                                          
         GOTO1 VDATCON,DMCB,(3,DBCMSTDT),(23,SVSDTE)   START DATE               
CSYBK10  OC    DBCMENDD,DBCMENDD                                                
         JZ    CSYBK20                                                          
         GOTO1 VDATCON,DMCB,(3,DBCMENDD),(23,SVEDTE)   END DATE                 
*                                                                               
CSYBK20  OC    DBCMFRBK,DBCMFRBK   FORCE BOOK PASSED?                           
         JZ    CSYBK40                                                          
         XC    FULL,FULL                                                        
         MVC   FULL(2),DBCMFRBK                                                 
         MVI   FULL+2,X'01'                                                     
         GOTO1 VDATCON,DMCB,(3,FULL),(6,SVSYBK)   SYSCODE BOOK                  
*                                                                               
         CLI   DBCMDATE,DBCMBRD    GET START/END VIA BROADCAST?                 
         JNE   CSYBK30                                                          
         GOTO1 VDATCON,DMCB,(3,FULL),(0,WORK)                                   
         GOTO1 VGETBROD,DMCB,(1,WORK),WORK+6,VGETDAY,VADDAY                     
         CLI   DMCB,X'FF'                                                       
         JE    CSYBKYES                                                         
         GOTO1 VDATCON,DMCB,(0,WORK+6),(23,SVSDTE)    START DATE                
         GOTO1 VDATCON,DMCB,(0,WORK+12),(23,SVEDTE)   END DATE                  
         J     CSYBKYES                                                         
*                                                                               
CSYBK30  CLI   DBCMDATE,DBCMSWP    GET START/END VIA SWEEP?                     
         JNE   CSYBKYES                                                         
         GOTO1 VDATCON,DMCB,(3,FULL),(0,WORK)     START DATE                    
*                                                                               
* LOOP THROUGH SWEETBL IN DEMTABS TO FIND FORCE BOOK                            
*                                                                               
         GOTO1 VDEMTABS,DMCB,SWEEPTBL                                           
         L     R3,0(R1)                                                         
         JZ    *+2                                                              
         L     RF,4(R1)            L'ENTRY                                      
         STC   RF,BYTE                                                          
         USING SWPTABD,R3                                                       
*                                                                               
CSYBK31  CLC   SWPTTYPE,=C'TTN'    TEST END OF TABLE                            
         JNE   CSYBKNO                                                          
         CLC   FULL(2),SWPTYR      SAME YEAR/MONTH?                             
         JE    CSYBK32                                                          
*                                                                               
         LLC   RF,BYTE             GET NEXT TABLE ENTRY                         
         AR    R3,RF                                                            
         J     CSYBK31                                                          
*                                                                               
CSYBK32  GOTO1 VDATCON,DMCB,(10,SWPTSDTE),(23,SVSDTE) START DATE                
         GOTO1 VDATCON,DMCB,(10,SWPTSDTE),(0,WORK)    START DATE                
                                                                                
         CLI   SWPTNUM,X'01'       1 WEEK?                                      
         JNE   CSYBK33                                                          
         GOTOR VADDAY,DMCB,WORK,WORK+8,F'6'                                     
CSYBK33  CLI   SWPTNUM,X'02'       2 WEEK?                                      
         JNE   CSYBK34                                                          
         GOTOR VADDAY,DMCB,WORK,WORK+8,F'13'                                    
CSYBK34  CLI   SWPTNUM,X'03'       3 WEEK?                                      
         JNE   CSYBK35                                                          
         GOTOR VADDAY,DMCB,WORK,WORK+8,F'20'                                    
CSYBK35  CLI   SWPTNUM,X'04'       4 WEEK?                                      
         JNE   CSYBK36                                                          
         GOTOR VADDAY,DMCB,WORK,WORK+8,F'27'                                    
                                                                                
CSYBK36  GOTO1 VDATCON,DMCB,(0,WORK+8),(23,SVEDTE)    END DATE                  
         J     CSYBKYES                                                         
         DROP  R3                                                               
*                                                                               
CSYBK40  OC    DBCMSPDT,DBCMSPDT   SPOT DATE PASSED?                            
         JNZ   CSYBK50             YES = AFFID, NO = ACHIEVED                   
*                                                                               
         GOTO1 VDATCON,DMCB,(3,DBCMSTDT),(0,WORK)                               
         GOTO1 VGETBROD,DMCB,(1,WORK),WORK+6,VGETDAY,VADDAY                     
         CLI   DMCB,X'FF'          SYSCODE BK = BRD MON OF START DATE           
         JE    CSYBKYES                                                         
         GOTO1 VDATCON,DMCB,(0,WORK+12),(6,SVSYBK)   SYSCODE BOOK               
         J     CSYBKYES                                                         
*                                                                               
CSYBK50  GOTO1 VDATCON,DMCB,(2,DBCMSPDT),(0,WORK)   AFFID POSTING               
         GOTO1 VGETBROD,DMCB,(1,WORK),WORK+6,VGETDAY,VADDAY                     
         CLI   DMCB,X'FF'          SYSCODE BK = BRD MON OF SPOT DATE            
         JE    CSYBKYES                                                         
         GOTO1 VDATCON,DMCB,(0,WORK+12),(6,SVSYBK)   SYSCODE BOOK               
*                                                                               
* MAKE SURE START AND END DATES ARE WITHIN THE REQUESTED START AND              
* END DATE RANGE                                                                
CSYBKYES OC    DBCMREQS,DBCMREQS                                                
         JZ    YES                                                              
         OC    DBCMSPDT,DBCMSPDT    ONLY ADJUST START AND END DATE              
         JNZ   YES                  FOR ACHIEVED LOOKUPS                        
         GOTO1 VDATCON,DMCB,(2,DBCMREQS),(23,REPSDATE)                          
         GOTO1 VDATCON,DMCB,(2,DBCMREQE),(23,REPEDATE)                          
*                                                                               
         OC    DBCMFRBK,DBCMFRBK   if we have a forced book                     
         JNZ   YES                 leave the dates alone                        
*                                                                               
         CLC   REPSDATE,SVSDTE                                                  
         BNH   *+10                                                             
         MVC   SVSDTE,REPSDATE                                                  
         CLC   REPEDATE,SVEDTE                                                  
         BNL   *+10                                                             
         MVC   SVEDTE,REPEDATE                                                  
*                                                                               
         J     YES                                                              
CSYBKNO  J     NO                                                               
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* PROCGET FOR NET                                                               
***********************************************************************         
PGETN    NTR1  BASE=*,LABEL=*                                                   
         CLI   SVCALLER,SVCNNPQ    NPOD?                                        
         JE    PGETNP00                                                         
         CLI   SVCALLER,SVCNSDQ    SEED?                                        
         JE    PGETND00                                                         
         CLI   SVCALLER,SVCNWRQ    WRITER?                                      
         JE    PGETNR00                                                         
         J     NO                                                               
*                                                                               
* NET RESEARCH WRITER (NPOD)                                                    
*                                                                               
PGETNP00 BRAS  RE,BLDDCAT          BUILD DEMO CATEGORY LIST                     
*                                                                               
         SAM31                                                                  
         LA    R4,BPARAMS                                                       
         USING BSPARA,R4                                                        
*                                                                               
         XC    NETBKEY,NETBKEY     BINSRCH KEY FOR NET RESEARCH WRITER          
         LA    R2,NETBKEY                                                       
         USING BRNRECD,R2                                                       
         L     R3,ADBXCSRN         A(DBEXTEND - CSRN EXTENSION)                 
         USING DBCSRND,R3                                                       
*                                                                               
         ST    R2,BSPAREC          A(RECORD)                                    
         MVC   BSPSTRT,ABINTAB     A(BINSRCH TABLE)                             
*                                                                               
         LA    RF,BRNRECLN                                                      
         ST    RF,BSPLENR          L'RECORD                                     
*                                                                               
         LA    RF,BRNKEYLN                                                      
         ST    RF,BSPLENK          L'KEY                                        
         MVI   BSPKEYD,0           DISPLACEMENT OF KEY                          
*                                                                               
         L     RF,ABINTAB                                                       
         SHI   RF,4                                                             
         MVC   BSPEND,0(RF)        MAX NUMBER OF RECORDS                        
         MVC   BSPNOR,0(RF)        NUMBER OF RECORDS IN TABLE                   
*                                                                               
         TM    DBCSMODE,DBCSMCOQ   GET COMSCORE ONLY NETWORKS?                  
         JZ    PGETNP10                                                         
         OI    BSPLENR,X'02'       READ HIGH                                    
*                                                                               
* GET FIRST UNPROCESSED COMSCORE ONLY NETWORK ENTRY IN BINSRCH TABLE            
*                                                                               
PGETNP05 GOTO1 VBINSRCH,BPARAMS                                                 
         OC    BSPADD,BSPADD                                                    
         JZ    NO                                                               
*                                                                               
         L     R2,BSPAREC          A(RECORD)                                    
         USING BRNRECD,R2                                                       
         OC    0(BRNKEYLN,R2),0(R2)   FOUND ANYTHING?                           
         JZ    NO                                                               
         CLC   BRNSTAT,=C'FAIL'    TEST IF REQUEST PASSED                       
         JE    *+12                                                             
         TM    BRNFLG,BRNPROCQ     ALREADY PROCESSED?                           
         JZ    PGETNP15                                                         
*                                                                               
         MVC   NETBKEY,0(R2)       GET NEXT ENTRY IN BINSRCH TABLE              
         LA    R2,NETBKEY                                                       
         MVI   BRNFLG,X'FF'        FORCE RDHI TO GET NEXT RECORD                
         ST    R2,BSPAREC                                                       
         J     PGETNP05                                                         
*                                                                               
PGETNP10 MVC   BRNSTA,DBCSRNET     NETWORK                                      
         MVC   BRNPROG,DBCSRPN     PROGRAM NAME                                 
         GOTO1 VDATCON,DMCB,(3,DBCSRPRD),(23,BRNDATE) PROGRAM RUN DATE          
         GOTOR SETROT,DMCB,DBCSRDAY,BRNROT     SET ROTATION                     
         MVC   BRNVTYPE,DBCSRVT    VIEWING TYPE                                 
*                                                       START/END TIME          
         EDIT  (2,DBCSRST),(4,BRNSTIM),ALIGN=RIGHT,FILL=0                       
         EDIT  (2,DBCSRET),(4,BRNETIM),ALIGN=RIGHT,FILL=0                       
*                                                                               
         CLC   =C'2400',BRNSTIM                                                 
         JNE   *+10                                                             
         MVC   BRNSTIM,=C'0000'                                                 
         CLC   =C'2400',BRNETIM                                                 
         JNE   *+10                                                             
         MVC   BRNETIM,=C'0000'                                                 
*                                                                               
         GOTO1 VBINSRCH,BPARAMS                                                 
         TM    BSPADD,X'80'        RECORD NOT FOUND?                            
         JO    NO                                                               
*                                                                               
PGETNP15 L     R2,BSPAREC          A(RECORD)                                    
         USING BRNRECD,R2                                                       
         MVC   DBCSRAB,BSPAREC     A(RECORD)                                    
         OI    BRNFLG,BRNPROCQ     MARK IT PROCESSED                            
         DROP  R4                                                               
*                                                                               
* LOOP THROUGH REQUESTED DEMOS IN DBLOCK                                        
* THEN FIND IT IN THE BINSRCH RECORD                                            
* THEN FIND AND RETURN THE CORRESPONDING DEMO VALUE                             
*                                                                               
PGETNP20 CLC   BRNSTAT,=C'FAIL'    TEST IF REQUEST PASSED                       
         JE    NO                                                               
         SAM24                                                                  
*                                                                               
         L     R3,ADBXCSRN         A(DBEXTEND - CSRN EXTENSION)                 
         USING DBCSRND,R3                                                       
         L     R4,DBCSRRDL         A(REQUESTED DEMO LIST) - HEX                 
         MVI   SVDEMOFF,0          DEMO OFFSET IN REQ DEMO LIST                 
*                                                                               
PGETNP30 CLI   0(R4),X'FF'         ANY MORE DEMOS?                              
         JE    PGETNX                                                           
         OC    1(2,R4),1(R4)       COMSCORE DEMO?                               
         JNZ   PGETNP60            GET NEXT DEMO FROM DBLOCK                    
*                                                                               
         ZIC   RE,3(R4)            THE N'TH DEMO IN THE LIST                    
         SHI   RE,1                                                             
         MH    RE,=H'8'            DISPLACEMENT INTO BUY DEMO LIST              
*                                                                               
         L     R3,DBCSRDNL         A(COMSCORE DEMOS NAMES)                      
         AR    R3,RE                                                            
         MVC   DUB,0(R3)           GET ALPHA DEMO CATEGORY                      
*                                                                               
* FOUND DEMO IN DBLOCK, NOW FIND IT IN BINSRCH RECORD                           
*                                                                               
         SAM31                                                                  
         LA    R2,BPARAMS                                                       
         USING BSPARA,R2                                                        
         L     R2,BSPAREC          A(BINSRCH RECORD)                            
         USING BRNRECD,R2                                                       
         LA    R2,BRNCATS                                                       
         LA    R3,BRNCATLN                                                      
         MVI   BYTE,0              DEMO INDEX IN BINSRCH RECORD                 
*                                                                               
PGETNP40 GOTOR BLDFLD,DMCB,(R2),C'|',EBCDICQ  BLD FLD HDR                       
         JNE   PGETNP60            GET NEXT DEMO FROM DBLOCK                    
*                                                                               
         OC    FLD,=CL8' '                                                      
         CLC   DUB,FLD             FOUND DEMO MATCH?                            
         JNE   PGETNP50                                                         
*                                                                               
* FOUND DEMO IN BINSRCH TABLE, NOW FIND CORRESONDING DEMO VALUE                 
*                                                                               
         LA    R2,BPARAMS                                                       
         USING BSPARA,R2                                                        
         L     R2,BSPAREC          A(BINSRCH RECORD)                            
         USING BRNRECD,R2                                                       
*                                                                               
         LA    R2,BRNVALS          FIND DEMO VALUE IN BINSRCH LIST              
*                                                                               
         ZIC   RE,BYTE             INDEX INTO BINSRCH DEMO LIST                 
         MH    RE,=H'4'            DISPLACEMENT INTO BINSRCH DEMO VALS          
         AR    R2,RE                                                            
         MVC   FULL,0(R2)          EXTRACTED DEMO VALUE                         
         SAM24                                                                  
*                                                                               
* FOUND CORRESPONDING DEMO VALUE, NOW RETURN IT IN DBLOCK                       
*                                                                               
         L     R3,ADBXCSRN         A(DBEXTEND - CSRN EXTENSION)                 
         USING DBCSRND,R3                                                       
         L     R5,DBCSRODV         A(OUTPUT DEMO VALUES)                        
*                                                                               
         ZIC   RE,SVDEMOFF         OFFSET INTO REQ DEMO LIST                    
         MH    RE,=H'4'            DISPLACEMENT INTO BUY DEMO LIST              
         AR    R5,RE                                                            
         MVC   0(4,R5),FULL        RETURN DEMO VALUE                            
         J     PGETNP60            GET NEXT DEMO FROM DBLOCK                    
*                                                                               
PGETNP50 ZIC   RF,BYTE             GET NEXT DEMO IN BINSRCH LIST                
         AHI   RF,1                                                             
         STC   RF,BYTE                                                          
         L     R2,ANXTFLD                                                       
         BCT   R3,PGETNP40                                                      
*                                                                               
PGETNP60 AHI   R4,4                GET NEXT DEMO FROM DBLOCK                    
         ZIC   RF,SVDEMOFF         DEMO OFFSET IN REQ DEMO LIST                 
         AHI   RF,1                                                             
         STC   RF,SVDEMOFF                                                      
         J     PGETNP30                                                         
         DROP  R2,R3                                                            
*                                                                               
* NET SEED REPORT                                                               
*                                                                               
PGETND00 SAM31                                                                  
         LA    R4,BPARAMS                                                       
         USING BSPARA,R4                                                        
*                                                                               
         XC    BHNDKEY,BHNDKEY     BINSRCH KEY FOR SEED                         
         LA    R2,BHNDKEY                                                       
         USING HNDKEYD,R2                                                       
         L     R3,ADBXNSED         A(DBEXTEND - SEED EXTENSION)                 
         USING DBSEEDD,R3                                                       
*                                                                               
         L     RF,DBSEAPGT         A(OUTPUT PROGRAM NAMES)                      
         MVI   0(RF),X'FF'                                                      
*                                                                               
* BUILD HASHKEY FOR BINSRCH TABLE LOOKUP                                        
*                                                                               
         MVC   HNDNNUM,DBSENETN    NETWORK NUMBER                               
*                                  START/END TIME                               
         EDIT  (2,DBSERSTM),(4,HNDSTIM),ALIGN=RIGHT,FILL=0                      
         MVC   HNDETIM,HNDSTIM                                                  
         OC    DBSERETM,DBSERETM   ANY END TIME?                                
         JZ    PPUTND10                                                         
*                                                                               
         CLC   DBSERETM,=H'2400'   AFTER MIDNIGHT?                              
         BL    PGETND05                                                         
         SR    RF,RF               YES                                          
         ICM   RF,3,DBSERETM                                                    
         SH    RF,=H'2400'                                                      
         STCM  RF,3,DBSERETM                                                    
*                                                                               
PGETND05 EDIT  (2,DBSERETM),(4,HNDETIM),ALIGN=RIGHT,FILL=0                      
*                                                                               
PGETND10 CLC   =C'2400',HNDSTIM                                                 
         JNE   *+10                                                             
         MVC   HNDSTIM,=C'0000'                                                 
         CLC   =C'2400',HNDETIM                                                 
         JNE   *+10                                                             
         MVC   HNDETIM,=C'0000'                                                 
*                                                                               
         OC    DBSERSDT,DBSERSDT                                                
         JZ    PGETND15                                                         
         GOTO1 VDATCON,DMCB,(2,DBSERSDT),(23,HNDSDAT)  START DATE               
         OC    DBSEREDT,DBSEREDT                                                
         JZ    PGETND15                                                         
         GOTO1 VDATCON,DMCB,(2,DBSEREDT),(23,HNDEDAT)  END DATE                 
*                                                                               
PGETND15 MVI   HNDSDAT-1,C'|'                                                   
         MVI   HNDEDAT-1,C'|'                                                   
         MVI   HNDSTIM-1,C'|'                                                   
         MVI   HNDETIM-1,C'|'                                                   
         BRAS  RE,SQSHBHND         REMOVE NULLS & SPACES                        
*                                                                               
PGETND20 LA    R2,BHNDKEY                                                       
         ST    R2,BSPAREC          A(RECORD)                                    
         MVC   BSPSTRT,ABINTAB     A(BINSRCH TABLE)                             
*                                                                               
         LA    RF,BNDRECLN                                                      
         ST    RF,BSPLENR          L'RECORD                                     
*                                                                               
         LA    RF,BNDKEYLN                                                      
         ST    RF,BSPLENK          L'KEY                                        
         MVI   BSPKEYD,0           DISPLACEMENT OF KEY                          
*                                                                               
         L     RF,ABINTAB                                                       
         SHI   RF,4                                                             
         MVC   BSPEND,0(RF)        MAX NUMBER OF RECORDS                        
         MVC   BSPNOR,0(RF)        NUMBER OF RECORDS IN TABLE                   
         OI    BSPLENR,X'02'       READ HIGH                                    
*                                                                               
PGETND30 GOTO1 VBINSRCH,BPARAMS                                                 
         OC    BSPADD,BSPADD                                                    
         JZ    PGETND50                                                         
*                                                                               
         L     R2,BSPAREC          A(RECORD)                                    
         USING BNDRECD,R2                                                       
*                                                                               
         CLC   BHNDKEY,0(R2)       FOUND EXACT MATCH?                           
         JNE   PGETND50            NO - DONE                                    
*                                                                               
         L     RF,DBSEAPGT         A(OUTPUT PROGRAM NAMES)                      
PGETND32 CLI   0(RF),X'FF'         GET NEXT AVAILABLE POSITION                  
         JE    *+12                                                             
         AHI   RF,DBSEPLNQ                                                      
         J     PGETND32                                                         
*                                                                               
         MVC   0(DBSEPLNQ,RF),BNDSNUM                                           
         AHI   RF,DBSEPLNQ                                                      
         MVI   0(RF),X'FF'                                                      
*                                                                               
         LA    R2,BHNDKEY          GET NEXT PROGRAM FOR THIS HASHKEY            
         ZIC   RF,BNDSEQ                                                        
         AHI   RF,1                                                             
         STC   RF,BNDSEQ                                                        
         J     PGETND20                                                         
*                                                                               
PGETND50 SAM24                                                                  
         L     R3,ADBXNSED         A(DBEXTEND - SEED EXTENSION)                 
         USING DBSEEDD,R3                                                       
*                                                                               
         L     RF,DBSEAPGT         A(OUTPUT PROGRAM NAMES)                      
         CLI   0(RF),X'FF'         FOUND ANYTHING?                              
         JE    NO                                                               
         J     PGETNX                                                           
         DROP  R2,R3,R4                                                         
*                                                                               
* NET WRITER                                                                    
*                                                                               
PGETNR00 BRAS  RE,BLDHASH          BUILD HASHKEY FROM DBLOCK                    
         MVC   PHNWHKEY,BHNWKEY                                                 
         XC    PHNWHSEQ,PHNWHSEQ                                                
*                                                                               
PGETNR10 SAM31                                                                  
         LA    R4,BPARAMS                                                       
         USING BSPARA,R4                                                        
*                                                                               
         LA    RF,PHNWHKEY                                                      
         ST    RF,BSPAREC          A(RECORD)                                    
*                                                                               
         MVC   BSPSTRT,ABINTAB     A(BINSRCH TABLE)                             
*                                                                               
         LA    RF,BNWRECLN                                                      
         ST    RF,BSPLENR          L'RECORD                                     
*                                                                               
         LA    RF,BNWKEYLN                                                      
         ST    RF,BSPLENK          L'KEY                                        
         MVI   BSPKEYD,0           DISPLACEMENT OF KEY                          
*                                                                               
         L     RF,ABINTAB                                                       
         SHI   RF,4                                                             
         MVC   BSPEND,0(RF)        MAX NUMBER OF RECORDS                        
         MVC   BSPNOR,0(RF)        NUMBER OF RECORDS IN TABLE                   
*                                                                               
         GOTO1 VBINSRCH,BPARAMS                                                 
         TM    BSPADD,X'80'        RECORD NOT FOUND?                            
         JO    NO                                                               
*                                                                               
         L     R2,BSPAREC          A(RECORD)                                    
         USING BNWRECD,R2                                                       
         CLC   BNWCATS,DEMOCATS    SAME DEMO LIST?                              
         JE    PGETNR20                                                         
*                                                                               
         SR    RF,RF               FOUND HASHKEY BUT NOT DEMO LIST              
         ICM   RF,15,PHNWHSEQ      SO TRY NEXT SEQUENCE NUMBER                  
         AHI   RF,1                                                             
         STCM  RF,15,PHNWHSEQ                                                   
         J     PGETNR10                                                         
         DROP  R4                                                               
*                                                                               
* LOOP THROUGH REQUESTED DEMOS IN DBLOCK                                        
* THEN FIND IT IN THE BINSRCH RECORD                                            
* THEN FIND AND RETURN THE CORRESPONDING DEMO VALUE                             
*                                                                               
PGETNR20 CLC   BNWSTAT,=C'FAIL'    TEST IF REQUEST PASSED                       
         JE    NO                                                               
         SAM24                                                                  
*                                                                               
         L     R3,ADBXNWRI         A(DBEXTEND - NWRI EXTENSION)                 
         USING DBCNWD,R3                                                        
         L     R4,DBCNWRDL         A(REQUESTED DEMO LIST) - HEX                 
         MVI   SVDEMOFF,0          DEMO OFFSET IN REQ DEMO LIST                 
*                                                                               
PGETNR30 CLI   0(R4),X'FF'         ANY MORE DEMOS?                              
         JE    PGETNX                                                           
         CLI   2(R4),0             COMSCORE DEMO?                               
         JNE   PGETNR60            GET NEXT DEMO FROM DBLOCK                    
*                                                                               
         ZIC   RE,1(R4)            THE N'TH DEMO IN THE LIST                    
         SHI   RE,1                                                             
         MH    RE,=H'8'            DISPLACEMENT INTO BUY DEMO LIST              
*                                                                               
         L     R3,DBCNWDNL         A(COMSCORE DEMOS NAMES)                      
         AR    R3,RE                                                            
*                                                                               
         MVC   DUB,0(R3)           GET ALPHA DEMO CATEGORY                      
         CLI   0(R3),C'X'          REMOVE ANY MODIFIER                          
         JE    *+10                                                             
         MVC   DUB(7),1(R3)                                                     
*                                                                               
* FOUND DEMO IN DBLOCK, NOW FIND IT IN BINSRCH RECORD                           
*                                                                               
         SAM31                                                                  
         LA    R2,BPARAMS                                                       
         USING BSPARA,R2                                                        
         L     R2,BSPAREC          A(BINSRCH RECORD)                            
         USING BNWRECD,R2                                                       
         LA    R2,BNWCATS                                                       
         LA    R3,SPTDMAXQ                                                      
         MVI   BYTE,0              DEMO INDEX IN BINSRCH RECORD                 
*                                                                               
* THE RETURN BLOCK FOR EACH DEMO CATEGORY IS 12 BYTES.                          
* BYTES 0-3:  VPH VALUES                                                        
* BYTES 4-7:  IMPRESSION VALUES                                                 
* BYTES 8-11: RATING VALUES                                                     
*                                                                               
* THERE ARE TWO TYPES OF LOOKUPS - ONE FOR ACTUALS & ONE FOR ESTIMATES          
*                                                                               
* FOR ACTUALS:                                                                  
*   DEMO CATEGORIES ARE IN PAIRS (RTGS AND IMPRESSIONS) IN BNWCATS              
*   EX. RXM2554|IXM2554.  WE JUST NEED TO MATCH ON ONE OF THEM.                 
*   CALLING BLDFLD TWICE TO USE THE IXM2554 SO BYTE HAS THE CORRECT             
*   VALUE.                                                                      
*                                                                               
* FOR ESTIMATES:                                                                
*   DEMO CATEGORIES IN BNWCATS ARE FOR UNIVERSES EX. UXA2554|UXM2554            
*   AFTER FINDING THE VALUE IN THE BINSRCH TABLE, WE WILL RETURN THE            
*   DEMO VALUE IN THE VPH PORTION OF THE BLOCK                                  
*                                                                               
PGETNR40 GOTOR BLDFLD,DMCB,(R2),C'|',EBCDICQ  BLD FLD HDR                       
         JNE   PGETNR60            GET NEXT DEMO FROM DBLOCK                    
*                                                                               
         CLI   FLD,C'U'            GET UNIVERSES?                               
         JE    PGETNR45            YES                                          
*                                                                               
         L     R2,ANXTFLD                                                       
         GOTOR BLDFLD,DMCB,(R2),C'|',EBCDICQ  BLD FLD HDR                       
         JNE   PGETNR60            GET NEXT DEMO FROM DBLOCK                    
*                                                                               
PGETNR45 OC    FLD(8),=CL8' '                                                   
         CLC   DUB(7),FLD+1        FOUND DEMO MATCH?                            
         JNE   PGETNR50                                                         
*                                                                               
* FOUND DEMO IN BINSRCH TABLE, NOW FIND CORRESONDING DEMO VALUE                 
*                                                                               
         LA    R2,BPARAMS                                                       
         USING BSPARA,R2                                                        
         L     R2,BSPAREC          A(BINSRCH RECORD)                            
         USING BNWRECD,R2                                                       
*                                                                               
         LA    R2,BNWVALS          FIND DEMO VALUE IN BINSRCH LIST              
*                                  DEMO VALUES ARE IN PAIRS FOR EACH            
*                                  DEMO CATEGORY (IMPS/RTGS)                    
         ZIC   RE,BYTE             INDEX INTO BINSRCH DEMO LIST                 
*                                                                               
* 4 BYTES PER DEMO CATEGORY FOR UNIVERSES                                       
* 8 BYTES PER DEMO CATEGORY FOR IMPS/RTGS                                       
*                                                                               
         CLI   FLD,C'U'            GET UNIVERSES?                               
         JNE   *+12                                                             
         MH    RE,=H'4'            DISPLACEMENT INTO BINSRCH DEMO VALS          
         J     *+8                                                              
         MH    RE,=H'8'            DISPLACEMENT INTO BINSRCH DEMO VALS          
*                                                                               
         AR    R2,RE                                                            
         MVC   DUB2,0(R2)          GET RTGS + IMPRESSIONS (8 BYTES)             
         SAM24                                                                  
*                                                                               
* FOUND CORRESPONDING DEMO VALUE, NOW RETURN IT IN DBLOCK                       
*                                                                               
         L     R3,ADBXNWRI         A(DBEXTEND - NWRI EXTENSION)                 
         USING DBCNWD,R3                                                        
         L     R5,DBCNWODV         A(OUTPUT DEMO VALUES)                        
*                                                                               
         ZIC   RE,SVDEMOFF         OFFSET INTO REQ DEMO LIST                    
         MH    RE,=H'12'           DISPLACEMENT INTO BUY DEMO LIST              
         AR    R5,RE                                                            
         TM    DBCNWFLG,DBCNWUQ    GET UNIVERSES?                               
         JZ    *+14                                                             
         MVC   0(4,R5),DUB2        RETURN UNIVERSE IN VPH PORTION               
         J     PGETNR60                                                         
         MVC   4(8,R5),DUB2        RETURN IMPS AND RTGS                         
*                                                                               
* IMPRESSIONS FROM COMSCORE IS IN HUNDREDS.  RETURN IT IN THOUSANDS.            
*                                                                               
         ICM   RF,15,DUB2+4        IMPRESSIONS                                  
         SR    RE,RE                                                            
         D     RE,=F'10'           CHANGE IT TO THOUSANDS PRECISION             
         STCM  RF,15,8(R5)         SET IMPRESSIONS                              
*                                                                               
         J     PGETNR60            GET NEXT DEMO FROM DBLOCK                    
*                                                                               
PGETNR50 ZIC   RF,BYTE             GET NEXT DEMO IN BINSRCH LIST                
         AHI   RF,1                                                             
         STC   RF,BYTE                                                          
         L     R2,ANXTFLD                                                       
         BCT   R3,PGETNR40                                                      
*                                                                               
PGETNR60 AHI   R4,3                GET NEXT DEMO FROM DBLOCK                    
         ZIC   RF,SVDEMOFF         DEMO OFFSET IN REQ DEMO LIST                 
         AHI   RF,1                                                             
         STC   RF,SVDEMOFF                                                      
         J     PGETNR30                                                         
         DROP  R2,R3                                                            
*                                                                               
PGETNX   SAM24                                                                  
         J     YES                                                              
         LTORG                                                                  
***********************************************************************         
* PROCGET FOR SPOT                                                              
***********************************************************************         
PGETS    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,BLDDCAT          BUILD DEMO CATEGORY LIST                     
*                                                                               
         BRAS  RE,BLDHASH          BUILD HASHKEY FROM DBLOCK                    
         MVC   PREVHKEY,HASHKEY                                                 
         MVI   PREVHSEQ,0                                                       
*                                                                               
PGETS10  SAM31                                                                  
         LA    R4,BPARAMS                                                       
         USING BSPARA,R4                                                        
*                                                                               
         LA    RF,PREVKEY                                                       
         ST    RF,BSPAREC          A(RECORD)                                    
*                                                                               
         MVC   BSPSTRT,ABINTAB     A(BINSRCH TABLE)                             
*                                                                               
         LA    RF,BINRECLN                                                      
         ST    RF,BSPLENR          L'RECORD                                     
*                                                                               
         LA    RF,BINKEYLN                                                      
         ST    RF,BSPLENK          L'KEY                                        
         MVI   BSPKEYD,0           DISPLACEMENT OF KEY                          
*                                                                               
         L     RF,ABINTAB                                                       
         SHI   RF,4                                                             
         MVC   BSPEND,0(RF)        MAX NUMBER OF RECORDS                        
         MVC   BSPNOR,0(RF)        NUMBER OF RECORDS IN TABLE                   
*                                                                               
         GOTO1 VBINSRCH,BPARAMS                                                 
         TM    BSPADD,X'80'        RECORD NOT FOUND?                            
         JO    NO                                                               
*                                                                               
         L     R2,BSPAREC          A(RECORD)                                    
         USING BINRECD,R2                                                       
         CLC   BINCATS,DEMOCATS    SAME DEMO LIST?                              
         JE    PGETS20                                                          
*                                                                               
         ZIC   RF,PREVHSEQ         FOUND HASHKEY BUT NOT DEMO LIST              
         AHI   RF,1                SO TRY NEXT SEQUENCE NUMBER                  
         STC   RF,PREVHSEQ                                                      
         J     PGETS10                                                          
         DROP  R4                                                               
*                                                                               
* LOOP THROUGH REQUESTED DEMOS IN DBLOCK                                        
* THEN FIND IT IN THE BINSRCH RECORD                                            
* THEN FIND AND RETURN THE CORRESPONDING DEMO VALUE                             
*                                                                               
PGETS20  CLC   BINSTAT,=C'FAIL'    TEST IF REQUEST PASSED                       
         JE    NO                                                               
         SAM24                                                                  
*                                                                               
         L     R3,ADBXCLOC         A(DBEXTEND - CLOC EXTENSION)                 
         USING DBCMINTD,R3                                                      
         L     R4,DBDEMOL          A(REQUESTED DEMO LIST) - HEX                 
         MVI   SVDEMOFF,0          DEMO OFFSET IN REQ DEMO LIST                 
*                                                                               
PGETS30  CLI   0(R4),X'FF'         ANY MORE DEMOS?                              
         JE    PGETSX                                                           
         CLI   2(R4),0             COMSCORE DEMO?                               
         JNE   PGETS60             GET NEXT DEMO FROM DBLOCK                    
*                                                                               
         ZIC   RE,1(R4)            THE N'TH DEMO IN THE LIST                    
         SHI   RE,1                                                             
         MH    RE,=H'9'            DISPLACEMENT INTO BUY DEMO LIST              
*                                                                               
         L     R3,DBCMAEST         A(COMSCORE DEMOS FROM BUY)                   
         USING NTDELEM,R3                                                       
         LA    R3,NTDDMONM                                                      
         AR    R3,RE                                                            
         MVC   DUB,0(R3)           GET ALPHA DEMO CATEGORY                      
         DROP  R3                                                               
*                                                                               
* FOUND DEMO IN DBLOCK, NOW FIND IT IN BINSRCH RECORD                           
*                                                                               
         SAM31                                                                  
         LA    R2,BPARAMS                                                       
         USING BSPARA,R2                                                        
         L     R2,BSPAREC          A(BINSRCH RECORD)                            
         USING BINRECD,R2                                                       
         LA    R2,BINCATS                                                       
         LA    R3,BINCATLN                                                      
         MVI   BYTE,0              DEMO INDEX IN BINSRCH RECORD                 
*                                                                               
PGETS40  GOTOR BLDFLD,DMCB,(R2),C'|',EBCDICQ   BLD FLD HDR                      
         JNE   PGETS60             GET NEXT DEMO FROM DBLOCK                    
*                                                                               
         OC    FLD,=CL8' '                                                      
         CLC   DUB,FLD             FOUND DEMO MATCH?                            
         JNE   PGETS50                                                          
*                                                                               
* FOUND DEMO IN BINSRCH TABLE, NOW FIND CORRESONDING DEMO VALUE                 
*                                                                               
         LA    R2,BPARAMS                                                       
         USING BSPARA,R2                                                        
         L     R2,BSPAREC          A(BINSRCH RECORD)                            
         USING BINRECD,R2                                                       
         MVC   SVPNAME,BINPNAM                                                  
*                                                                               
         LA    R2,BINVALS          FIND DEMO VALUE IN BINSRCH LIST              
*                                                                               
         ZIC   RE,BYTE             INDEX INTO BINSRCH DEMO LIST                 
         MH    RE,=H'4'            DISPLACEMENT INTO BINSRCH DEMO VALS          
         AR    R2,RE                                                            
         MVC   FULL,0(R2)          EXTRACTED DEMO VALUE                         
         SAM24                                                                  
*                                                                               
* FOUND CORRESPONDING DEMO VALUE, NOW RETURN IT IN DBLOCK                       
*                                                                               
         L     R3,ADBXCLOC         A(DBEXTEND - CLOC EXTENSION)                 
         USING DBCMINTD,R3                                                      
         MVC   DBCMPNAM,SVPNAME                                                 
         L     R5,DBDEMVAL                                                      
         DROP  R3                                                               
*                                                                               
         ZIC   RE,SVDEMOFF         OFFSET INTO REQ DEMO LIST                    
         MH    RE,=H'4'            DISPLACEMENT INTO BUY DEMO LIST              
         AR    R5,RE                                                            
         MVC   0(4,R5),FULL        RETURN DEMO VALUE                            
         J     PGETS60             GET NEXT DEMO FROM DBLOCK                    
*                                                                               
PGETS50  ZIC   RF,BYTE             GET NEXT DEMO IN BINSRCH LIST                
         AHI   RF,1                                                             
         STC   RF,BYTE                                                          
         L     R2,ANXTFLD                                                       
         BCT   R3,PGETS40                                                       
*                                                                               
PGETS60  AHI   R4,3                GET NEXT DEMO FROM DBLOCK                    
         ZIC   RF,SVDEMOFF         DEMO OFFSET IN REQ DEMO LIST                 
         AHI   RF,1                                                             
         STC   RF,SVDEMOFF                                                      
         J     PGETS30                                                          
*                                                                               
PGETSX   SAM24                                                                  
         J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
***********************************************************************         
* CLOSE DEMO REQUEST RECORD DATASET & PUT MQ MESSAGE                            
***********************************************************************         
PUTMQ    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,MQNAME),(X'E0',0),0                  
         CLI   DMCB+8,0            DID MQ OPEN FAIL?                            
         JNE   MQNO                                                             
*                                                                               
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),DSN,L'DSN,0                              
         CLI   DMCB+8,0            DID MQ PUT FAIL?                             
         JNE   MQNO                                                             
*                                                                               
         CLI   SYSTEM,SPOTQ        SPOT?                                        
         JE    PUTMQS00                                                         
         CLI   SYSTEM,NETQ         NET?                                         
         JE    PUTMQN00                                                         
         J     PUTMQX                                                           
*                                                                               
* SPOT                                                                          
*                                                                               
PUTMQS00 GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQSYSS,L'MQSYSS,0                        
         CLI   DMCB+8,0            DID MQ PUT FAIL?                             
         JNE   MQNO                                                             
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQREPS1,L'MQREPS1,0                      
         CLI   DMCB+8,0            DID MQ PUT FAIL?                             
         JNE   MQNO                                                             
         J     PUTMQX                                                           
*                                                                               
* NET                                                                           
*                                                                               
PUTMQN00 GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQSYSN,L'MQSYSN,0                        
         CLI   DMCB+8,0            DID MQ PUT FAIL?                             
         JNE   MQNO                                                             
*                                                                               
         CLI   SVCALLER,SVCNNPQ    NPOD?                                        
         JNE   PUTMQN02                                                         
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQREPNP,L'MQREPNP,0                      
         CLI   DMCB+8,0            DID MQ PUT FAIL?                             
         JNE   MQNO                                                             
         J     PUTMQX                                                           
*                                                                               
PUTMQN02 CLI   SVCALLER,SVCNEXQ    NET EXTRACT?                                 
         JNE   PUTMQN04                                                         
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQREPNE,L'MQREPNE,0                      
         CLI   DMCB+8,0            DID MQ PUT FAIL?                             
         JNE   MQNO                                                             
         J     PUTMQX                                                           
*                                                                               
PUTMQN04 CLI   SVCALLER,SVCNSDQ    NET SEED REPORT?                             
         JNE   PUTMQN06                                                         
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQREPND,L'MQREPND,0                      
         CLI   DMCB+8,0            DID MQ PUT FAIL?                             
         JNE   MQNO                                                             
         J     PUTMQX                                                           
*                                                                               
PUTMQN06 CLI   SVCALLER,SVCNWRQ    NET WRITER?                                  
         JNE   MQNO                                                             
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQREPNW,L'MQREPNW,0                      
         CLI   DMCB+8,0            DID MQ PUT FAIL?                             
         JNE   MQNO                                                             
         J     PUTMQX                                                           
*                                                                               
PUTMQX   GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0            DID MQ CLOSE FAIL?                           
         JNE   MQNO                                                             
*                                                                               
MQYES    SR    RC,RC                                                            
MQNO     LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
MQNAME   DC    CL16'comScoreREQ*****'                                           
MQSYSS   DC    CL15'SYSTEM=SPOT'                                                
MQREPS1  DC    CL15'STYLE=STANDARD'                                             
MQSYSN   DC    CL15'SYSTEM=NET'                                                 
MQREPND  DC    CL15'STYLE=SEED'                                                 
MQREPNE  DC    CL15'STYLE=EXTRACT'                                              
MQREPNP  DC    CL15'STYLE=NPOD'                                                 
MQREPNW  DC    CL15'STYLE=WRITER'                                               
         LTORG                                                                  
***********************************************************************         
* BUILD NET DEMO REQUEST & PUT TO DATASET                                       
***********************************************************************         
PPUTN    NTR1  BASE=*,LABEL=*                                                   
         CLI   SVCALLER,SVCNNPQ    NPOD?                                        
         JE    PPUTNP00                                                         
         CLI   SVCALLER,SVCNSDQ    NET SEED?                                    
         JE    PPUTND00                                                         
         CLI   SVCALLER,SVCNWRQ    NET WRITER?                                  
         JE    PPUTNR00                                                         
         J     NO                                                               
*                                                                               
* NET RESEARCH WRITER (NPOD)                                                    
*                                                                               
PPUTNP00 L     R2,ADBLOCK                                                       
         USING DBLOCKD,R2                                                       
         LA    R3,DSNREC                                                        
         USING RRQRECD,R3                                                       
         L     R4,ADBXCSRN         A(DBEXTEND - CSRN EXTENSION)                 
         USING DBCSRND,R4                                                       
*                                                                               
         BRAS  RE,BLDDCAT          BUILD DEMO CATEGORY LIST                     
*                                                                               
         MVI   RRQTYPE,RRQTYPEQ    LINE TYPE                                    
*                                                                               
         MVC   RRQSTA,DBCSRNET     NETWORK                                      
         MVC   RRQPREC,DBCSRPRE    PRECISION                                    
*                                                                               
         MVC   RRQDTYPE,=C'PAV'    DEFAULT PROGRAM AVERAGE                      
         TM    DBCSRFLG,DBCSRFTQ   TIME PERIOD?                                 
         JZ    *+10                                                             
         MVC   RRQDTYPE,=C'TP '                                                 
*                                                                               
         MVC   RRQVTYPE,DBCSRVT    VIEWING TYPE                                 
*                                                                               
         GOTOR SETROT,DMCB,DBCSRDAY,RRQROT     SET ROTATION                     
*                                                       START/END TIME          
         EDIT  (2,DBCSRST),(4,RRQSTIM),ALIGN=RIGHT,FILL=0                       
         MVC   RRQETIM,RRQSTIM                                                  
         OC    DBCSRET,DBCSRET     ANY END TIME?                                
         JZ    PPUTNP10                                                         
*                                                                               
         CLC   DBCSRET,=H'2400'    AFTER MIDNIGHT?                              
         BL    PPUTNP05                                                         
         SR    RF,RF               YES                                          
         ICM   RF,3,DBCSRET                                                     
         SH    RF,=H'2400'                                                      
         STCM  RF,3,DBCSRET                                                     
*                                                                               
PPUTNP05 EDIT  (2,DBCSRET),(4,RRQETIM),ALIGN=RIGHT,FILL=0                       
*                                                                               
PPUTNP10 CLC   =C'2400',RRQSTIM                                                 
         JNE   *+10                                                             
         MVC   RRQSTIM,=C'0000'                                                 
         CLC   =C'2400',RRQETIM                                                 
         JNE   *+10                                                             
         MVC   RRQETIM,=C'0000'                                                 
*                                                                               
         MVC   RRQSDTIM,=C'6:00A'  START OF DAY (DEFAULT 6AM)                   
*                                                                               
         OC    DBCSRSD,DBCSRSD                                                  
         JZ    PPUTNP20                                                         
         GOTO1 VDATCON,DMCB,(3,DBCSRSD),(23,RRQSDTE)  START DATE                
         OC    DBCSRED,DBCSRED                                                  
         JZ    PPUTNP20                                                         
         GOTO1 VDATCON,DMCB,(3,DBCSRED),(23,RRQEDTE)  END DATE                  
*                                                                               
PPUTNP20 MVC   RRQDCATS,DEMOCATS   DEMO CATEGORIES                              
         MVC   RRQGRPBY,=CL10'NONE'                                             
         TM    DBCSRFLG,DBCSRFEQ   GROUP BY EPISODE?                            
         JZ    *+10                                                             
         MVC   RRQGRPBY,=CL10'EPISODE'                                          
*                                                                               
         MVI   RRQSTA-1,C','                                                    
         MVI   RRQSDTE-1,C','                                                   
         MVI   RRQEDTE-1,C','                                                   
         MVI   RRQROT-1,C','                                                    
         MVI   RRQSTIM-1,C','                                                   
         MVI   RRQETIM-1,C','                                                   
         MVI   RRQSDTIM-1,C','                                                  
         MVI   RRQPREC-1,C','                                                   
         MVI   RRQVTYPE-1,C','                                                  
         MVI   RRQDTYPE-1,C','                                                  
         MVI   RRQDCATS-1,C','                                                  
         MVI   RRQGRPBY-1,C','                                                  
         J     PPUTNX                                                           
         DROP  R2,R3,R4                                                         
*                                                                               
* NET SEED                                                                      
*                                                                               
PPUTND00 L     R2,ADBLOCK                                                       
         USING DBLOCKD,R2                                                       
         LA    R3,DSNREC                                                        
         USING RSQRECD,R3                                                       
         L     R4,ADBXNSED         A(DBEXTEND - SEED EXTENSION)                 
         USING DBSEEDD,R4                                                       
*                                                                               
         MVI   RSQTYPE,RSQTYPEQ    LINE TYPE                                    
         MVC   RSQNNUM,DBSENETN    NETWORK NUMBER                               
*                                  START/END TIME                               
         EDIT  (2,DBSERSTM),(4,RSQSTIM),ALIGN=RIGHT,FILL=0                      
         MVC   RSQETIM,RSQSTIM                                                  
         OC    DBSERETM,DBSERETM   ANY END TIME?                                
         JZ    PPUTND10                                                         
*                                                                               
         CLC   DBSERETM,=H'2400'   AFTER MIDNIGHT?                              
         BL    PPUTND05                                                         
         SR    RF,RF               YES                                          
         ICM   RF,3,DBSERETM                                                    
         SH    RF,=H'2400'                                                      
         STCM  RF,3,DBSERETM                                                    
*                                                                               
PPUTND05 EDIT  (2,DBSERETM),(4,RSQETIM),ALIGN=RIGHT,FILL=0                      
*                                                                               
PPUTND10 CLC   =C'2400',RSQSTIM                                                 
         JNE   *+10                                                             
         MVC   RSQSTIM,=C'0000'                                                 
         CLC   =C'2400',RSQETIM                                                 
         JNE   *+10                                                             
         MVC   RSQETIM,=C'0000'                                                 
*                                                                               
         OC    DBSERSDT,DBSERSDT                                                
         JZ    PPUTND20                                                         
         GOTO1 VDATCON,DMCB,(2,DBSERSDT),(23,RSQSDAT)  START DATE               
         OC    DBSEREDT,DBSEREDT                                                
         JZ    PPUTND20                                                         
         GOTO1 VDATCON,DMCB,(2,DBSEREDT),(23,RSQEDAT)  END DATE                 
*                                                                               
PPUTND20 LA    RF,RSQHASH                                                       
         USING HNDKEYD,RF                                                       
         MVC   HNDNNUM,RSQNNUM     NETWORK NUMBER                               
         MVI   HNDSDAT-1,C'|'                                                   
         MVC   HNDSDAT,RSQSDAT     START DATE                                   
         MVI   HNDEDAT-1,C'|'                                                   
         MVC   HNDEDAT,RSQEDAT     END DATE                                     
         MVI   HNDSTIM-1,C'|'                                                   
         MVC   HNDSTIM,RSQSTIM     START TIME                                   
         MVI   HNDETIM-1,C'|'                                                   
         MVC   HNDETIM,RSQETIM     END TIME                                     
         DROP  RF                                                               
*                                                                               
         MVI   RSQNNUM-1,C','                                                   
         MVI   RSQSDAT-1,C','                                                   
         MVI   RSQEDAT-1,C','                                                   
         MVI   RSQSTIM-1,C','                                                   
         MVI   RSQETIM-1,C','                                                   
         MVI   RSQHASH-1,C','                                                   
         J     PPUTNX                                                           
         DROP  R2,R3,R4                                                         
*                                                                               
* NET WRITER                                                                    
*                                                                               
PPUTNR00 L     R2,ADBLOCK                                                       
         USING DBLOCKD,R2                                                       
         LA    R3,DSNREC                                                        
         USING RWQRECD,R3                                                       
         L     R4,ADBXNWRI         A(DBEXTEND - NWRI EXTENSION)                 
         USING DBCNWD,R4                                                        
*                                                                               
         BRAS  RE,BLDDCAT          BUILD DEMO CATEGORY LIST                     
*                                                                               
         MVI   RWQTYPE,RWQTYPEQ    LINE TYPE                                    
         MVC   RWQNET#,DBCNWNET    COMSCORE NETWORK #                           
         MVC   RWQSER#,DBCNWSN     COMSCORE SERIES #                            
*                                  START/END TIME                               
         EDIT  (2,DBCNWST),(4,RWQSTIM),ALIGN=RIGHT,FILL=0                       
         MVC   RWQETIM,RWQSTIM                                                  
         OC    DBCNWET,DBCNWET     ANY END TIME?                                
         JZ    PPUTNR10                                                         
*                                                                               
         CLC   DBCNWET,=H'2400'    AFTER MIDNIGHT?                              
         BL    PPUTNR05                                                         
         SR    RF,RF               YES                                          
         ICM   RF,3,DBCNWET                                                     
         SH    RF,=H'2400'                                                      
         STCM  RF,3,DBCNWET                                                     
*                                                                               
PPUTNR05 EDIT  (2,DBCNWET),(4,RWQETIM),ALIGN=RIGHT,FILL=0                       
*                                                                               
PPUTNR10 CLC   =C'2400',RWQSTIM                                                 
         JNE   *+10                                                             
         MVC   RWQSTIM,=C'0000'                                                 
         CLC   =C'2400',RWQETIM                                                 
         JNE   *+10                                                             
         MVC   RWQETIM,=C'0000'                                                 
*                                                                               
         OC    DBCNWSD,DBCNWSD                                                  
         JZ    PPUTNR20                                                         
         GOTO1 VDATCON,DMCB,(2,DBCNWSD),(23,RWQSDTE)  START DATE                
         OC    DBCNWED,DBCNWED                                                  
         JZ    PPUTNR20                                                         
         GOTO1 VDATCON,DMCB,(2,DBCNWED),(23,RWQEDTE)  END DATE                  
*                                                                               
PPUTNR20 MVC   RWQPREC,DBCNWPRE    PRECISION                                    
         MVC   RWQVTYPE,VTYPE      VIEWING TYPE                                 
*                                                                               
         MVC   RWQDTYPE,=C'PAV'    DEFAULT PROGRAM AVERAGE                      
         TM    DBCNWFLG,DBCNWFTQ   TIME PERIOD?                                 
         JZ    *+10                                                             
         MVC   RWQDTYPE,=C'TP '                                                 
*                                                                               
         GOTOR SETROT,DMCB,DBCNWROT,RWQROT     SET ROTATION                     
         MVC   RWQSDTIM,=C'6:00A'  START OF DAY (DEFAULT 6AM)                   
         MVC   RWQDCATS,DEMOCATS   DEMO CATEGORIES                              
*                                                                               
         LA    RF,RWQHASH                                                       
         USING HNWKEYD,RF                                                       
         MVC   HNWNET#,RWQNET#     NETWORK NUMBER                               
         MVI   HNWSDTE-1,C'|'                                                   
         MVC   HNWSDTE,RWQSDTE     START DATE                                   
         MVI   HNWEDTE-1,C'|'                                                   
         MVC   HNWEDTE,RWQEDTE     END DATE                                     
         MVI   HNWSTIM-1,C'|'                                                   
         MVC   HNWSTIM,RWQSTIM     START TIME                                   
         MVI   HNWETIM-1,C'|'                                                   
         MVC   HNWETIM,RWQETIM     END TIME                                     
         MVI   HNWVTYPE-1,C'|'                                                  
         MVC   HNWROT,RWQROT       ROTATION                                     
         MVI   HNWROT-1,C'|'                                                    
         MVC   HNWVTYPE,RWQVTYPE   VIEWING TYPE                                 
         MVI   HNWSER#-1,C'|'                                                   
         MVC   HNWSER#,RWQSER#     SERIES #                                     
         DROP  RF                                                               
*                                                                               
         MVI   RWQHASH-1,C','                                                   
         MVI   RWQNET#-1,C','                                                   
         MVI   RWQSDTE-1,C','                                                   
         MVI   RWQEDTE-1,C','                                                   
         MVI   RWQROT-1,C','                                                    
         MVI   RWQSTIM-1,C','                                                   
         MVI   RWQETIM-1,C','                                                   
         MVI   RWQSDTIM-1,C','                                                  
         MVI   RWQPREC-1,C','                                                   
         MVI   RWQVTYPE-1,C','                                                  
         MVI   RWQDTYPE-1,C','                                                  
         MVI   RWQSER#-1,C','                                                   
         MVI   RWQDCATS-1,C','                                                  
         J     PPUTNX                                                           
         DROP  R2,R3,R4                                                         
*                                                                               
PPUTNX   J     YES                                                              
         LTORG                                                                  
***********************************************************************         
* BUILD SPOT DEMO REQUEST & PUT TO DATASET                                      
***********************************************************************         
PPUTS    NTR1  BASE=*,LABEL=*                                                   
         L     R2,ADBLOCK                                                       
         USING DBLOCKD,R2                                                       
         LA    R3,DSNREC                                                        
         USING REQRECD,R3                                                       
         L     R4,ADBXCLOC         A(DBEXTEND - CLOC EXTENSION)                 
         USING DBCMINTD,R4                                                      
*                                                                               
         BRAS  RE,BLDDCAT          BUILD DEMO CATEGORY LIST                     
*                                                                               
         OC    DBSELSTA,DBSELSTA                                                
         JZ    NO                                                               
         CLI   DBSELSTA,C' '                                                    
         JE    NO                                                               
         CLI   DBSELDAY,0          NNNNNNN ROTATION - SKIP                      
         JE    NO                                                               
         OC    DBCMMKT,DBCMMKT                                                  
         JZ    NO                                                               
*                                                                               
         MVI   REQTYPE,REQTYPEQ    LINE TYPE                                    
*                                                                               
         MVC   REQSTA(4),DBSELSTA                                               
         CLI   DBSELSTA+3,C'+'     PARENT+?                                     
         JE    PPUTS02                                                          
         CLI   DBSELSTA+4,C'+'     PARENT+                                      
         JNE   *+10                                                             
         MVC   REQSTA,DBSELSTA     STATION                                      
*                                                                               
PPUTS02  MVC   REQPREC,DBCMPREC    RATING PRECISION                             
*                                                                               
         MVI   REQIPREC,C'1'       DEFAULT TO 1 DECIMAL IMP PRECISION           
         TM    DBBSTFLG,DBBST2DI   2 DECIMAL IMP PRECISION?                     
         JZ    *+8                                                              
         MVI   REQIPREC,C'2'       IMPRESSION PRECISION                         
*                                                                               
         OC    DBCMMKT,DBCMMKT                                                  
         JZ    PPUTS10                                                          
         EDIT  DBCMMKT,REQMKT,ALIGN=LEFT        MARKET                          
*                                                                               
PPUTS10  OC    DBSELSYC,DBSELSYC                                                
         JZ    PPUTS12                                                          
         EDIT  DBSELSYC,REQSYS,ALIGN=LEFT       SYSCODE                         
*                                                                               
PPUTS12  BRAS  RE,CALCSYBK         CALC START/END DATES & SYSCODE BOOK          
         JNE   NO                                                               
*                                                                               
         MVC   REQSYBK,SVSYBK      BOOK                                         
         MVC   REQSDTE,SVSDTE      START DATE                                   
         MVC   REQEDTE,SVEDTE      END DATE                                     
*                                                                               
         GOTOR SETROT,DMCB,DBSELDAY,REQROT     SET ROTATION                     
*                                                       START/END TIME          
         EDIT  (2,DBSELTIM),(4,REQSTIM),ALIGN=RIGHT,FILL=0                      
         MVC   REQETIM,REQSTIM                                                  
         OC    DBSELTIM+2(2),DBSELTIM+2                                         
         JZ    PPUTS14                                                          
         EDIT  (2,DBSELTIM+2),(4,REQETIM),ALIGN=RIGHT,FILL=0                    
*                                                                               
PPUTS14  CLC   =C'2400',REQSTIM                                                 
         JNE   *+10                                                             
         MVC   REQSTIM,=C'0000'                                                 
         CLC   =C'2400',REQETIM                                                 
         JNE   *+10                                                             
         MVC   REQETIM,=C'0000'                                                 
*                                                                               
         GOTO1 VDATCON,DMCB,(2,DBCMSPDT),(23,REQSDAT)   SPOT DATE               
*                                                                               
         BRAS  RE,BLDHASH          BUILD HASHKEY FROM DBLOCK                    
         MVC   REQHASH,HASHKEY     HASHKEY                                      
*                                                                               
         MVC   REQDCATS,DEMOCATS   DEMO CATEGORIES                              
*                                                                               
         MVI   REQTYPE+L'REQTYPE,C','                                           
         MVI   REQSTA+L'REQSTA,C','                                             
         MVI   REQMKT+L'REQMKT,C','                                             
         MVI   REQSYS+L'REQSYS,C','                                             
         MVI   REQSYBK+L'REQSYBK,C','                                           
         MVI   REQSDTE+L'REQSDTE,C','                                           
         MVI   REQEDTE+L'REQEDTE,C','                                           
         MVI   REQROT+L'REQROT,C','                                             
         MVI   REQSTIM+L'REQSTIM,C','                                           
         MVI   REQETIM+L'REQETIM,C','                                           
         MVI   REQHASH+L'REQHASH,C','                                           
         MVI   REQPREC+L'REQPREC,C','                                           
         MVI   REQSDAT+L'REQSDAT,C','                                           
         MVI   REQIPREC+L'REQIPREC,C','                                         
*                                                                               
PPUTSX   J     YES                                                              
         DROP  R2,R3,R4                                                         
         LTORG                                                                  
***********************************************************************         
* PROCSTRT FOR NET                                                              
***********************************************************************         
PSTRTN   NTR1  BASE=*,LABEL=*                                                   
         CLI   SVCALLER,SVCNNPQ    NPOD?                                        
         JE    PSTRNP00                                                         
         CLI   SVCALLER,SVCNSDQ    NET SEED?                                    
         JE    PSTRND00                                                         
         CLI   SVCALLER,SVCNWRQ    NET WRITER?                                  
         JE    PSTRNR00                                                         
         J     NO                                                               
*                                                                               
* NET RESEARCH WRITER (NPOD)                                                    
* BUILD BINSRCH TABLE WITH DEMO VALUE RECORDS FROM DATASET                      
*                                                                               
PSTRNP00 L     R2,=A(FILEIN)                                                    
         LA    R3,OUTREC                                                        
PSTRNP10 GET   (2),(3)             GET RECORD                                   
*                                                                               
         LA    RE,DSNREC           SKIP FIRST 4 BYTES FROM DATASET              
         LA    RF,DSNRECLN                                                      
         LA    R0,OUTREC+4                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   DSNREC,C'D'         DEMO VALUE RECORD?                           
         JNE   PSTRNP10                                                         
         BRAS  RE,BLDBIN           BUILD BINSRCH RECORD                         
*                                                                               
         SAM31                                                                  
         LA    R4,BPARAMS          NET RESEARCH WRITER                          
         USING BSPARA,R4                                                        
*                                                                               
         LA    RF,BRNREC                                                        
         ST    RF,BSPAREC          A(RECORD)                                    
*                                                                               
         MVC   BSPSTRT,ABINTAB     A(BINSRCH TABLE)                             
*                                                                               
         LA    RF,BRNRECLN                                                      
         ST    RF,BSPLENR          L'RECORD                                     
         MVI   BSPLENR,X'01'       INSERT IF NOT FOUND                          
*                                                                               
         LA    RF,BRNKEYLN                                                      
         ST    RF,BSPLENK          L'KEY                                        
         MVI   BSPKEYD,0           DISPLACEMENT OF KEY                          
         LARL  R1,SVNUMREC                                                      
         MVC   BSPEND,0(R1)        MAX NUMBER OF RECORDS                        
*                                                                               
         GOTO1 VBINSRCH,BPARAMS                                                 
         SAM24                                                                  
*                                                                               
         J     PSTRNP10                                                         
         DROP  R4                                                               
*                                                                               
* NET SEED REPORT                                                               
* BUILD BINSRCH TABLE WITH PROGRAM NAMES FROM DATASET                           
*                                                                               
PSTRND00 XC    PHNDKEY,PHNDKEY                                                  
         L     R2,=A(FILEIN)                                                    
         LA    R3,OUTREC                                                        
PSTRND10 GET   (2),(3)             GET RECORD                                   
*                                                                               
         LA    RE,DSNREC           SKIP FIRST 4 BYTES FROM DATASET              
         LA    RF,DSNRECLN                                                      
         LA    R0,OUTREC+4                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   DSNREC,C'D'         DEMO VALUE RECORD?                           
         JNE   PSTRND10                                                         
         BRAS  RE,BLDBIN           BUILD BINSRCH RECORD                         
*                                                                               
         SAM31                                                                  
         LA    R4,BPARAMS          NET RESEARCH WRITER                          
         USING BSPARA,R4                                                        
*                                                                               
         LA    RF,BNDREC                                                        
         ST    RF,BSPAREC          A(RECORD)                                    
*                                                                               
         MVC   BSPSTRT,ABINTAB     A(BINSRCH TABLE)                             
*                                                                               
         LA    RF,BNDRECLN                                                      
         ST    RF,BSPLENR          L'RECORD                                     
         MVI   BSPLENR,X'01'       INSERT IF NOT FOUND                          
*                                                                               
         LA    RF,BNDKEYLN                                                      
         ST    RF,BSPLENK          L'KEY                                        
         MVI   BSPKEYD,0           DISPLACEMENT OF KEY                          
         LARL  R1,SVNUMREC                                                      
         MVC   BSPEND,0(R1)        MAX NUMBER OF RECORDS                        
*                                                                               
         GOTO1 VBINSRCH,BPARAMS                                                 
         SAM24                                                                  
*                                                                               
         J     PSTRND10                                                         
         DROP  R4                                                               
*                                                                               
* NET WRITER                                                                    
* BUILD BINSRCH TABLE WITH DEMO VALUE RECORDS FROM DATASET                      
*                                                                               
PSTRNR00 XC    PHNWKEY,PHNWKEY                                                  
         L     R2,=A(FILEIN)                                                    
         LA    R3,OUTREC                                                        
PSTRNR10 GET   (2),(3)             GET RECORD                                   
*                                                                               
         LA    RE,DSNREC           SKIP FIRST 4 BYTES FROM DATASET              
         LA    RF,DSNRECLN                                                      
         LA    R0,OUTREC+4                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   DSNREC,C'D'         DEMO VALUE RECORD?                           
         JNE   PSTRNR10                                                         
         BRAS  RE,BLDBIN           BUILD BINSRCH RECORD                         
*                                                                               
         SAM31                                                                  
         LA    R4,BPARAMS          NET RESEARCH WRITER                          
         USING BSPARA,R4                                                        
*                                                                               
         LA    RF,BNWREC                                                        
         ST    RF,BSPAREC          A(RECORD)                                    
*                                                                               
         MVC   BSPSTRT,ABINTAB     A(BINSRCH TABLE)                             
*                                                                               
         LA    RF,BNWRECLN                                                      
         ST    RF,BSPLENR          L'RECORD                                     
         MVI   BSPLENR,X'01'       INSERT IF NOT FOUND                          
*                                                                               
         LA    RF,BNWKEYLN                                                      
         ST    RF,BSPLENK          L'KEY                                        
         MVI   BSPKEYD,0           DISPLACEMENT OF KEY                          
         LARL  R1,SVNUMREC                                                      
         MVC   BSPEND,0(R1)        MAX NUMBER OF RECORDS                        
*                                                                               
         GOTO1 VBINSRCH,BPARAMS                                                 
         SAM24                                                                  
*                                                                               
         J     PSTRNR10                                                         
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* PROCSTRT FOR SPOT                                                             
***********************************************************************         
PSTRTS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* BUILD BINSRCH TABLE WITH DEMO VALUE RECORDS FROM DATASET                      
*                                                                               
         XC    PREVKEY,PREVKEY                                                  
         L     R2,=A(FILEIN)                                                    
         LA    R3,OUTREC                                                        
PSTRTS10 GET   (2),(3)             GET RECORD                                   
*                                                                               
         LA    RE,DSNREC           SKIP FIRST 4 BYTES FROM DATASET              
         LA    RF,DSNRECLN                                                      
         LA    R0,OUTREC+4                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   DSNREC,C'D'         DEMO VALUE RECORD?                           
         JNE   PSTRTS10                                                         
         BRAS  RE,BLDBIN           BUILD BINSRCH RECORD                         
*                                                                               
         SAM31                                                                  
         LA    R4,BPARAMS                                                       
         USING BSPARA,R4                                                        
*                                                                               
         LA    RF,BINREC                                                        
         ST    RF,BSPAREC          A(RECORD)                                    
*                                                                               
         MVC   BSPSTRT,ABINTAB     A(BINSRCH TABLE)                             
*                                                                               
         LA    RF,BINRECLN                                                      
         ST    RF,BSPLENR          L'RECORD                                     
         MVI   BSPLENR,X'01'       INSERT IF NOT FOUND                          
*                                                                               
         LA    RF,BINKEYLN                                                      
         ST    RF,BSPLENK          L'KEY                                        
         MVI   BSPKEYD,0           DISPLACEMENT OF KEY                          
*                                                                               
         LARL  R1,SVNUMREC                                                      
         MVC   BSPEND,0(R1)        MAX NUMBER OF RECORDS                        
*                                                                               
         GOTO1 VBINSRCH,BPARAMS                                                 
         SAM24                                                                  
*                                                                               
         J     PSTRTS10                                                         
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* SQUASHIT                                                                      
*   ON EXIT - OUTREC HAS SQUASHED FIELD                                         
***********************************************************************         
SQUASHIT NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(FIELD TO SQUASH)                           
*                                                                               
         LA    R4,DSNRECLN                                                      
SQ10     CLI   0(R2),C' '          MAKE EVERYTHING 0 PADDED                     
         JH    *+8                                                              
         MVI   0(R2),0                                                          
         AHI   R2,1                                                             
         BCT   R4,SQ10                                                          
*                                                                               
         LA    RE,OUTREC                                                        
         LA    RF,DSNRECLN                                                      
         XCEF                                                                   
*                                                                               
         L     R2,0(R1)            A(FIELD TO SQUASH)                           
         LA    R3,OUTREC                                                        
*                                                                               
         LA    R4,DSNRECLN                                                      
         TM    MODE,GETQ           GET MODE?                                    
         JZ    *+8                                                              
         LA    R4,L'HASHKEY                                                     
*                                                                               
SQ20     CLI   0(R2),0             SQUASH ALL 0'S                               
         JE    SQ25                                                             
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
SQ25     AHI   R2,1                                                             
         BCT   R4,SQ20                                                          
*                                                                               
SQUASHX  J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* SQUASH BHNDKEY                                                                
***********************************************************************         
SQSHBHND NTR1  BASE=*,LABEL=*                                                   
         LA    R2,BHNDKEY                                                       
*                                                                               
         LA    R4,L'BHNDKEY                                                     
SBHND10  CLI   0(R2),C' '          MAKE EVERYTHING 0 PADDED                     
         JH    *+8                                                              
         MVI   0(R2),0                                                          
         AHI   R2,1                                                             
         BCT   R4,SBHND10                                                       
*                                                                               
         LA    RE,OUTREC                                                        
         L     RF,FULL                                                          
         XCEF                                                                   
*                                                                               
         LA    R2,BHNDKEY                                                       
         LA    R3,OUTREC                                                        
*                                                                               
         LA    R4,L'BHNDKEY                                                     
SBHND20  CLI   0(R2),0             SQUASH ALL 0'S                               
         JE    SBHND25                                                          
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
SBHND25  AHI   R2,1                                                             
         BCT   R4,SBHND20                                                       
*                                                                               
         XC    BHNDKEY,BHNDKEY                                                  
         MVC   BHNDKEY,OUTREC                                                   
         OC    BHNDKEY(L'BNDHASH),=CL50' '   SPACE PAD HASHKEY ONLY             
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* SQUASH BHNWKEY                                                                
***********************************************************************         
SQSHBHNW NTR1  BASE=*,LABEL=*                                                   
         LA    R2,BHNWKEY                                                       
*                                                                               
         LA    R4,L'BHNWKEY                                                     
SBHNW10  CLI   0(R2),C' '          MAKE EVERYTHING 0 PADDED                     
         JH    *+8                                                              
         MVI   0(R2),0                                                          
         AHI   R2,1                                                             
         BCT   R4,SBHNW10                                                       
*                                                                               
         LA    RE,OUTREC                                                        
         LA    RF,DSNRECLN                                                      
         XCEF                                                                   
*                                                                               
         LA    R2,BHNWKEY                                                       
         LA    R3,OUTREC                                                        
*                                                                               
         LA    R4,L'BHNWKEY                                                     
SBHNW20  CLI   0(R2),0             SQUASH ALL 0'S                               
         JE    SBHNW25                                                          
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
SBHNW25  AHI   R2,1                                                             
         BCT   R4,SBHNW20                                                       
*                                                                               
         XC    BHNWKEY,BHNWKEY                                                  
         MVC   BHNWKEY,OUTREC                                                   
         OC    BHNWKEY(L'BNWHASH),=CL100' '   SPACE PAD HASHKEY ONLY            
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* SQUASH DEMOCATS                                                               
***********************************************************************         
SQSHDEMO NTR1  BASE=*,LABEL=*                                                   
         LA    R2,DEMOCATS                                                      
*                                                                               
         LA    R4,L'DEMOCATS                                                    
SDEMO10  CLI   0(R2),C' '          MAKE EVERYTHING 0 PADDED                     
         JH    *+8                                                              
         MVI   0(R2),0                                                          
         AHI   R2,1                                                             
         BCT   R4,SDEMO10                                                       
*                                                                               
         LA    RE,OUTREC                                                        
         LA    RF,DSNRECLN                                                      
         XCEF                                                                   
*                                                                               
         LA    R2,DEMOCATS                                                      
         LA    R3,OUTREC                                                        
*                                                                               
         LA    R4,L'DEMOCATS                                                    
SDEMO20  CLI   0(R2),0             SQUASH ALL 0'S                               
         JE    SDEMO25                                                          
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
SDEMO25  AHI   R2,1                                                             
         BCT   R4,SDEMO20                                                       
*                                                                               
         XC    DEMOCATS,DEMOCATS                                                
         MVC   DEMOCATS,OUTREC                                                  
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* UNSQUASH                                                                      
*   ON EXIT - OUTREC HAS UNSQUASHED FIELD                                       
*           - ANXTFLD HAS A(NEXT FIELD)                                         
***********************************************************************         
UNSQUASH NTR1  BASE=*,LABEL=*                                                   
         L     RF,0(R1)                                                         
         CLC   =C'HASHKEY',0(RF)   UNSQUASH HASHKEY?                            
         JNE   UNSQX                                                            
*                                                                               
         LA    RE,OUTREC                                                        
         LA    RF,DSNRECLN                                                      
         XCEF                                                                   
*                                                                               
         L     R2,4(R1)            A(FIELD TO UNSQUASH)                         
         LA    R3,OUTREC                                                        
         USING HASHKEYD,R3                                                      
         LA    R4,L'HASHSTA        UNSQUASH STATION                             
*                                                                               
UNSQ10   CLI   0(R2),C'|'          DONE?                                        
         JE    UNSQ20                                                           
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
         AHI   R2,1                                                             
         BCT   R4,UNSQ10                                                        
*                                                                               
UNSQ20   AHI   R2,1                                                             
         LA    R3,OUTREC                                                        
         LA    R3,HASHMKT                                                       
         LA    R4,L'HASHMKT        UNSQUASH MARKET                              
*                                                                               
UNSQ25   CLI   0(R2),C'|'          DONE?                                        
         JE    UNSQ30                                                           
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
         AHI   R2,1                                                             
         BCT   R4,UNSQ25                                                        
*                                                                               
UNSQ30   AHI   R2,1                                                             
         LA    R3,OUTREC                                                        
         LA    R3,HASHSYS                                                       
         LA    R4,L'HASHSYS        UNSQUASH SYSCODE                             
*                                                                               
UNSQ35   CLI   0(R2),C'|'          DONE?                                        
         JE    UNSQ40                                                           
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
         AHI   R2,1                                                             
         BCT   R4,UNSQ35                                                        
*                                                                               
UNSQ40   AHI   R2,1                                                             
         LA    R3,OUTREC                                                        
         LA    R3,HASHSDTE                                                      
         LA    R4,L'HASHSDTE       UNSQUASH START DATE                          
*                                                                               
UNSQ45   CLI   0(R2),C'|'          DONE?                                        
         JE    UNSQ50                                                           
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
         AHI   R2,1                                                             
         BCT   R4,UNSQ45                                                        
*                                                                               
UNSQ50   AHI   R2,1                                                             
         LA    R3,OUTREC                                                        
         LA    R3,HASHEDTE                                                      
         LA    R4,L'HASHEDTE       UNSQUASH END DATE                            
*                                                                               
UNSQ55   CLI   0(R2),C'|'          DONE?                                        
         JE    UNSQ60                                                           
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
         AHI   R2,1                                                             
         BCT   R4,UNSQ55                                                        
*                                                                               
UNSQ60   AHI   R2,1                                                             
         LA    R3,OUTREC                                                        
         LA    R3,HASHSYBK                                                      
         LA    R4,L'HASHSYBK       UNSQUASH SYSCODE BOOK                        
*                                                                               
UNSQ65   CLI   0(R2),C'|'          DONE?                                        
         JE    UNSQ70                                                           
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
         AHI   R2,1                                                             
         BCT   R4,UNSQ65                                                        
*                                                                               
UNSQ70   AHI   R2,1                                                             
         LA    R3,OUTREC                                                        
         LA    R3,HASHROT                                                       
         LA    R4,L'HASHROT        UNSQUASH ROTATION                            
*                                                                               
UNSQ75   CLI   0(R2),C'|'          DONE?                                        
         JE    UNSQ80                                                           
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
         AHI   R2,1                                                             
         BCT   R4,UNSQ75                                                        
*                                                                               
UNSQ80   AHI   R2,1                                                             
         LA    R3,OUTREC                                                        
         LA    R3,HASHSTIM                                                      
         LA    R4,L'HASHSTIM       UNSQUASH START TIME                          
*                                                                               
UNSQ85   CLI   0(R2),C'|'          DONE?                                        
         JE    UNSQ90                                                           
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
         AHI   R2,1                                                             
         BCT   R4,UNSQ85                                                        
*                                                                               
UNSQ90   AHI   R2,1                                                             
         LA    R3,OUTREC                                                        
         LA    R3,HASHETIM                                                      
         LA    R4,L'HASHETIM       UNSQUASH END TIME                            
*                                                                               
UNSQ95   CLI   0(R2),C'|'          DONE?                                        
         JE    UNSQ100                                                          
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
         AHI   R2,1                                                             
         BCT   R4,UNSQ95                                                        
*                                                                               
UNSQ100  AHI   R2,1                                                             
         LA    R3,OUTREC                                                        
         LA    R3,HASHSDAT                                                      
         LA    R4,L'HASHSDAT       UNSQUASH SPOT DATE                           
*                                                                               
UNSQ105  CLI   0(R2),C','          DONE?                                        
         JE    UNSQ110                                                          
         MVC   0(1,R3),0(R2)                                                    
         AHI   R3,1                                                             
         AHI   R2,1                                                             
         BCT   R4,UNSQ105                                                       
*                                                                               
UNSQ110  AHI   R2,1                                                             
         ST    R2,ANXTFLD          A(NEXT FIELD) = A(DEMO LIST)                 
*                                                                               
         LA    R3,OUTREC                                                        
         MVI   HASHSTA+L'HASHSTA,C'|'                                           
         MVI   HASHMKT+L'HASHMKT,C'|'                                           
         MVI   HASHSYS+L'HASHSYS,C'|'                                           
         MVI   HASHSDTE+L'HASHSDTE,C'|'                                         
         MVI   HASHEDTE+L'HASHEDTE,C'|'                                         
         MVI   HASHSYBK+L'HASHSYBK,C'|'                                         
         MVI   HASHROT+L'HASHROT,C'|'                                           
         MVI   HASHSTIM+L'HASHSTIM,C'|'                                         
         MVI   HASHETIM+L'HASHETIM,C'|'                                         
         OC    OUTREC(L'HASHKEY),=CL100' '                                      
         J     UNSQX                                                            
*                                                                               
UNSQX    J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* SET ROTATION                                                                  
* PARAM 1 = ROTATION (HEX)                                                      
* PARAM 2 = A(OUTPUT IN EBCDIC)                                                 
***********************************************************************         
SETROT   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            DBSELDAY                                     
         L     R3,4(R1)            A(OUTPUT)                                    
*                                                                               
         MVC   0(7,R3),=C'NNNNNNN' DEFAULT ROTATION                             
         TM    0(R2),X'40'         MONDAY?                                      
         JZ    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         TM    0(R2),X'20'         TUESDAY?                                     
         JZ    *+8                                                              
         MVI   1(R3),C'Y'                                                       
         TM    0(R2),X'10'         WEDNESDAY?                                   
         JZ    *+8                                                              
         MVI   2(R3),C'Y'                                                       
         TM    0(R2),X'08'         THURSDAY?                                    
         JZ    *+8                                                              
         MVI   3(R3),C'Y'                                                       
         TM    0(R2),X'04'         FRIDAY?                                      
         JZ    *+8                                                              
         MVI   4(R3),C'Y'                                                       
         TM    0(R2),X'02'         SATURDAY?                                    
         JZ    *+8                                                              
         MVI   5(R3),C'Y'                                                       
         TM    0(R2),X'01'         SUNDAY?                                      
         JZ    *+8                                                              
         MVI   6(R3),C'Y'                                                       
*                                                                               
SETROTX  J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* SET ROTATION                                                                  
* PARAM 1 = ROTATION (EBCDIC)                                                   
* PARAM 2 = A(OUTPUT IN HEX)                                                    
***********************************************************************         
SETROTH  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            ROTATION (EBCDIC)                            
         L     R3,4(R1)            A(OUTPUT)                                    
*                                                                               
         MVI   0(R3),0                                                          
         CLI   0(R2),C'Y'          MONDAY?                                      
         JNE   *+8                                                              
         OI    0(R3),X'40'                                                      
         CLI   1(R2),C'Y'          TUESDAY?                                     
         JNE   *+8                                                              
         OI    0(R3),X'20'                                                      
         CLI   2(R2),C'Y'          WEDNESDAY?                                   
         JNE   *+8                                                              
         OI    0(R3),X'10'                                                      
         CLI   3(R2),C'Y'          THURSDAY?                                    
         JNE   *+8                                                              
         OI    0(R3),X'08'                                                      
         CLI   4(R2),C'Y'          FRIDAY?                                      
         JNE   *+8                                                              
         OI    0(R3),X'04'                                                      
         CLI   5(R2),C'Y'          SATURDAY?                                    
         JNE   *+8                                                              
         OI    0(R3),X'02'                                                      
         CLI   6(R2),C'Y'          SUNDAY?                                      
         JNE   *+8                                                              
         OI    0(R3),X'01'                                                      
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
***********************************************************************         
* EQUATES                                                                       
***********************************************************************         
***********************************************************************         
* DSECT TO COVER WORKING STORAGE                                                
***********************************************************************         
WORKD    DSECT                                                                  
RELO     DS    A                                                                
BYTE     DS    X                                                                
HALF     DS    H                                                                
FULL     DS    F                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
KEY      DS    XL48                                                             
KEYSAVE  DS    XL48                                                             
WORK     DS    XL64                                                             
ELCODE   DS    XL1                                                              
DATADISP DS    H                                                                
*                                                                               
VADDAY   DS    A                   A(ADDAY)                                     
VBINSRCH DS    A                   A(BINSRCH)                                   
VDATCON  DS    A                   A(DATCON)                                    
VDEMTABS DS    A                   A(DEMTABS)                                   
VDMGR    DS    A                   A(DATAMGR)                                   
VDYN     DS    A                   A(DYNALLOC)                                  
VGETBROD DS    A                   A(GETBROAD)                                  
VGETDAY  DS    A                   A(GETDAY)                                    
VMASTC   DS    A                   A(MASTC)                                     
VMQRPT   DS    A                   A(MQRPT)                                     
VNSIWEEK DS    A                   A(NSIWEEK)                                   
VSQUASHR DS    A                   A(SQUASHER)                                  
*                                                                               
ANXTFLD  DS    F                   A(NEXT FIELD                                 
APARMS   DS    F                   A(INPUT PARAMETERS)                          
ACOMFACS DS    F                   A(COMFACS)                                   
ADBLOCK  DS    F                   A(DBLOCK)                                    
ADBXCLOC DS    F                   A(DBEXTEND - CLOC EXTENSION)                 
ADBXCSRN DS    F                   A(DBEXTEND - CSRN EXTENSION)                 
ADBXNSED DS    F                   A(DBEXTEND - NSED EXTENSION)                 
ADBXNWRI DS    F                   A(DBEXTEND - NWRI EXTENSION)                 
ADBXNEXT DS    F                   A(DBEXTEND - NEXT EXTENSION)                 
ABINTAB  DS    F                   A(BINSRCH TABLE)                             
*                                                                               
SVPNAME  DS    CL(L'DBCMPNAM)      COMSCORE PROGRAM NAME                        
SVSDTE   DS    CL10                SAVED START DATE                             
SVEDTE   DS    CL10                SAVED END DATE                               
SVSYBK   DS    CL6                 SAVED SYSCODE BOOK                           
SVDEMOFF DS    X                   DEMO OFFSET IN REQ DEMO LIST                 
SVDEMTYP DS    C                   TYPE OF DEMO LIST                            
*                                                                               
SYSTEM   DS    XL1                 SYSTEM                                       
SPOTQ    EQU   2                   SPOT SYSTEM (MCOVSYS)                        
NETQ     EQU   3                   NET SYSTEM (MCOVSYS)                         
*                                                                               
SVCALLER DS    XL1                 CALLING PROGRAM                              
SVCSPTQ  EQU   C'S'                SPOT                                         
SVCNSDQ  EQU   C'D'                NET SEED REPORT                              
SVCNETQ  EQU   C'N'                NET                                          
SVCNEXQ  EQU   C'E'                NET EXTRACT                                  
SVCNNPQ  EQU   C'P'                NET RESEARCH WRITER (NPOD)                   
SVCNWRQ  EQU   C'W'                NET WRITER                                   
*                                                                               
USER     DS    CL2                 USER                                         
VTYPE    DS    CL2                 VIEWING TYPE                                 
*                                                                               
REPSDATE DS    CL10                REPORT START DATE                            
REPEDATE DS    CL10                REPORT END DATE                              
*                                                                               
DELIMITR DS    X                   DELIMITER                                    
FLDH     DS    XL8                                                              
FLD      DS    CL50                                                             
*                                                                               
SWEEPTBL EQU   9                   SWEEP TABLE IN DEDEMTABS                     
*                                                                               
MODE     DS    X                   MODE                                         
ALLOCQ   EQU   X'80'               ALLOCATE & OPEN DATASET                      
CLOSEQ   EQU   X'40'               CLOSE DATASET                                
ENDQ     EQU   X'20'               DEL BINSRCH TABLE & DEALLOC DATASET          
GETQ     EQU   X'10'               GET DEMOS FROM BINSRCH TABLE                 
PUTQ     EQU   X'08'               BUILD DEMO REQUEST & PUT TO DATASET          
STARTQ   EQU   X'04'               OPEN DATASET & BUILD BINSRCH TABLE           
*                                                                               
         DS    0F                                                               
BPARAMS  DS    XL32                BINSRCH PARAMETERS                           
BINTABLN DS    F                   L'BINSRCH TABLE IN MEMORY                    
*                                                                               
DSN      DS    CL44                DATASET NAME                                 
DSNREC   DS    CL550               DATASET RECORD                               
DSNRECLN EQU   *-DSNREC                                                         
OUTREC   DS    CL550               DATASET RECORD                               
*                                                                               
* SPOT KEYS & RECS                                                              
*                                                                               
HASHKEY  DS    CL(HASHLNQ)         HASHKEY (SPOT)                               
PREVKEY  DS    CL(PREVHLNQ)        PREVIOUS HASHKEY + SEQUENCE #                
         ORG   PREVKEY                                                          
PREVHKEY DS    CL(HASHLNQ)         PREVIOUS HASHKEY                             
PREVHSEQ DS    X                   PREVIOUS HASHKEY SEQUENCE NUMBER             
PREVHLNQ EQU   *-PREVHKEY                                                       
BINREC   DS    CL(BINRECLN)        BINSRCH RECORD (SPOT)                        
*                                                                               
* NPOD KEYS & RECS                                                              
*                                                                               
NETBKEY  DS    CL(BRNKEYLN)        BINSRCH KEY FOR NET RESEARCH WRITER          
BRNREC   DS    CL(BRNRECLN)        BINSRCH RECORD (NPOD)                        
*                                                                               
* NET SEED KEYS & RECS                                                          
*                                                                               
HNDKEY   DS    CL(HNDLNQ)          HASHKEY (NET SEED)                           
PHNDKEY  DS    CL(PHNDHLNQ)        PREVIOUS HASHKEY + SEQUENCE #                
         ORG   PHNDKEY                                                          
PHNDHKEY DS    CL(HNDLNQ)          PREVIOUS HASHKEY                             
PHNDHSEQ DS    X                   PREVIOUS HASHKEY SEQUENCE #                  
PHNDHLNQ EQU   *-PHNDHKEY                                                       
BHNDKEY  DS    CL(BNDKEYLN)        BINSRCH KEY FOR NET SEED REPORT              
BNDREC   DS    CL(BNDRECLN)        BINSRCH RECORD (NET SEED)                    
*                                                                               
* NET WRITER KEYS & RECS                                                        
*                                                                               
HNWKEY   DS    CL(HNWLNQ)          HASHKEY (NET WRITER)                         
PHNWKEY  DS    CL(PHNWHLNQ)        PREVIOUS HASHKEY + SEQUENCE #                
         ORG   PHNWKEY                                                          
PHNWHKEY DS    CL(HNWLNQ)          PREVIOUS HASHKEY                             
PHNWHSEQ DS    XL4                 PREVIOUS HASHKEY SEQUENCE #                  
PHNWHLNQ EQU   *-PHNWHKEY                                                       
BHNWKEY  DS    CL(BNWKEYLN)        BINSRCH KEY FOR NET WRITER                   
BNWREC   DS    CL(BNWRECLN)        BINSRCH RECORD (NET WRITER)                  
*                                                                               
DEMOCATS DS    CL(SPTDMAXQ*(DEMNAMLQ+1))  ALHPA DEMO CAT W/ DELIMITER           
*                                                                               
PARMERRQ EQU   X'80'               INVALID PARAMETER                            
OPCLERRQ EQU   X'40'               UNABLE TO OPEN/CLOSE DATASET                 
PUTERRQ  EQU   X'20'               UNABLE TO PUT RECORD TO DATASET              
MQERRQ   EQU   X'10'               UNABLE TO PUT MQ MESSAGE OUT                 
TOKERRQ  EQU   X'08'               TOKEN RECORD MISSING                         
DSNERRQ  EQU   X'04'               INVALID VALUE IN DATASET                     
FAILERRQ EQU   X'02'               DEMO REQUEST FAILED                          
*                                                                               
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDBSPARA          BINSRCH PARAMETERS                           
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDMASTD                                                        
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK          DBLOCK                                       
       ++INCLUDE DEDBEXTRAD        DBLOCK EXTEND FIELD DSECT                    
       ++INCLUDE DERENTABD                                                      
       ++INCLUDE FASSB                                                          
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE GEGENTOK                                                       
       ++INCLUDE SPGENBUY                                                       
ESTRECD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE COMINTERD                                                      
       ++INCLUDE NEGENPACK                                                      
***********************************************************************         
* DSECT TO COVER SWEEP TABLE ENTRY                                              
***********************************************************************         
SWPTABD  DSECT                                                                  
SWPTTYPE DS    CL3                 TTN                                          
SWPTYR   DS    XL1                 BINARY YEAR                                  
SWPTMO   DS    XL1                 BINARY MONTH                                 
SWPTNUM  DS    XL1                 # WEEK SWEEP                                 
SWPTSDTE DS    0CL10               START DATE                                   
SWPTYEAR DS    CL4                 YEAR                                         
         DS    CL1                                                              
SWPTMON  DS    CL2                 MONTH                                        
         DS    CL1                                                              
SWPTDAY  DS    CL2                 DAY                                          
         DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010DDCOMINT  11/18/19'                                      
         END                                                                    
