*          DATA SET WZSHFIMON  AT LEVEL 005 AS OF 10/02/13                      
*PHASE WZMONA                                                                   
*INCLUDE FATABOFF                                                               
*INCLUDE DMDMGRL                                                                
*INCLUDE SHFIMON                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE QSORT                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE PERVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DDWTO                                                                  
*INCLUDE TREE                                                                   
*                                                                               
***********************************************************************         
* *NOTES                                                                        
* R8 = A(SHARED MEMORY AREA) AND IS SET IN SHFIMON                              
* R9 = A(SPECIFIC FILE'S INDEX TABLE) AND IS SET IN SHFIMON                     
* RA IS USED BY SHFIMON AND CANNOT BE USED IN THIS MODULE                       
* RC = A(WORKING STORAGE) AND IS PASSED TO SHFIMON                              
***********************************************************************         
         TITLE 'WZMON - WRKZ WORKER FILE INDEX MONITOR'                         
         PRINT NOGEN                                                            
WZMON    CSECT                                                                  
         ENTRY SETFINF             ROUTINES NEEDED BY DDSHFIMON                 
         ENTRY ADDFINF                                                          
         ENTRY FILSCAN                                                          
*                                                                               
         NBASE WORKX-WORKD,**WZMO**,AWORK,CLEAR=YES                             
         USING WORKD,RC            WORKING STORAGE                              
         USING SIHDRD,R8           SHARED MEMORY HEADER                         
         USING SITABD,R9           SHARED MEMORY INDEX TABLE HEADER             
         USING PLINED,PLINE        PRINT LINE                                   
         USING UKRECD,APINDEX      PRINT QUEUE INDEX                            
*                                                                               
         GOTO1 =V(SHFIMON),DMCB,(RC) SHFIMON GETS RC=(WORKING STORAGE)          
*                                                                               
         XBASE                                                                  
*                                                                               
AWORK    DC    A(WORKAREA)                                                      
         LTORG                                                                  
                                                                                
***********************************************************************         
* SET WRKZ WORKER FILE INFORMATION                                              
***********************************************************************         
SETFINF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   FIWCIA,=X'00001001'                                              
         GOTO1 =V(READCI)          READ THE FIRST CI                            
*                                                                               
         L     R4,ACIREC                                                        
         USING CIFDATA,R4                                                       
*                                                                               
         MVC   RIFTRK,FIWCIA                                                    
         MVC   RIFXID,CFFWFXID     FILE EXTERNAL ID (TST/ADV/REP)               
*                                                                               
         MVI   RIDNDX,SI1NDX-SI1PARD     DISPL  TO INDEX   IN NODE              
         MVI   RIDPREV,W_CIPREV-W_RECD   DISPL  TO PREV CI IN RECORD            
         MVI   RIDNEXT,W_CINEXT-W_RECD   DISPL  TO NEXT CI IN RECORD            
         MVI   RIDXKEY,W_KEY-W_INDEX     DISPL  TO KEY     IN INDEX             
         MVI   RIDSEQ,W_SEQ-W_INDEX      DISPL  TO SEQ#    IN INDEX             
         MVI   RIDREF,W_FILENO-W_INDEX   DISPL  TO FILE #  IN INDEX             
         MVI   RIDSTAT,W_STAT-W_INDEX    DISPL  TO STATUS  IN INDEX             
         MVI   RILNDX,L'W_INDEX          LENGTH OF INDEX                        
         MVI   RILPREV,L'W_CIPREV        LENGTH OF PREV CI                      
         MVI   RILNEXT,L'W_CINEXT        LENGTH OF NEXT CI                      
         MVI   RILKEY,L'W_KEY+L'W_FILENO LENGTH OF KEY = 10 BYTES               
         MVI   RILREF,L'W_FILENO         LENGTH OF FILE #                       
         OI    RIVULN,W_STSE             VULNERABLE BITS                        
*                                                                               
         LLH   R1,CIFBLKLN         BLOCK LENGTH                                 
         ST    R1,RIBLKLN          DATA SET BLOCK LENGTH                        
*                                                                               
         MVC   HALF,CIFINDX        INDEX TOTAL NUMBER OF CIS                    
         NI    HALF,X'0F'          TURN OFF FLAGS                               
         LLH   R1,HALF                                                          
         ST    R1,RIXCIC                                                        
         SLL   R1,12                                                            
         ST    R1,RIXCIT                                                        
*                                                                               
         LLH   R1,CIFCITOT         PART1 TOTAL NUMBER OF CI'S                   
         S     R1,RIXCIC           SUBTRACT # OF CIS USED FOR INDEX             
         ST    R1,RI1CIC                                                        
         SLL   R1,12                                                            
         ST    R1,RI1CIT                                                        
*                                                                               
         LLH   R1,CJFCITOT         PART2 TOTAL NUMBER OF CI'S                   
         ST    R1,RI2CIC                                                        
         SLL   R1,12                                                            
         ST    R1,RI2CIT                                                        
*                                                                               
         LLH   R1,CIFTRKS          NUMBER OF TRACKS PER PART1 CI                
         ST    R1,RI1TPC                                                        
         SLL   R1,12                                                            
         ST    R1,RI1TPT                                                        
*                                                                               
         LLH   R1,CJFTRKS          NUMBER OF TRACKS PER PART2 CI                
         ST    R1,RI2TPC                                                        
         SLL   R1,12                                                            
         ST    R1,RI2TPT                                                        
*                                                                               
         LLH   R1,CIFFDTRK         PART1 STARTING TRACK                         
         SLL   R1,12                                                            
         OILL  GR1,X'0001'         R1                                           
         ST    R1,RIFTRK1                                                       
*                                                                               
         LLH   R1,CJFSTTRK         PART2 STARTING TRACK                         
         SLL   R1,12                                                            
         OILL  GR1,X'0001'         R1                                           
         ST    R1,RIFTRK2                                                       
*                                                                               
         LLH   R1,RIFTRK+2                                                      
         NILL  GR1,X'0FFF'         R1 ISOLATE THE BLOCK/RECORD                  
         MH    R1,CIFHIREC                                                      
         ST    R1,RIHIREC          HIGH BLOCK/RECORD NUMBER                     
*                                                                               
         J     EXITOK                                                           
         DROP  R4                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* ADD WRKZ WORKER FILE INFORMATION TO SHARED QUEUE                              
***********************************************************************         
ADDFINF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,FIWNDA           R4=A(PART1 NODE)                             
         USING SI1PARD,R4                                                       
*                                                                               
         L     R2,ACIREC                                                        
         USING W_RECD,R2                                                        
         MVC   SVINDEX(L'W_INDEX),W_INDEX                                       
*                                                                               
         CLI   W_STAT,0            IGNORE EMPTY/PURGED ENTRIES                  
         JE    EXITH               NO NEED TO ADD TO TREE                       
*                                                                               
         MVC   SI1NDX,W_INDEX      SET PART1 INDEX IN MEMORY                    
*                                                                               
         OC    W_CINEXT,W_CINEXT   IS THERE A NEXT CI                           
         JZ    EXITOK              NO: FINISH UP                                
*                                                                               
         LHI   R1,X'0001'                                                       
         ICM   R1,B'1110',W_CINEXT PICK UP NEXT CI IN CHAIN                     
         CL    R1,SIT2STT          CHECK PART2 HAS VALID DISK ADDRESS           
         JL    BADNXT              NO: CHOP IT OFF                              
         ST    R1,FIWCIA                                                        
*                                                                               
         GOTO1 =V(READCI)                                                       
         MVC   THISCI,FIWCIA       THIS CI UNDER INSPECTION                     
*                                                                               
         CLC   W_FILENO,SVINDEX+(W_FILENO-W_INDEX) FILE # MISMTCH, CHOP         
         JNE   BADFNO              INDEX KEY MISMATCH, CHOP IT                  
         CLC   W_KEY,SVINDEX       CHECK PART2 KEY IS SAME AS PART1 KEY         
         JNE   BADNDX              INDEX KEY MISMATCH, CHOP IT                  
*                                                                               
         BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE)                     
         L     R1,FIWNDA                                                        
         S     R1,FIWSHA           DISPLACEMENT FROM BEGINNING OF TABLE         
         ST    R1,SI1NXT           NEXT PART2                                   
*                                                                               
         L     R4,FIWNDA           R4=A(PART2 NODE)                             
         ST    R4,ACURPAR2         SAVE A(CURRENT PART2 NODE)                   
         USING SI2PARD,R4                                                       
         MVC   SI2NDX,W_INDEX      SET PART2 INDEX IN MEMORY                    
*                                                                               
         L     R1,ACURPAR1         CURRENT PART1                                
         S     R1,FIWSHA                                                        
         ST    R1,SI2PRV           FIRST PREVIOUS IS THE PART1                  
         B     AFWF060                                                          
*                                                                               
AFWF040  GOTO1 =V(READCI)                                                       
         MVC   THISCI,FIWCIA       THIS CI UNDER INSPECTION                     
*                                                                               
         L     R4,FIWNDA                                                        
         ST    R4,ACURPAR2         SAVE CURRENT PART2                           
*                                                                               
         CLC   W_FILENO,SVINDEX+(W_FILENO-W_INDEX) FILE # MISMTCH, CHOP         
         JNE   BADFNO              INDEX KEY MISMATCH, CHOP IT                  
         CLC   W_KEY,SVINDEX       CHECK PART2 KEY IS SAME AS PART1 KEY         
         JNE   BADNDX              INDEX KEY MISMATCH, CHOP IT                  
*                                                                               
         LHI   R1,X'0001'                                                       
         ICM   R1,B'1110',W_CIPREV POINTER TO PREVIOUS CI IN CHAIN              
         CL    R1,SIT2STT          VALID PART2 WITHIN RANGE?                    
         JL    BADPRE              NO: CHOP IT OFF                              
*                                                                               
         MVC   SI2NDX,W_INDEX      SET PART2 INDEX IN MEMORY                    
*                                                                               
         ST    R1,FIWCIA                                                        
         BRAS  RE,FIRCN                                                         
         L     R1,FIWNDA                                                        
         S     R1,FIWSHA           DISPLACEMENT FROM BEGINNING OF TABLE         
         ST    R1,SI2PRV           DISPLACEMENT TO PREVIOUS PART2               
*                                                                               
AFWF060  OC    W_CINEXT,W_CINEXT   IS THERE A NEXT CI?                          
         JZ    EXITOK              NO: FINISH UP                                
*                                                                               
         LHI   R1,X'0001'                                                       
         ICM   R1,B'1110',W_CINEXT PICK UP NEXT CI IN CHAIN                     
*                                                                               
         CL    R1,SIT2STT          VALID PART2 WITHIN RANGE?                    
         BL    BADNXT              NO: CHOP IT OFF                              
         ST    R1,FIWCIA                                                        
*                                                                               
         BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE)                     
         L     R1,FIWNDA                                                        
         S     R1,FIWSHA           DISPLACEMENT FROM BEGINNING OF TABLE         
         ST    R1,SI2NXT           DIPLACEMENT TO NEXT PART2                    
*                                                                               
         MVC   GOODCI,THISCI       CI IS ALL GOOD                               
         B     AFWF040                                                          
         DROP  R2,R4                                                            
*                                                                               
*======================================================================         
* BAD FILE INFORMATION - ATTEMPT TO REPAIR                                      
*======================================================================         
BADNDX   MVC   PLMSG,=CL20'*INDEX MISMATCH*'                                    
         J     BAD010                                                           
BADFNO   MVC   PLMSG,=CL20'*FILE # MISMATCH*'                                   
         J     BAD010                                                           
BADPRE   MVC   PLMSG,=CL20'*BAD PREV CI*'                                       
         J     BAD010                                                           
BADNXT   MVC   PLMSG,=CL20'*BAD NEXT CI*'                                       
         J     BAD020                                                           
*                                                                               
BAD010   MVC   APINDEX,0(R2)       BAD INDEX                                    
         BRAS  RE,PRINTNDX         PRINT BAD INDEX                              
*                                                                               
         MVC   FIWCIA,GOODCI       READ LAST GOOD CI                            
         GOTO1 =V(READCI)                                                       
*                                                                               
BAD020   MVC   APINDEX,0(R2)       INDEX OF LAST GOOD CI                        
                                                                                
         LLC   R1,RIDNEXT          DISPLACEMENT TO CI NEXT                      
         LA    R1,0(R1,R2)                                                      
         LLC   RF,RILNEXT          LENGTH OF CI NEXT                            
         AHI   RF,-1                                                            
         EXRL  RF,BAD022                                                        
         J     BAD024                                                           
BAD022   XC    0(0,R1),0(R1)       CHOP IT OFF                                  
*                                                                               
BAD024   BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE)                     
         L     R4,FIWNDA                                                        
*                                                                               
         BRAS  RE,FIRC1            IS THIS A PART1                              
         JNE   BAD030              NO                                           
                                                                                
         LLC   R1,RIDSEQ           DISPLACEMENT TO REPORT SEQUENCE #            
         LA    R1,0(R1,R2)                                                      
         MVI   0(R1),0             SET SEQUENCE TO INDICATE NO PART2S           
*                                                                               
         USING SI1PARD,R4                                                       
         XC    SI1NXT,SI1NXT       REMOVE LINK IN INDEX                         
         LLC   R1,RIDSEQ                                                        
         LA    R1,SI1NDX(R1)                                                    
         MVI   0(R1),0             SET SEQUENCE # IN INDEX                      
         J     BAD040                                                           
*                                                                               
         USING SI2PARD,R4                                                       
BAD030   XC    SI2NXT,SI2NXT       REMOVE LINK IN INDEX                         
*                                                                               
BAD040   GOTO1 =V(WRITECI)         WRITE BACK THE FIX                           
*                                                                               
         BRAS  RE,PRINTNDX         PRINT LAST GOOD INDEX                        
*                                                                               
         LA    R3,MSGW                                                          
         MVC   MSGWMSG,=CL40'*ERROR* REPORT FORMAT ERROR'                       
         MVC   MSGWDET(5),=C'FILE='                                             
         MVC   MSGWDET+5(L'FIWRES),FIWRES                                       
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     EXITOK                                                           
         DROP  R4                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* WRKF WORKER FILE - SCAN FOR EXPIRED AND VULNERABLE REPORTS                    
***********************************************************************         
         USING SI1PARD,R2                                                       
         USING W_RECD,R3                                                        
         USING RVTABD,R5                                                        
FILSCAN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
GSWZ020  ST    R2,FIWNDA           CURRENT PART1                                
         LA    R3,SI1NDX           R3= A(INDEX)                                 
         BRAS  RE,FIRNC            CONVERT A(INDEX NODE) TO A(CI) REF#          
*                                                                               
         LR    R1,R2                                                            
         S     R1,FIWSHA           DISPL TO CURRENT PART1 NODE                  
         C     R1,SITP1HD          IS THIS NODE NEXT AVAILABLE?                 
         BE    GSWZ200             YES: LEAVE IT                                
         OC    SI1NAV,SI1NAV       IS IT ALREADY AVAILABLE?                     
         BNZ   GSWZ200             YES: LEAVE IT                                
         CLI   W_STAT,W_STPU       ALREADY PURGED?                              
         BE    GSWZ200             YES: SKIP IT                                 
*                                                                               
         CLC   W_AGERD,TODAY       TEST RETAIN DATE WITH TODAY                  
         BL    GSWZ030             EXPIRED: PURGE THIS ONE                      
         BH    GSWZ040             NOT YET: KEEP IT                             
         CLI   W_AGERT,X'FE'       TEST TIME AVAILABLE                          
         BE    GSWZ040             NO: DON'T TOUCH IT                           
         CLC   W_AGERT,TIMENOWM    TEST RETAIN TIME VALUE                       
         BNL   GSWZ040             NOT TIME YET                                 
         B     GSWZ030                                                          
                                                                                
*----------------------------------------------------------------------         
* EXPIRED REPORTS                                                               
*----------------------------------------------------------------------         
GSWZ030  TM    W_STAT,W_STKE       ON KEEP?                                     
         BZ    GSWZ032             NO                                           
         CLC   W_AGERD,TWOWEEKS    DID IT EXPIRE OVER TWO WEEKS AGO?            
         BL    GSWZ150             YES: THEN GET RID OF IT ALREADY              
         B     GSWZ100             ELSE VULNERABLE                              
*                                                                               
GSWZ032  TM    W_STAT,W_STRUN      RUNNING - CREATION / SENDING                 
         BZ    GSWZ150             NO: PURGE                                    
         TM    W_STAT,W_STCRE      CREATION IN PROCESS                          
         BZ    GSWZ034             NO                                           
         CLC   W_AGELD,YESTRDAY    DID THIS START BEFORE YESTERDAY              
         BL    GSWZ150             YES: IT MUST BE BAD NOW, PURGE IT            
         B     GSWZ200             NO: LEAVE IT BE                              
GSWZ034  TM    W_STAT,W_STSEN      IS IT SENDING                                
         BZ    GSWZ200             NO                                           
         CLC   W_AGERD,YESTRDAY    DID IT EXPIRE BEFORE YESTERDAY               
         BL    GSWZ150             YES: WHAT'S TAKING SO LONG, PURGE            
         B     GSWZ200             KEEP IT, GET NEXT                            
                                                                                
*----------------------------------------------------------------------         
* NON-EXPIRED REPORTS                                                           
*----------------------------------------------------------------------         
GSWZ040  TM    W_STAT,W_STDEAD     PRINTED / SENT / DEAD?                       
         BNZ   GSWZ050             YES: THEN VULNERABLE                         
*                                                                               
         TM    W_STAT,W_STRUN      RUNNING - CREATION / SENDING                 
         BZ    GSWZ200             NO: GET NEXT                                 
         TM    W_STAT,W_STCRE      CREATION IN PROCESS                          
         BZ    GSWZ200             NO                                           
*        CLC   W_AGELD,YESTRDAY    WAS IT CREATED AT LEAST A DAY AGO?           
*        BL    GSWZ150             YES: IT MUST BE BAD NOW, PURGE IT            
         B     GSWZ200             GET NEXT REPORT                              
*                                                                               
GSWZ050  CLC   W_AGELD,TODAY       WAS IT CREATED BEFORE TODAY                  
         BL    GSWZ100             THEN WE CAN PURGE THIS VULNERABLE            
         BH    GSWZ200             CREATED IN THE FUTURE???? SURE...            
         CLC   W_AGELT,TIMEOLDC    WAS IT CREATED BEFORE THE LAST SCAN          
         BNL   GSWZ200             IF NOT, THEN DON'T PURGE IT                  
                                                                                
*----------------------------------------------------------------------         
* VULNERABLE REPORTS - SAVE FOR LATER                                           
*----------------------------------------------------------------------         
GSWZ100  XC    RVTABD(RVTABL),RVTABD                                            
*                                                                               
         CLI   W_SEQ,0             ANY PART2S?                                  
         BE    GSWZ130                                                          
         ICM   R6,15,SI1NXT        ANY PART2S?                                  
         BZ    GSWZ130                                                          
         XC    HALF,HALF                                                        
*                                                                               
         USING SI2PARD,R6                                                       
GSWZ110  A     R6,FIWSHA           GO TO LAST PART2 AND GET SEQUENCE #          
         ICM   R1,15,SI2NXT                                                     
         BZ    GSWZ120                                                          
         LR    R6,R1                                                            
         B     GSWZ110                                                          
*                                                                               
GSWZ120  LLC   R1,SI2NDX+(W_SEQ-W_INDEX)                                        
         TM    W_ATTB,W_ATXTN      DO WE HAVE EXTENSION CIS                     
         BZ    *+8                                                              
         AHI   R1,255              YES: THEN 255 REGULAR PART2S                 
         AHI   R1,-1               DECREMENT COUNT FOR THE PART1                
         STCM  R1,3,HALF           # OF PART2 CIS                               
         A     R1,CNTP2VU          UPDATE VULNERABLE COUNT                      
         ST    R1,CNTP2VU                                                       
         DROP  R6                                                               
*                                                                               
GSWZ130  L     R0,CNTP1VU          COUNT SCANNED VULNERABLE PART1S              
         AHI   R0,1                                                             
         ST    R0,CNTP1VU                                                       
*                                                                               
         CHI   R0,RVMAX            MORE THAN WE CAN HANDLE?                     
         BH    GSWZ200             DON'T ADD IT TO THE TABLE                    
*                                                                               
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         ICM   R0,3,W_AGELD                                                     
         ICM   R1,3,TODAY          LIVE DATE MINUS TODAY                        
         SR    R1,R0               WILL GIVE ME SOME NUMBER                     
         AHI   R1,1                ADD ONE TO THAT TO AVOID ZERO                
         STCM  R1,3,RVONE          WHATEVER THAT IS, SAVE IT                    
         MVC   RVTWO,HALF          SAVE # OF PART2 CIS                          
*                                                                               
         TM    MINDI,MIFREE2       DO WE ABSOLUTELY NEED PART2S?                
         BZ    GSWZ140             NO                                           
         MVC   RVONE,HALF          YES: SORT WITH SIZE FIRST                    
         STCM  R1,3,RVTWO                                                       
*                                                                               
GSWZ140  MVC   RVNDX,W_INDEX       FILE INDEX                                   
*                                                                               
         L     RF,ARVTAB           INCREMENT VULNERABLE TABLE COUNT             
         XR    R1,R1                                                            
         ICM   R1,3,0(RF)                                                       
         AHI   R1,1                                                             
         STCM  R1,3,0(RF)                                                       
*                                                                               
         LA    R5,RVTABL(,R5)                                                   
         B     GSWZ200             NEXT                                         
                                                                                
*----------------------------------------------------------------------         
* PURGE THE REPORT                                                              
*----------------------------------------------------------------------         
GSWZ150  BRAS  RE,FIRRLOCK         LOCK THE REPORT                              
         MVC   SVREF,FIWREF        SAVE THE REFERENCE FOR THE UNLOCK            
*                                                                               
         MVC   SVINDEX,W_INDEX                                                  
         MVC   UKINDEX,W_INDEX                                                  
         OI    UKFLAG,UKFLDAT      PASSING REF#                                 
*                                                                               
         SAM24                                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'INDEX',FIWRES,UKINDEX,ACTREC,ACIREC          
         CLI   DMCB+8,0                                                         
         JE    GSWZ152                                                          
         SAM31                                                                  
         MVC   FIWREF,SVREF        REPORT #                                     
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         BRAS  RE,FIRRC            REPORT TO A(CI)                              
         MVC   FIWNDX,UKINDEX      INDEX                                        
         GOTO1 =V(BADREP),2        OUTPUT BAD REPORT                            
         B     GSWZ200                                                          
*                                                                               
GSWZ152  SAM31                                                                  
         CLC   UKINDEX,SVINDEX     MAKE SURE IT'S THE SAME REPORT               
         BE    GSWZ160             . NO, LEAVE IT ALONE                         
         MVC   FIWREF,SVREF        RESTORE REFERENCE NUMBER AND UNLOCK          
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         B     GSWZ200                                                          
*                                                                               
GSWZ160  CLI   WRTFIL,C'Y'                                                      
         BE    GSWZ170                                                          
         MVC   PLMSG,=CL20'*PURGE (WRITE=NO)*'                                  
         MVC   FIWREF,SVREF        RESTORE REFERENCE NUMBER AND                 
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         B     GSWZ180                                                          
*                                                                               
GSWZ170  SAM24                                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'PURGE',FIWRES,UKINDEX,ACTREC,ACIREC          
         CLI   DMCB+8,0                                                         
         JE    GSWZ175             PURGE WILL UNLOCK THE REPORT                 
         SAM31                                                                  
         MVC   FIWREF,SVREF        REPORT #                                     
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         BRAS  RE,FIRRC            REPORT TO A(CI)                              
         MVC   FIWNDX,UKINDEX      INDEX                                        
         GOTO1 =V(BADREP),3        OUTPUT BAD REPORT                            
         B     GSWZ200                                                          
*                                                                               
GSWZ175  SAM31                                                                  
         MVC   PLMSG,=CL20'*REPORT PURGE*'                                      
*                                                                               
GSWZ180  OI    SINDI,SIPURGE                                                    
         BRAS  RE,PRINTNDX                                                      
*                                                                               
GSWZ200  LA    R2,L'SI1PAR(,R2)    GET NEXT INDEX                               
         BCT   R4,GSWZ020                                                       
                                                                                
*----------------------------------------------------------------------         
* FINISHED SCAN                                                                 
*----------------------------------------------------------------------         
         BRAS  RE,PUVU             PURGE VULNERABLES IF NECESSARY               
*                                                                               
GSWZX    J     EXITOK                                                           
         DROP  R2,R3                                                            
         LTORG                                                                  
                                                                                
***********************************************************************         
* PURGE VULNERABLE REPORTS IF NECESSARY                                         
***********************************************************************         
PUVU     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ARVTAB           VULNERABLE REPORT TABLE                      
         XR    R4,R4                                                            
         ICM   R4,3,0(R5)          ANY VULNERABLES FOR THIS RESOURCE?           
         BZ    PUVUX               NO                                           
*                                                                               
         LA    R5,2(,R5)                                                        
         USING RVTABD,R5                                                        
*                                  SORT IN DESCENDING ORDER                     
         SAM24                                                                  
         GOTO1 =V(QSORT),DMCB,(1,(R5)),(R4),RVTABL,5,0,0                        
         SAM31                                                                  
*                                                                               
PUVU010  GOTO1 =V(PACALC)          CALCULATE % AVAILABLE                        
*                                                                               
         L     R1,PAPART1          # OF AVAILABLE PART1S                        
         CHI   R1,500              LESS THEN 5.00% AVAILABLE?                   
         BL    PUVU020             YES: PURGE VULNERABLE                        
         L     R1,PAPART2          # OF AVAILABLE PART2S                        
         CHI   R1,1500             LESS THEN 15.00% AVAILABLE?                  
         BL    PUVU020             YES: PURGE VULNERABLE                        
         B     PUVUX                                                            
                                                                                
*----------------------------------------------------------------------         
* PURGE THE REPORT                                                              
*----------------------------------------------------------------------         
PUVU020  XC    FIWREF,FIWREF                                                    
         MVC   FIWREF+2(2),RVNDX+(W_FILENO-W_INDEX)                             
         OC    FIWREF,FIWREF                                                    
         BZ    PUVU080                                                          
*                                                                               
         BRAS  RE,FIRRLOCK         LOCK THE REPORT                              
         MVC   SVREF,FIWREF        SAVE THE REFERENCE NUMBER FOR UNLOCK         
*                                                                               
         MVC   UKINDEX,RVNDX                                                    
         OI    UKFLAG,UKFLDAT      PASSING REF#                                 
*                                                                               
         SAM24                                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'INDEX',FIWRES,UKINDEX,ACTREC,ACIREC          
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
         SAM31                                                                  
*                                                                               
         CLC   UKINDEX,RVNDX       MAKE SURE SAME INDEX                         
         BE    PUVU040             YES: PURGE                                   
         MVC   FIWREF,SVREF        NO: RESTORE REFERENCE NUMBER                 
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         B     PUVU080                                                          
*                                                                               
PUVU040  CLI   WRTFIL,C'Y'                                                      
         BE    PUVU050                                                          
         MVC   PLMSG,=CL20'*VLNRBL(WRITE=NO)*'                                  
         MVC   FIWREF,SVREF        RESTORE REFERENCE NUMBER                     
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         B     PUVU060                                                          
*                                                                               
PUVU050  SAM24                                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'PURGE',FIWRES,UKINDEX,ACTREC,ACIREC          
         CLI   DMCB+8,0                                                         
         JNE   *+2                 PURGE WILL UNLOCK THE REPORT                 
         MVC   PLMSG,=CL20'*VULNERABLE PURGE*'                                  
         SAM31                                                                  
*                                                                               
PUVU060  L     R0,CNTP1VU          DECREMENT VULNERABLE COUNT                   
         AHI   R0,-1                                                            
         ST    R0,CNTP1VU                                                       
*                                                                               
         L     R0,CNTP2VU          REDUCE PART2 VULNERABLE COUNT                
         XR    R1,R1                                                            
         ICM   R1,3,RVTWO                                                       
         TM    MINDI,MIFREE2       DO WE ABSOLUTELY NEED PART2S?                
         BZ    *+8                 NO                                           
         ICM   R1,3,RVONE                                                       
         SR    R0,R1                                                            
         ST    R0,CNTP2VU                                                       
*                                                                               
         OI    SINDI,SIPURGE                                                    
         BRAS  RE,PRINTNDX                                                      
*                                                                               
PUVU080  LA    R5,RVTABL(,R5)      GET NEXT INDEX FROM VULNERABLE TABLE         
         BCT   R4,PUVU010                                                       
*                                                                               
PUVUX    J     EXITOK                                                           
         DROP  R5                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* PRINT WRKF WORKER FILE INDEX                                                  
***********************************************************************         
PRINTNDX NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SAM24                                                                  
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,UKUSRID                                                  
         GOTO1 =V(DATAMGR),DMCB,=CL7'DMREAD',=CL7'CTFILE',IOKEY,AIOAREA         
         BNE   PXWF020                                                          
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CTIDATA                                                       
PXWF010  CLI   0(R3),0                                                          
         BE    PXWF020                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    PXWF012                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PXWF010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
PXWF012  MVC   PLUSER,CTDSC                                                     
         B     PXWF022                                                          
         DROP  R2,R3                                                            
*                                                                               
PXWF020  GOTO1 =V(HEXOUT),DMCB,UKUSRID,PLUSER,L'UKUSRID,0                       
PXWF022  MVC   PLSYSP,UKSYSPRG                                                  
         GOTO1 =V(HEXOUT),DMCB,UKDAY,PLDAY,L'UKDAY,0                            
         MVC   PLCLASS,UKCLASS                                                  
         MVC   FULL,UKFILENO                                                    
         EDIT  (B4,FULL),(5,PLREFNO)                                            
         GOTO1 =V(HEXOUT),DMCB,UKTYPE,PLTYPE,L'UKTYPE,0                         
         GOTO1 =V(HEXOUT),DMCB,UKATTB,PLATTB,L'UKATTB,0                         
         GOTO1 =V(HEXOUT),DMCB,UKSTAT,PLSTAT,L'UKSTAT,0                         
         EDIT  (B1,UKSEQ),(3,PLSEQ)                                             
         GOTO1 =V(DATCON),DMCB,(2,UKAGERD),(21,PLAGE)                           
*                                                                               
         XR    R0,R0               RETAIN TIME                                  
         LLC   R1,UKAGERD+L'UKAGERD                                             
         MHI   R1,10               CONVERT 10 MIN INCREMENTS                    
         D     R0,=F'60'                                                        
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         GOTO1 =V(TIMEOUT)                                                      
         MVC   PLAGET,DUB+2                                                     
*                                                                               
         MVC   FULL,FIWCIA                                                      
         GOTO1 =V(HEXOUT),DMCB,FULL,PLCIA,L'FIWCIA,0                            
*                                                                               
         GOTO1 =V(PRINTL)                                                       
         J     EXITOK                                                           
         LTORG                                                                  
                                                                                
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITL    LHI   RF,0                                                             
         J     EXITCC                                                           
EXITH    LHI   RF,2                                                             
         J     EXITCC                                                           
EXITOK   LHI   RF,1                                                             
EXITCC   CHI   RF,1                                                             
EXIT     XIT1  ,                                                                
                                                                                
***********************************************************************         
* SHARED MEMORY FILE INDEX ROUTINES                                             
***********************************************************************         
         DROP  R8,R9               DROP SIHDRD AND SITABD                       
       ++INCLUDE DDSHFIR                                                        
         LTORG                                                                  
                                                                                
***********************************************************************         
* WORKING STORAGE DC                                                            
***********************************************************************         
         DS    0D                                                               
WORKAREA DC    60000D'00'                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* WORKER FILE DSECTS                                                            
***********************************************************************         
       ++INCLUDE DMWRKZD                                                        
       ++INCLUDE DMWRKZK                                                        
       ++INCLUDE DMWRKZX                                                        
                                                                                
***********************************************************************         
* WORKING STORAGE AND SHARED DSECTS                                             
***********************************************************************         
       ++INCLUDE DDSHFIWRK                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005WZSHFIMON 10/02/13'                                      
         END                                                                    
