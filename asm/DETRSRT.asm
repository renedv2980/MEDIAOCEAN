*          DATA SET DETRSRT    AT LEVEL 006 AS OF 03/06/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DETRSRTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DDINFO                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE SMTP                                                                   
*INCLUDE KHDUMMY                                                                
         TITLE 'PRE-PROCESSOR FOR TRITON DATA'                                  
**********************************************************************          
* THIS MODULE WAS COPIED FROM THE ARBITRON PRE-PROCESSOR. SOME CODE             
* MAY BE SPECIFIC TO ARBITRON.                                                  
**********************************************************************          
DETRSRT  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,DETRSRT,=V(REGSAVE),RA                                         
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         MVC   TITLE(20),=C'TRITON PRE-PROCESSOR'                               
         MVC   P(16),=C'PARAMETER CARDS:'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CARDS10  GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(L'CARD),CARD      PRINT EACH PARAMETER CARD                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   =C'/*',CARD                                                      
         BE    CHKDUMP             NO MORE PARAMETER CARDS                      
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    CARDS10             YES                                          
*                                                                               
         CLC   =C'DDSIO=',CARD     DDSIO?                                       
         BNE   CARDS20                                                          
         ICM   RF,15,=V(DDSIO)                                                  
         BNZ   *+6                                                              
         DC    H'0'                V(DDSIO) UNAVAILABLE ?!?                     
         MVC   0(8,RF),CARD+6      OVERRIDE DDSIO PHASE                         
         B     CARDS10                                                          
*                                                                               
CARDS20  CLC   =C'DEMTABS=',CARD   DEMTABS?                                     
         BNE   CARDS25                                                          
         MVC   CDEMTABS,CARD+8     OVERRIDE DEMTABS PHASE                       
         B     CARDS10                                                          
*                                                                               
CARDS25  CLC   =C'DSPACE=',CARD    DSPACE= CONTROL CARD?                        
         BNE   CARDS30                                                          
         LAY   RF,SSB              YES                                          
         MVC   SSODSPAC-SSOOFF(,RF),CARD+7    DSPACE IDENTIFIER                 
         B     CARDS10                                                          
*                                                                               
CARDS30  CLC   =C'SENDTO=',CARD                                                 
         BNE   CARDS40                                                          
         MVC   SENDTO,CARD+7                                                    
         B     CARDS10                                                          
*                                                                               
CARDS40  CLC   =C'DUMP=OS',CARD                                                 
         BE    *+6                                                              
         DC    H'0'                INVALID PARAMETER CARD                       
         MVI   DUMPFLAG,C'O'       PRODUCE FULL MVS DUMP                        
         B     CARDS10                                                          
*                                                                               
CHKDUMP  DS    0H                                                               
         CLI   DUMPFLAG,C'O'                                                    
         BE    LOADTAB                                                          
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
*                                                                               
LOADTAB  DS    0H                  LOAD DEMTABS                                 
         GOTO1 =V(PRINTER)         PRINT A BLANK LINE                           
*                                                                               
         MVC   DUB,CDEMTABS                                                     
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMTABS,4(R1)                                                   
*                                                                               
         LHI   RE,L'SORTKEY                                                     
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCRDL,DUB                                                     
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'A',=C'FILIN1  ')                            
         CLI   DMCB+4,0            IS FILIN1 ALLOCATED VIA DD STMT?             
         BE    *+12                NO                                           
         MVI   BYTE,C'Y'           YES: WE HAVE SOMETHING TO CONVERT            
         B     OPENFILS                                                         
*                                                                               
* IF WE GET HERE, THEN THERE IS NO FILIN1 DD STATEMENT IN THE JCL.              
* THAT MEANS THAT WE WANT TO DYNAMICALLY ALLOCATE AND CONCATENATE THE           
* ENTIRE CONTENTS OF AN NFS-MOUNTED FOLDER. THE "DDNFSNAMES" PROGRAM            
* MUST BE RUN IN A JOB STEP BEFORE THIS ONE, PRODUCING A TEMPORARY              
* DATASET WITH DDNAME NFSFILES. THAT DATASET WILL CONTAIN THE NAMES OF          
* ALL OF THE FILES IN THE MOUNTED FOLDER.                                       
*                                                                               
         MVI   BYTE,C'N'           ASSUME MOUNTED FOLDER IS EMPTY               
         OPEN  NFSFILES            CONTAINS FILENAMES IN MOUNTED FOLDER         
*                                                                               
         GET   NFSFILES,IOAREA     GET THE FIRST FILENAME                       
         SR    R2,R2                                                            
         ICM   R2,3,IOAREA         RECLEN (FROM RDW)                            
         SHI   R2,4                L'RDW                                        
*                                                                               
         LR    RF,R2               L'PATHNAME                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),IOAREA+4       PRINT THE PATHNAME                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'H',=C'FILIN1  '),((R2),IOAREA+4),  +        
               (X'50',FILIN1)                                                   
         TM    DMCB+8,X'20'        DYNAMIC ALLOCATION FAILED?                   
         BZ    *+6                                                              
         DC    H'0'                YES                                          
         MVI   BYTE,C'Y'           WE HAVE AT LEAST ONE FILE TO CONVERT         
*                                                                               
****** BART SAYS THIS MAY GO UP TO 5385 IN THE FUTURE                           
         MVC   CNTSA1S,=CL80'COUNT FROM(FILIN1) NOTEQUAL(2705) RC8'             
         LA    R1,ICEPAR1          COUNT RECORDS IN TRITON INPUT FILE           
         LINK  EP=ICETOOL                                                       
         LTR   RF,RF               CORRECT NUMBER OF RECORDS COUNTED?           
         BZ    *+6                 YES                                          
         DC    H'0'                NO: IF RC=8, BAD RECORD COUNT.               
*                                      IF RC IS ANYTHING ELSE, DEATH            
*                                                                               
NEXTFILE DS    0H                                                               
         GET   NFSFILES,IOAREA     GET THE NEXT FILENAME                        
         SR    R2,R2                                                            
         ICM   R2,3,IOAREA         RECLEN (FROM RDW)                            
         SHI   R2,4                L'RDW                                        
*                                                                               
         LR    RF,R2               L'PATHNAME                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),IOAREA+4       PRINT THE PATHNAME                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'H',=C'FILIN1T '),((R2),IOAREA+4),  +        
               (X'50',FILIN1)                                                   
         TM    DMCB+8,X'20'        DYNAMIC ALLOCATION FAILED?                   
         BZ    *+6                                                              
         DC    H'0'                YES                                          
*                                                                               
****** BART SAYS THIS MAY GO UP TO 5385 IN THE FUTURE                           
         MVC   CNTSA1S,=CL80'COUNT FROM(FILIN1T) NOTEQUAL(2705) RC8'            
         LA    R1,ICEPAR1          COUNT RECORDS IN TRITON INPUT FILE           
         LINK  EP=ICETOOL                                                       
         LTR   RF,RF               CORRECT NUMBER OF RECORDS COUNTED?           
         BZ    *+6                 YES                                          
         DC    H'0'                NO: IF RC=8, BAD RECORD COUNT.               
*                                      IF RC IS ANYTHING ELSE, DEATH            
*                                                                               
*                                  DYNAMIC CONCATENATION                        
         GOTO1 =V(DYNALLOC),DMCB,(C'C',=C'FILIN1  '),=C'FILIN1T '               
         B     NEXTFILE            ALLOCATE & CONCATENATE THE NEXT FILE         
*                                                                               
CLOSNAMS DS    0H                                                               
         CLOSE NFSFILES                                                         
*                                                                               
OPENFILS DS    0H                                                               
         CLI   BYTE,C'Y'           IS THERE ANY DATA TO CONVERT?                
         BNE   XBASE               NO                                           
*                                                                               
         OPEN  (FILIN1,(INPUT))                                                 
         OPEN  (FILOUT,(OUTPUT))                                                
         GOTO1 =V(SORTER),DMCB,SORTCRD,(X'80',RECCRD),(X'80',0)                 
*                                                                               
         LAY   RE,VALTAB                                                        
         LHI   RF,L'VALTAB                                                      
         XCEFL                                                                  
         LA    RE,TEMPREC                                                       
         LHI   RF,L'TEMPREC                                                     
         XCEFL                                                                  
         XC    INCNT,INCNT                                                      
         XC    OUTCNT,OUTCNT                                                    
         XC    PREVKEY,PREVKEY                                                  
         MVI   FRSTFLAG,1                                                       
         MVI   GETFLAG,0                                                        
*                                                                               
COMB10   GET   FILIN1,IOAREA                                                    
         CLI   IOAREA+4,C'A'       IF NEXT RECORD IS A.                         
         BNE   COMB20                                                           
         CLI   FRSTFLAG,1                                                       
         BNE   COMB25                                                           
         MVI   FRSTFLAG,0                                                       
*                                                                               
COMB20   L     RE,INCNT            COUNT THE INPUT RECORDS                      
         LA    RE,1(RE)                                                         
         ST    RE,INCNT                                                         
         LA    R2,IOAREA+4                                                      
*                                                                               
         BAS   RE,PROCREC                                                       
         B     COMB10                                                           
*                                                                               
         EJECT                                                                  
*OMB25   CLOSE (FILIN1,)                                                        
COMB25   MVI   GETFLAG,C'G'        FOR "GET"                                    
         BAS   RE,PROC_V                                                        
         CLI   IOAREA+4,C'A'                                                    
         BNE   DONE                                                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCRD,(X'80',RECCRD),(X'80',0)                 
         B     COMB20                                                           
*                                                                               
DONE     CLOSE (FILIN1,)                                                        
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         GOTO1 =V(PRINTER)         BLANK LINE                                   
         MVC   P(23),=C'TOTAL RECORDS OUTPUT = '                                
         EDIT  OUTCNT,(8,P+25)                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
DONE10   CLOSE FILOUT                                                           
*                                                                               
XBASE    DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
*                                                                               
* I/O ERROR HANDLER                                                             
*                                                                               
IOERR    DS    0H                                                               
*                                                                               
* EXTRACT SYSTEM ERROR MESSAGES INTO FIELD WORK. SEE IBM MANUAL                 
* "Z/OS DFSMS MACRO INSTRUCTIONS FOR DATA SETS", SECTION ON SYNADAF             
* MACRO, FOR DETAILS. IF WE HAVE A DDNAME, TRY TO EXTRACT A DSN OR              
* PATHNAME AND DISPLAY IT ALONG WITH THE SYNAD MESSAGES.                        
*                                                                               
         SYNADAF ACSMETH=QSAM                                                   
         MVC   WORK,50(R1)         MESSAGE AREA FROM SYNADAF                    
         SYNADRLS                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'I/O ERROR: FORCING USER AB+        
               END.'                                                            
*                                                                               
         CLC   WORK+25(8),SPACES   DO WE HAVE A DDNAME?                         
         BE    IOERR20             NO                                           
*                                  YES: TRY TO EXTRACT DSN                      
         GOTO1 =V(DDINFO),DMCB,(8,WORK+25),(0,=AL2(DINRTDSN)),0                 
         LTR   RF,RF                                                            
         BNZ   IOERR20             BAD RETURN FROM DDINFO                       
         SR    R2,R2                                                            
         ICM   R2,1,DMCB+8                                                      
         BZ    IOERR20             NO DSN AVAILABLE                             
         L     RE,DMCB+8           A(RETURNED DSN OR PATHNAME)                  
         CLC   =C'...PATH=.SPECIFIED...',0(RE)                                  
         BNE   IOERR10             IT'S NOT A PATHNAME                          
*                                                                               
*                                  TRY TO EXTRACT PATHNAME                      
         GOTO1 =V(DDINFO),DMCB,(8,WORK+25),(0,=AL2(DINRPATH)),0                 
         LTR   RF,RF                                                            
         BNZ   IOERR20             BAD RETURN FROM DDINFO                       
         SR    R2,R2                                                            
         ICM   R2,1,DMCB+8                                                      
         BZ    IOERR20             NO PATHNAME RETURNED                         
         L     RE,DMCB+8           A(RETURNED DSN OR PATHNAME)                  
*                                                                               
IOERR10  DS    0H                                                               
         MVC   OPERMSG(21),=C'FAILURE READING FROM '                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   OPERMSG+21(0),0(RE) PUT PATHNAME INTO CONSOLE MESSAGE            
         AHI   R2,1+21             R2 = L'MESSAGE                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',((R2),OPERMSG)                        
*                                                                               
IOERR20  DS    0H                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'SYNAD ERROR MESSAGES FOLLO+        
               W:'                                                              
         MVC   OPERMSG,SPACES      BUILD FIRST MESSAGE LINE...                  
         MVC   OPERMSG(59),WORK+18 STARTING AFTER <JOBNAME,STEPNAME,>           
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(59,OPERMSG)                          
         CLI   WORK+77,C'S'        IS THERE A 2ND MESSAGE?                      
         BNE   IOERRXIT                                                         
         MVC   OPERMSG,SPACES      YES: BUILD IT                                
         MVC   OPERMSG,WORK+94                                                  
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPERMSG,OPERMSG)                   
*                                                                               
IOERRXIT DS    0H                                                               
         ABEND 925                                                              
         EJECT                                                                  
***********************************************************************         
*PROCREC -  PROCESS RECORDS                                                     
***********************************************************************         
PROCREC  NTR1                      PROCESS AND COMBINE THE RECORDS              
*                                                                               
         CLI   0(R2),C'A'                                                       
         BNE   *+12                                                             
         BAS   RE,PROC_A                                                        
         B     PROCX                                                            
         CLI   0(R2),C'D'                                                       
         BNE   *+12                                                             
         BAS   RE,PROC_D                                                        
         B     PROCX                                                            
         CLI   0(R2),C'G'                                                       
         BNE   *+12                                                             
         BAS   RE,PROC_G                                                        
         B     PROCX                                                            
         CLI   0(R2),C'H'                                                       
         BNE   *+12                                                             
         BAS   RE,PROC_H                                                        
         B     PROCX                                                            
         CLI   0(R2),C'J'                                                       
         BNE   *+12                                                             
         BAS   RE,PROC_J                                                        
         B     PROCX                                                            
         CLI   0(R2),C'M'                                                       
         BNE   *+12                                                             
         BAS   RE,PROC_M                                                        
         B     PROCX                                                            
         CLI   0(R2),C'P'                                                       
         BNE   *+12                                                             
         BAS   RE,PROC_P                                                        
         B     PROCX                                                            
         CLI   0(R2),C'S'                                                       
         BNE   *+12                                                             
         BAS   RE,PROC_S                                                        
         B     PROCX                                                            
         CLI   0(R2),C'V'                                                       
         BNE   *+12                                                             
         BAS   RE,PROC_V                                                        
         B     PROCX                                                            
*                                                                               
PROCX    DS    0H                                                               
EXIT     XIT1                                                                   
* ****************************************************************              
* PROC_A  -    COPYRIGHT RECORD                                                 
* ****************************************************************              
PROC_A   NTR1                                                                   
*                                                                               
         LR    R6,R2               COPY IT AS IT IS                             
         BAS   RE,PROCESS                                                       
         LA    RE,IOAREA                                                        
         LHI   RF,L'IOAREA                                                      
         XCEFL                                                                  
*                                                                               
         B     EXIT                                                             
* ****************************************************************              
* PROC_D  -    ARUDIENCE CHARACTERISTIC RECORD                                  
* R2: POINTED AT INPUT RECORD                                                   
* R3: POINTED AT THE TABLE                                                      
* R6: POINTED AT OUTPUT RECORD                                                  
* ****************************************************************              
PROC_D   NTR1                                                                   
*                                                                               
         USING RDDSECT,R2                                                       
         LA    R6,TEMPREC+4                                                     
         USING PDDSECT,R6                                                       
         LAY   R3,CHARTAB                                                       
         USING CHARTABD,R3                                                      
         MVC   TEMPID,RDACHID                                                   
         MVC   PDRECID(PDQULCD-PDRECID+10),RDRECID                              
         LA    RE,PDPPQCN                                                       
         SHI   RE,1                                                             
         MVI   0(RE),C':'                                                       
         AHI   RE,3                                                             
         MVI   0(RE),C','                                                       
*                                                                               
D10      CLC   TEMPID,CHID                                                      
         BE    D20                                                              
         AHI   R3,L'CHARTAB                                                     
         B     D10                                                              
D20      MVC   PDPPQCN,CHQCODE                                                  
         MVC   PDPPDMN,CHDNUM                                                   
         DROP  R3                                                               
*                                                                               
         XC    TEMPREC(4),TEMPREC                                               
         MVC   TEMPREC(2),=AL2(DRECLEN)                                         
         BAS   RE,PROCESS                                                       
*                                                                               
         B     EXIT                                                             
* ****************************************************************              
* PROC_G  -    INTAB RECORD                                                     
* R2: POINTED AT INPUT RECORD                                                   
* R6: POINTED AT OUTPUT RECORD                                                  
* ****************************************************************              
PROC_G   NTR1                                                                   
*                                                                               
         LA    R6,TEMPREC+4                                                     
         USING PGDSECT,R6                                                       
         CLI   GETFLAG,C'G'        "G"ET THE LAST RECORD PUT OUT                
         BE    G08                                                              
*                                                                               
         USING RGDSECT,R2                                                       
         CLC   PREVKEY(RGACHID-RGRECID),RGRECID    SAME KEY?                    
         BE    G20                                                              
         CLI   PREVKEY,C'G'        WAS PREVIOUS RECORD A GREC?                  
         BNE   G15                                                              
*                                                                               
G08      LA    R3,1                                                             
         LAY   R4,VALTAB                                                        
G10      DS    0H                                                               
         OC    0(CELROWQ,R4),0(R4) IS TABLE EMPTY FOR THIS ROW?                 
         BZ    G12                 IF SO, SKIP TO NEXT ROW                      
         MVC   PGRECID(PGPPQCN-PGRECID),PREVKEY                                 
         EDIT  (R3),(2,PGPPQCN),COMMAS=YES,ALIGN=RIGHT,FILL=0                   
         MVI   PGSPCR1,C':'                                                     
         MVC   PGVAL(CELROWQ),0(R4)                                             
         XC    TEMPREC(4),TEMPREC                                               
         MVC   TEMPREC(2),=AL2(GRECLEN)                                         
         BAS   RE,PROCESS                                                       
G12      AHI   R4,CELROWQ                                                       
         AHI   R3,1                                                             
         CHI   R3,CELGRPQ                                                       
         BNH   G10                                                              
*                                                                               
         LAY   RE,VALTAB           WHEN ALL ROWS ARE PUT OUT                    
         LHI   RF,L'VALTAB         CLEAR TABLE FOR FUTURE USE                   
         XCEFL                                                                  
         CLI   GETFLAG,C'G'                                                     
         BNE   G15                                                              
         MVI   GETFLAG,0                                                        
         B     EXIT                                                             
*                                                                               
G15      XC    PREVKEY,PREVKEY                                                  
         MVC   PREVKEY(RGACHID-RGRECID),RGRECID    SAVE KEY                     
*                                                                               
G20      MVC   TEMPID,RGACHID                                                   
         BAS   RE,PROCPTR                                                       
         L     R1,VALPTR                                                        
         OC    RGVAL,=C'000000'                                                 
         MVC   0(CELLLNQ,R1),RGVAL                                              
         B     EXIT                                                             
* ****************************************************************              
* PROC_H  -    POPULATION RECORD                                                
* R2: POINTED AT INPUT RECORD                                                   
* R6: POINTED AT OUTPUT RECORD                                                  
* ****************************************************************              
PROC_H   NTR1                                                                   
*                                                                               
         LA    R6,TEMPREC+4                                                     
         USING PHDSECT,R6                                                       
         CLI   GETFLAG,C'G'        "G"ET THE LAST RECORD PUT OUT                
         BE    H08                                                              
*                                                                               
         USING RHDSECT,R2                                                       
         CLC   PREVKEY(RHACHID-RHRECID),RHRECID    SAME KEY?                    
         BE    H20                                                              
         CLI   PREVKEY,C'H'        WAS PREVIOUS RECORD A HREC?                  
         BE    *+16                                                             
         MVI   GETFLAG,C'G'                                                     
         BAS   RE,PROC_G                                                        
         B     H15                                                              
*                                                                               
H08      LA    R3,1                                                             
         LAY   R4,VALTAB                                                        
         OC    0(CELROWQ,R4),0(R4)                                              
         BZ    H12                                                              
H10      DS    0H                                                               
         MVC   PHRECID(PHPPQCN-PHRECID),PREVKEY                                 
         EDIT  (R3),(2,PHPPQCN),COMMAS=YES,ALIGN=RIGHT,FILL=0                   
         MVI   PHSPCR1,C':'                                                     
         MVC   PHWTPOP(CELROWQ),0(R4)                                           
         XC    TEMPREC(4),TEMPREC                                               
         MVC   TEMPREC(2),=AL2(HRECLEN)                                         
         BAS   RE,PROCESS                                                       
H12      AHI   R4,CELROWQ                                                       
         AHI   R3,1                                                             
         CHI   R3,CELGRPQ                                                       
         BNH   H10                                                              
*                                                                               
         LAY   RE,VALTAB                                                        
         LHI   RF,L'VALTAB                                                      
         XCEFL                                                                  
         CLI   GETFLAG,C'G'                                                     
         BNE   H15                                                              
         MVI   GETFLAG,0                                                        
         B     EXIT                                                             
*                                                                               
H15      XC    PREVKEY,PREVKEY                                                  
         MVC   PREVKEY(RHACHID-RHRECID),RHRECID    SAVE KEY                     
*                                                                               
H20      MVC   TEMPID,RHACHID                                                   
         BAS   RE,PROCPTR                                                       
         L     R1,VALPTR                                                        
         OC    RHWTPOP,=C'000000'                                               
         MVC   0(CELLLNQ,R1),RHWTPOP                                            
         B     EXIT                                                             
* ****************************************************************              
* PROC_J  -    STATION COMBO RECORD                                             
* ****************************************************************              
PROC_J   NTR1                                                                   
*                                                                               
         CLI   PREVKEY,C'H'                                                     
         BNE   PJ00                                                             
         MVI   DIGITAB,X'FF'                                                    
         MVI   GETFLAG,C'G'                                                     
         BAS   RE,PROC_H                                                        
         XC    PREVKEY,PREVKEY                                                  
*                                                                               
         USING RJDSECT,R2                                                       
*                                                                               
PJ00     CLC   RJSCMBID,=C'999999' LAST J RECORD? "PUMM"                        
         BNE   PJ01                                                             
*                                                                               
         LA    R1,DIGITAB                                                       
         CLI   0(R1),X'FF'                                                      
         BE    *+8                                                              
         BAS   RE,PRINTDIG                                                      
*                                                                               
PJ01     CLC   RJBAND,=C'SM'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PJ30     LR    R6,R2               COPY IT AS IT IS                             
         BAS   RE,PROCESS                                                       
         LA    RE,IOAREA                                                        
         LHI   RF,L'IOAREA                                                      
         XCEFL                                                                  
*                                                                               
PJX      B     EXIT                                                             
*                                                                               
* PROC_M  -    STATION WITHIN COMBO RECORD                                      
* ****************************************************************              
PROC_M   NTR1                                                                   
*                                                                               
         LR    R6,R2               COPY IT AS IT IS                             
         BAS   RE,PROCESS                                                       
         LA    RE,IOAREA                                                        
         LHI   RF,L'IOAREA                                                      
         XCEFL                                                                  
*                                                                               
         B     EXIT                                                             
* ****************************************************************              
* PROC_P  -    NETWORK AFFILIATION RECORD                                       
* ****************************************************************              
PROC_P   NTR1                                                                   
*                                                                               
         USING RPDSECT,R2                                                       
         LA    R1,DIGITAB                                                       
PP05     CLI   0(R1),X'FF'                                                      
         BE    PP10                                                             
*                                                                               
         CLC   RPSCMBID,0(R1)                                                   
         BE    PPX                                                              
         LA    R1,L'DIGITAB(R1)                                                 
         B     PP05                                                             
*                                                                               
PP10     LR    R6,R2               COPY IT AS IT IS                             
         BAS   RE,PROCESS                                                       
         LA    RE,IOAREA                                                        
         LHI   RF,L'IOAREA                                                      
         XCEFL                                                                  
*                                                                               
PPX      B     EXIT                                                             
* ****************************************************************              
* PROC_S  -    DAYPART RECORD                                                   
* ****************************************************************              
PROC_S   NTR1                                                                   
*                                                                               
         LR    R6,R2               COPY IT AS IT IS                             
         BAS   RE,PROCESS                                                       
         LA    RE,IOAREA                                                        
         LHI   RF,L'IOAREA                                                      
         XCEFL                                                                  
*                                                                               
         B     EXIT                                                             
* ****************************************************************              
* PROC_V  -    AUDIENCE ESTIMATE RECORD                                         
*     RECORDS ARE THROWN INTO SORTER TO SORT AUDCHAR BEFORE                     
*     STATION COMBO ID AND NAME.  THEN PROCEED AS NORMAL SIMILAR                
*     TO GRECS AND HRECS INPUTTING FROM SORTER(GET).                            
*                                                                               
* R2: POINTED AT INPUT RECORD                                                   
* R6: POINTED AT OUTPUT RECORD                                                  
* ****************************************************************              
PROC_V   NTR1                                                                   
*                                                                               
         CLI   GETFLAG,C'G'        PROCESS SORTED V REC                         
         BE    V05                                                              
*                                                                               
         USING RVDSECT,R2                                                       
*                                                                               
         LA    R1,DIGITAB                                                       
V01      CLI   0(R1),X'FF'                                                      
         BE    V03                                                              
*                                                                               
         CLC   RVSCMBID,0(R1)                                                   
         BE    EXIT                                                             
         LA    R1,L'DIGITAB(R1)                                                 
         B     V01                                                              
*                                                                               
V03      CLI   RVLSNLOC,C'1'                                                    
         BE    *+8                                                              
         CLI   RVLSNLOC,C'3'                                                    
         BE    *+8                                                              
         CLI   RVLSNLOC,C'4'                                                    
         BNE   EXIT                                                             
*                                                                               
         MVC   SORTKEY(RVACHID-RVRECID),RVRECID                                 
         MVC   SORTKEY+RVACHID-RVRECID(8),RVSCMBTP                              
         MVC   SORTKEY+RVSCMBID-RVRECID(6),RVACHID                              
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTAREA                                 
         B     EXIT                                                             
*                                                                               
V05      GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4    ARE WE DONE?                                 
         BNZ   V06                                                              
*                                                                               
         MVI   GETFLAG,C'D'        "D"ONE                                       
         LA    R6,TEMPREC+4                                                     
         B     V07                                                              
*                                                                               
V06      L     R2,DMCB+4           ADDRESS OF RECD FROM SORT                    
         MVC   SORTKEY,4(R2)       FIRST PART OF RECORD IS SORTKEY              
         USING PVDSECT,R6                                                       
         LA    R6,TEMPREC+4                                                     
         LA    R2,L'SORTKEY+8(R2)                                               
         USING RVDSECT,R2                                                       
*        MVC   VAIRTIME,RVAIRTIM                                                
         CLC   PREVKEY(L'SORTKEY-6),SORTKEY    SAME KEY?                        
         BE    V20                                                              
*                                                                               
         CLI   PREVKEY,C'V'                                                     
         BNE   V15                                                              
*                                                                               
V07      LA    R3,1                                                             
         LAY   R4,VALTAB                                                        
V10      DS    0H                                                               
         OC    0(CELVRWQ,R4),0(R4)                                              
         BZ    V12                                                              
         MVC   PVRECID(L'SORTKEY-6),PREVKEY                                     
         MVC   PVAIRTIM,VAIRTIME                                                
         EDIT  (R3),(2,PVPPQCN),COMMAS=YES,ALIGN=RIGHT,FILL=0                   
         MVI   PVSPCR1,C':'                                                     
         MVC   PVPRJCTN(CELVRWQ),0(R4)                                          
         XC    TEMPREC(4),TEMPREC                                               
         MVC   TEMPREC(2),=AL2(VRECLEN)                                         
         BAS   RE,PROCESS                                                       
V12      AHI   R4,CELVRWQ                                                       
         AHI   R3,1                                                             
         CHI   R3,CELGRPQ                                                       
         BNH   V10                                                              
*                                                                               
         LAY   RE,VALTAB           WHEN ALL ROWS ARE PUT OUT                    
         LHI   RF,L'VALTAB         CLEAR TABLE FOR FUTURE USE                   
         XCEFL                                                                  
         CLI   GETFLAG,C'D'        ALL DONE                                     
         BE    EXIT                                                             
*                                                                               
V15      LAY   RE,VALTAB                                                        
         LHI   RF,L'VALTAB                                                      
         XCEFL                                                                  
*                                                                               
         XC    PREVKEY,PREVKEY                                                  
         MVC   PREVKEY(L'SORTKEY-6),SORTKEY    SAVE KEY                         
*                                                                               
V20      MVC   TEMPID,RVACHID                                                   
         BAS   RE,PROCPTR                                                       
         L     R1,VALPTR                                                        
         OC    RVPRJCTN,=C'00000000'                                            
         MVC   VAIRTIME,RVAIRTIM                                                
         MVC   0(CELVLNQ,R1),RVPRJCTN                                           
*                                                                               
         B     V05                                                              
******************************************************************              
*PROCPTR: DETERMINE WHERE TO PLACE THE DEMVAL IN THE TABLE                      
******************************************************************              
PROCPTR  NTR1                                                                   
         LAY   R3,CHARTAB                                                       
         USING CHARTABD,R3                                                      
         LAY   R4,VALTAB                                                        
PV10     CLC   TEMPID,CHID                                                      
         BE    PV20                                                             
         AHI   R3,L'CHARTAB                                                     
         CLC   =X'FFFF',0(R3)                                                   
         BNE   PV10                                                             
         DC    H'0'                                                             
PV20     PACK  DUB,CHQCODE         ROW NUMBER IN TABLE                          
         CVB   R1,DUB                                                           
         SHI   R1,1                ROW-1                                        
         MHI   R1,CELNUMQ          (ROW-1)*CELNUMQ                              
*                                                                               
         PACK  DUB,CHDNUM          CELL NUMBER IN TABLE                         
         CVB   R2,DUB                                                           
         SHI   R2,1                CELL-1                                       
         AR    R1,R2               {(CELL-1)+(ROW-1)*22}*6=PTR                  
         CLI   PREVKEY,C'V'                                                     
         BNE   *+12                                                             
         MHI   R1,CELVLNQ                                                       
         B     *+8                                                              
         MHI   R1,CELLLNQ                                                       
         AR    R4,R1                                                            
         ST    R4,VALPTR           PTR+VALTAB=ADDRESS TO STORE CELL             
         B     EXIT                                                             
         DROP  R3                                                               
******************************************************************              
* R6: POINTS TO OUTPUT RECORD                                                   
******************************************************************              
PROCESS  NTR1                                                                   
*                                                                               
         OC    0(127,R6),SPACES                                                 
         OC    126(128,R6),SPACES                                               
*                                                                               
         SHI   R6,4                                                             
         PUT   FILOUT,(R6)           OUTPUT THE RECORD                          
         L     RE,OUTCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OUTCNT                                                        
*                                                                               
         LA    RE,TEMPREC                                                       
         LHI   RF,L'TEMPREC                                                     
         XCEFL                                                                  
         B     EXIT                                                             
******************************************************************              
* PRINT TO CONSOLE DIGITAL STATIONS WE DON'T EXPECT.                            
******************************************************************              
PRINTDIG NTR1                                                                   
         LA    R2,DIGITAB                                                       
         CLI   0(R2),X'FF'                                                      
         BE    PDX                                                              
PD10     MVC   P(L'RJSCMBID),0(R2)                                              
         MVI   P+L'RJSCMBID,X'40'                                               
         MVC   P+L'RJSCMBID+1(L'RJSTTN),L'RJSCMBID(R2)                          
         MVI   P+L'RJSCMBID+L'RJSTTN+1,X'40'                                    
         MVC   P+L'RJSCMBID+L'RJSTTN+2(L'RJBAND),10(R2)                         
         GOTO1 =V(PRINTER)                                                      
         LA    R2,L'DIGITAB(R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BNE   PD10                                                             
PDX      B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
* THE EMAIL ROUTINE UTILIZES SMTP TO SEND AN EMAIL TO THE TEAM                  
* NOTIFYING THEM OF STATIONS THAT THE SORT HAS SKIPPED, DUE TO                  
* VARIOUS REASONS..  (NO DIGITAL TRANSLATION AVAILABLE)                         
******************************************************************              
EMAIL    NTR1                                                                   
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAINI',0)    ATTACH AND INIT. JESMAIL         
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAPRS',SENDTO),('SUBJCTLQ',SUBJECT)           
*                                                                               
         LA    R2,DIGITAB                                                       
         CLI   0(R2),X'FF'                                                      
         BE    EMX                                                              
EM10     MVC   BODYLINE(L'RJSCMBID),0(R2)                                       
         MVI   BODYLINE+L'RJSCMBID,X'40'                                        
         MVC   BODYLINE+L'RJSCMBID+1(L'RJSTTN),L'RJSCMBID(R2)                   
         MVI   BODYLINE+L'RJSCMBID+L'RJSTTN+1,X'40'                             
         MVC   BODYLINE+L'RJSCMBID+L'RJSTTN+2(L'RJBAND),10(R2)                  
         GOTO1 =V(SMTP),DMCB,('SMTPAPTL',BODYLINE)                              
         LA    R2,L'DIGITAB(R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BNE   EM10                                                             
*                                                                               
EM20     DS    0H                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPASND',0)   SEND THE BUFFERED E-MAIL          
*                                                                               
         GOTO1 =V(SMTP),DMCB,('SMTPAEND',0)   DETACH JESMAIL                    
*                                                                               
EMX      B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
SORTCRD  DC    C'SORT FIELDS=(5,'                                               
SORTCRDL DS    CL3                                                              
         DC    C',A),FORMAT=BI '                                                
RECCRD   DC    C'RECORD TYPE=V,LENGTH=350 '                                     
*                                                                               
*** BLKSIZE= PARAMETER IS REQUIRED, OR ELSE THE ICETOOL "COUNT" CALL            
*** WILL FAIL. DFSORT REQUIRES THAT AN EXPLICIT BLKSIZE BE SET.                 
FILIN1   DCB   DDNAME=FILIN1,DSORG=PS,RECFM=VB,MACRF=(GM),             X        
               EODAD=COMB25,LRECL=255,BLKSIZE=25500,SYNAD=IOERR                 
*                                                                               
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=VB,MACRF=(PM),             X        
               LRECL=255,BLKSIZE=0                                              
*                                                                               
* DCB FOR DATASET WITH FILENAMES IN NFS-MOUNTED FOLDER                          
NFSFILES DCB   DDNAME=NFSFILES,DSORG=PS,MACRF=GM,LRECL=256,RECFM=VB,   X        
               EODAD=CLOSNAMS                                                   
         EJECT                                                                  
INCNT    DC    F'0'                                                             
OUTCNT   DC    F'0'                                                             
DUB      DS    D                                                                
DMCB     DS    6F                                                               
OPERMSG  DS    CL100               MAXIMUM LENGTH FOR LOGIO                     
WORK     DS    CL256                                                            
FRSTFLAG DS    X                                                                
GETFLAG  DS    CL1                                                              
TEMPID   DS    CL6                                                              
PREVKEY  DS    CL30                                                             
CARD     DS    CL80                                                             
VAIRTIME DS    CL1                 VRECS BACKUP AIRTIME INDICATOR               
BYTE     DS    C                                                                
DUMPFLAG DC    C'Y'                'Y' = STXITER DUMP, 'O' = MVS DUMP           
VALPTR   DS    F                                                                
VDEMTABS DS    V                                                                
DIGITAB  DS    100XL(L'RJSCMBID+L'RJSTTN+L'RJBAND)                              
         DS    F                                                                
         SPACE 2                                                                
SENDTO   DS    CL50                                                             
SUBJECT  DC    C'TRITON RADIO: BYPASSED DIGITAL STATIONS'                       
SUBJJBID DS    CL8                                                              
         DC    C' '                                                             
SUBJJBNM DS    CL8                                                              
SUBJCTLQ EQU   *-SUBJECT                                                        
BODYLINE DS    CL80                                                             
*                                                                               
STXTAB   DS    0F                                                               
         DC    A(DETRSRT)                                                       
         DC    V(DUMMY)                                                         
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
ICEPAR1  DC    A(0)                ICETOOL PARAMETER LIST                       
         DC    A(CNTSA1L)          A(COUNT STATEMENT)                           
         DC    A(0)                NO RETURN AREA NEEDED                        
         DC    X'FFFFFFFF'         EOL                                          
*                                                                               
CNTSA1L  DC    AL2(L'CNTSA1S)      LENGTH OF ONE ICETOOL CONTROL CARD           
CNTSA1S  DS    CL80                COUNT OPERATOR                               
*                                                                               
         SPACE 2                                                                
CDEMTABS DC    CL8'T00AD1'                                                      
ANALOGS  DC    C'AM'                                                            
         DC    C'FM'                                                            
         DC    C'AF'                                                            
         DC    C'AA'                                                            
         DC    C'FF'               FM+FM                                        
         DC    C'  '               PUMM                                         
         DC    X'FF'               END OF TABLE                                 
         SPACE 2                                                                
         LTORG                                                                  
RECLENQ  EQU   300                                                              
*                                                                               
*======================================================================         
* KEEP THESE AREAS CONTIGUOUS                                                   
         DS    0D                                                               
         DC    C'SORTAREA'                                                      
SORTAREA DC    AL2(RECLENQ+33+4)                                                
         DC    AL2(0)                                                           
SORTKEY  DS    CL(RVAIRTIM-RVRECID)     27 BYTES                                
IOAREA   DS    CL(RECLENQ)                                                      
*======================================================================         
*                                                                               
TEMPREC  DS    CL(RECLENQ)                                                      
VALTAB   DS    CL(RECLENQ*9)                                                    
CELGRPQ  EQU   14                  THERE ARE 14 GROUPS                          
CELNUMQ  EQU   24                  24 CELLS PER GROUP                           
CELLLNQ  EQU   6                   6 BYTES PER CELL                             
CELROWQ  EQU   144                                                              
CELVLNQ  EQU   8                   V RECS HAVE 8 BYTES PROJECTIONS              
CELVRWQ  EQU   192                                                              
DRECLEN  EQU   PDPPDMN-PDRECID+2+4                                              
GRECLEN  EQU   PGVAL-PGRECID+CELROWQ+4                                          
HRECLEN  EQU   PHWTPOP-PHRECID+CELROWQ+4                                        
VRECLEN  EQU   PVPRJCTN-PVRECID+CELVRWQ+4                                       
         EJECT                                                                  
CHARTAB  DS    0CL(CHARTQ)                                                      
         DC    CL6'     1',CL2'01',CL2'01'                                      
         DC    CL6'   254',CL2'02',CL2'01'                                      
         DC    CL6'   255',CL2'03',CL2'01'                                      
         DC    CL6'     3',CL2'05',CL2'01'                                      
         DC    CL6'     4',CL2'06',CL2'01'                                      
         DC    CL6'     5',CL2'07',CL2'01'                                      
         DC    CL6'     6',CL2'09',CL2'01'                                      
         DC    CL6'    82',CL2'10',CL2'01'                                      
         DC    CL6'     7',CL2'11',CL2'01'                                      
         DC    CL6'     8',CL2'12',CL2'01'                                      
         DC    CL6'   267',CL2'13',CL2'01'                                      
         DC    CL6'   268',CL2'14',CL2'01'                                      
         DC    CL6'    10',CL2'16',CL2'01'                                      
         DC    CL6'    11',CL2'17',CL2'01'                                      
         DC    CL6'    12',CL2'18',CL2'01'                                      
         DC    CL6'    13',CL2'20',CL2'01'                                      
         DC    CL6'    14',CL2'21',CL2'01'                                      
         DC    CL6'    15',CL2'22',CL2'01'                                      
         DC    CL6'   251',CL2'23',CL2'01'                                      
         DC    CL6'   264',CL2'24',CL2'01'                                      
         DC    CL6'   472',CL2'04',CL2'08'                                      
         DC    CL6'   441',CL2'04',CL2'04'                                      
         DC    CL6'   440',CL2'04',CL2'03'                                      
         DC    CL6'   442',CL2'04',CL2'05'                                      
         DC    CL6'   443',CL2'04',CL2'06'                                      
         DC    CL6'   444',CL2'04',CL2'07'                                      
         DC    CL6'   450',CL2'04',CL2'12'                                      
         DC    CL6'   447',CL2'04',CL2'10'                                      
         DC    CL6'   448',CL2'04',CL2'09'                                      
         DC    CL6'   449',CL2'04',CL2'11'                                      
         DC    CL6'   451',CL2'04',CL2'13'                                      
         DC    CL6'   452',CL2'04',CL2'14'                                      
         DC    CL6'   453',CL2'05',CL2'02'                                      
         DC    CL6'   454',CL2'05',CL2'03'                                      
         DC    CL6'   455',CL2'05',CL2'04'                                      
         DC    CL6'   456',CL2'05',CL2'05'                                      
         DC    CL6'   460',CL2'05',CL2'08'                                      
         DC    CL6'   458',CL2'05',CL2'06'                                      
         DC    CL6'   459',CL2'05',CL2'07'                                      
         DC    CL6'   461',CL2'05',CL2'09'                                      
         DC    CL6'   462',CL2'05',CL2'10'                                      
         DC    CL6'   463',CL2'05',CL2'11'                                      
         DC    CL6'   464',CL2'05',CL2'12'                                      
         DC    CL6'   465',CL2'05',CL2'13'                                      
         DC    CL6'   466',CL2'05',CL2'14'                                      
         DC    CL6'   467',CL2'08',CL2'02'                                      
         DC    CL6'   468',CL2'08',CL2'03'                                      
         DC    CL6'   469',CL2'08',CL2'04'                                      
         DC    CL6'   470',CL2'08',CL2'05'                                      
         DC    CL6'   471',CL2'08',CL2'06'                                      
         DC    CL6'   473',CL2'08',CL2'11'                                      
         DC    CL6'   474',CL2'08',CL2'12'                                      
         DC    CL6'   475',CL2'08',CL2'13'                                      
         DC    CL6'   476',CL2'08',CL2'14'                                      
         DC    CL6'   477',CL2'08',CL2'07'                                      
         DC    CL6'   478',CL2'08',CL2'08'                                      
         DC    CL6'   479',CL2'08',CL2'09'                                      
         DC    CL6'   480',CL2'08',CL2'10'                                      
         DC    CL6'   482',CL2'09',CL2'03'                                      
         DC    CL6'   483',CL2'09',CL2'04'                                      
         DC    CL6'   484',CL2'09',CL2'05'                                      
         DC    CL6'   485',CL2'09',CL2'06'                                      
         DC    CL6'   487',CL2'09',CL2'08'                                      
         DC    CL6'   488',CL2'09',CL2'09'                                      
         DC    CL6'   489',CL2'09',CL2'10'                                      
         DC    CL6'   490',CL2'09',CL2'11'                                      
         DC    CL6'   492',CL2'09',CL2'13'                                      
         DC    CL6'   493',CL2'09',CL2'14'                                      
         DC    CL6'   494',CL2'10',CL2'02'                                      
         DC    CL6'   496',CL2'10',CL2'04'                                      
         DC    CL6'   497',CL2'10',CL2'05'                                      
         DC    CL6'   498',CL2'10',CL2'06'                                      
         DC    CL6'   500',CL2'10',CL2'08'                                      
         DC    CL6'   501',CL2'10',CL2'09'                                      
         DC    CL6'   502',CL2'10',CL2'10'                                      
         DC    CL6'   503',CL2'10',CL2'11'                                      
         DC    CL6'   505',CL2'10',CL2'13'                                      
         DC    CL6'   506',CL2'10',CL2'14'                                      
         DC    CL6'   507',CL2'11',CL2'02'                                      
         DC    CL6'   509',CL2'11',CL2'04'                                      
         DC    CL6'   511',CL2'11',CL2'06'                                      
         DC    CL6'   512',CL2'11',CL2'07'                                      
         DC    CL6'   513',CL2'11',CL2'08'                                      
         DC    CL6'   515',CL2'11',CL2'10'                                      
         DC    CL6'   516',CL2'11',CL2'11'                                      
         DC    CL6'   517',CL2'11',CL2'12'                                      
         DC    CL6'   519',CL2'11',CL2'14'                                      
         DC    CL6'   524',CL2'15',CL2'03'                                      
         DC    CL6'   526',CL2'15',CL2'04'                                      
         DC    CL6'   527',CL2'15',CL2'05'                                      
         DC    CL6'   529',CL2'15',CL2'07'                                      
         DC    CL6'   531',CL2'15',CL2'12'                                      
         DC    CL6'   532',CL2'15',CL2'10'                                      
         DC    CL6'   533',CL2'15',CL2'09'                                      
         DC    CL6'   534',CL2'15',CL2'11'                                      
         DC    CL6'   537',CL2'16',CL2'02'                                      
         DC    CL6'   538',CL2'16',CL2'03'                                      
         DC    CL6'   539',CL2'16',CL2'04'                                      
         DC    CL6'   542',CL2'16',CL2'06'                                      
         DC    CL6'   543',CL2'16',CL2'07'                                      
         DC    CL6'   544',CL2'16',CL2'09'                                      
         DC    CL6'   545',CL2'16',CL2'10'                                      
         DC    CL6'   546',CL2'16',CL2'11'                                      
         DC    CL6'   549',CL2'16',CL2'14'                                      
         DC    CL6'   550',CL2'19',CL2'02'                                      
         DC    CL6'   551',CL2'19',CL2'03'                                      
         DC    CL6'   553',CL2'19',CL2'05'                                      
         DC    CL6'   554',CL2'19',CL2'06'                                      
         DC    CL6'   556',CL2'19',CL2'11'                                      
         DC    CL6'   558',CL2'19',CL2'14'                                      
         DC    CL6'   559',CL2'19',CL2'07'                                      
         DC    CL6'   560',CL2'19',CL2'08'                                      
         DC    CL6'   562',CL2'19',CL2'10'                                      
         DC    CL6'   564',CL2'20',CL2'03'                                      
         DC    CL6'   565',CL2'20',CL2'04'                                      
         DC    CL6'   566',CL2'20',CL2'05'                                      
         DC    CL6'   567',CL2'20',CL2'06'                                      
         DC    CL6'   568',CL2'20',CL2'07'                                      
         DC    CL6'   571',CL2'20',CL2'10'                                      
         DC    CL6'   572',CL2'20',CL2'11'                                      
         DC    CL6'   573',CL2'20',CL2'12'                                      
         DC    CL6'   574',CL2'20',CL2'13'                                      
         DC    CL6'   576',CL2'21',CL2'02'                                      
         DC    CL6'   577',CL2'21',CL2'03'                                      
         DC    CL6'   579',CL2'21',CL2'05'                                      
         DC    CL6'   581',CL2'21',CL2'07'                                      
         DC    CL6'   582',CL2'21',CL2'08'                                      
         DC    CL6'   583',CL2'21',CL2'09'                                      
         DC    CL6'   585',CL2'21',CL2'11'                                      
         DC    CL6'   587',CL2'21',CL2'13'                                      
         DC    CL6'   588',CL2'21',CL2'14'                                      
         DC    CL6'   589',CL2'22',CL2'02'                                      
         DC    CL6'   481',CL2'09',CL2'02'                                      
         DC    CL6'   486',CL2'09',CL2'07'                                      
         DC    CL6'   491',CL2'09',CL2'12'                                      
         DC    CL6'   495',CL2'10',CL2'03'                                      
         DC    CL6'   499',CL2'10',CL2'07'                                      
         DC    CL6'   504',CL2'10',CL2'12'                                      
         DC    CL6'   590',CL2'22',CL2'03'                                      
         DC    CL6'   591',CL2'22',CL2'04'                                      
         DC    CL6'   592',CL2'22',CL2'05'                                      
         DC    CL6'   593',CL2'22',CL2'06'                                      
         DC    CL6'   594',CL2'22',CL2'07'                                      
         DC    CL6'   595',CL2'22',CL2'08'                                      
         DC    CL6'   596',CL2'22',CL2'09'                                      
         DC    CL6'   597',CL2'22',CL2'10'                                      
         DC    CL6'   598',CL2'22',CL2'11'                                      
         DC    CL6'   599',CL2'22',CL2'12'                                      
         DC    CL6'   600',CL2'22',CL2'13'                                      
         DC    CL6'   601',CL2'22',CL2'14'                                      
         DC    CL6'   602',CL2'15',CL2'02'                                      
         DC    CL6'   510',CL2'11',CL2'05'                                      
         DC    CL6'   518',CL2'11',CL2'13'                                      
         DC    CL6'   528',CL2'15',CL2'06'                                      
         DC    CL6'   535',CL2'15',CL2'13'                                      
         DC    CL6'   541',CL2'16',CL2'08'                                      
         DC    CL6'   548',CL2'16',CL2'13'                                      
         DC    CL6'   555',CL2'15',CL2'08'                                      
         DC    CL6'   561',CL2'19',CL2'09'                                      
         DC    CL6'   570',CL2'20',CL2'09'                                      
         DC    CL6'   578',CL2'21',CL2'04'                                      
         DC    CL6'   584',CL2'21',CL2'10'                                      
         DC    CL6'   508',CL2'11',CL2'03'                                      
         DC    CL6'   514',CL2'11',CL2'09'                                      
         DC    CL6'   520',CL2'04',CL2'02'                                      
         DC    CL6'   530',CL2'19',CL2'12'                                      
         DC    CL6'   536',CL2'15',CL2'14'                                      
         DC    CL6'   540',CL2'16',CL2'05'                                      
         DC    CL6'   547',CL2'16',CL2'12'                                      
         DC    CL6'   552',CL2'19',CL2'04'                                      
         DC    CL6'   557',CL2'19',CL2'13'                                      
         DC    CL6'   563',CL2'20',CL2'02'                                      
         DC    CL6'   569',CL2'20',CL2'08'                                      
         DC    CL6'   575',CL2'20',CL2'14'                                      
         DC    CL6'   580',CL2'21',CL2'06'                                      
         DC    CL6'   586',CL2'21',CL2'12'                                      
         DC    X'FFFF'             EOT                                          
         EJECT                                                                  
         DS    0D                                                               
         DC    CL16'*****UTL********'                                           
UTL      DC    4X'00',X'0C'        UTL FOR DEMO                                 
*                                                                               
         EJECT                                                                  
         DS    0L                                                               
         DC    CL16'*****SSB********'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
         EJECT                                                                  
* DDDPRINT                                                                      
* DERAREC                                                                       
* DEDEMTABD                                                                     
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DERAREC                                                        
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
       ++INCLUDE DDSMTPD                                                        
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
CHARTABD DSECT                                                                  
CHID     DS    CL6                                                              
CHDNUM   DS    CL2                                                              
CHQCODE  DS    CL2                                                              
CHARTQ   EQU   *-CHID                                                           
*                                                                               
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
         IEFZB4D2                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DETRSRT   03/06/14'                                      
         END                                                                    
