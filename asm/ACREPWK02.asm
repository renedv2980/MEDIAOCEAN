*          DATA SET ACREPWK02  AT LEVEL 007 AS OF 06/16/08                      
*PHASE ACWK02A,+0                                                               
         TITLE 'PROGRAM TO COMPARE WORKER FILES'                                
ACWK02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACWK**,RA                                                    
         L     R9,0(R1)                                                         
         USING ACWORKD,R9                                                       
         LA    RC,SPACEND                                                       
         USING ACWKD,RC                                                         
         MVI   NOFLG,C' '                                                       
         MVC   NOFLG+1(L'NOFLG-1),NOFLG                                         
         TM    0(R1),X'80'         TEST CALLED FROM ANOTHER PROGRAM             
         BNO   ACWK04                                                           
         XC    HEADHOOK,HEADHOOK                                                
         L     RF,ACSPECS          CLEAR SPEC POOL                              
         MVI   0(RF),0                                                          
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     RF,4(R1)                                                         
         MVC   OLDFIL,0(RF)        SET FILE TO PRINT                            
         OI    OPT,OPTPRNT                                                      
         TM    4(R1),X'01'         PURGE THE WORKER FILE                        
         BNO   *+8                                                              
         OI    OPT,OPTPURW                                                      
         B     ACWK06                                                           
*                                                                               
ACWK04   CLI   MODE,RUNFRST                                                     
         BNE   XIT                                                              
*                                                                               
ACWK06   L     RF,ADCOMFAC                                                      
         MVC   SCANNER,CSCANNER-COMFACSD(RF)                                    
*                                                                               
         XR    RF,RF                                                            
         LHI   R1,L'IO1                                                         
         L     R0,AIO1                                                          
         MVCL  R0,RE               CLEAR IO1                                    
         TM    OPT,OPTPRNT                                                      
         BO    PRTFIL                                                           
*                                                                               
         EJECT                                                                  
**********************************************************************          
* GET INPUT OPTIONS                                                  *          
**********************************************************************          
OPT1     GOTO1 CARDS,DMCB,CARD,RE00                                             
         CLC   CARD(L'EOFC),EOFC                                                
         BE    OPT9                                                             
         MVC   P(80),CARD                                                       
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R0,MXBLK                                                         
         GOTO1 SCANNER,DMCB,(C'C',CARD),((R0),BLOCK)                            
         XR    R0,R0                                                            
         ICM   R0,1,DMCB+4         NUMBER OF PARAMETERS                         
         BZ    *+12                                                             
         CHI   R0,MXBLK                                                         
         BNH   *+12                                                             
         OI    ERROR,ERRBADC                                                    
         B     OPTERR                                                           
*                                                                               
         LA    R3,BLOCK                                                         
         USING SCANBLKD,R3                                                      
*                                                                               
OPT3     LA    R2,OPTAB                                                         
         XR    R1,R1                                                            
         IC    R1,SC1STLEN         R1=LENGTH OF LEFT SIDE                       
         BCTR  R1,0                                                             
*                                                                               
OPT5     EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SC1STFLD    MATCH TABLE TO INPUT FIELD                   
         BE    OPT7                                                             
         LA    R2,L'OPTAB(R2)                                                   
         CLI   0(R2),EOT                                                        
         BNE   OPT5                                                             
         OI    ERROR,ERRBADC                                                    
         B     OPTERR                                                           
*                                                                               
OPT7     XR    RF,RF                                                            
         ICM   RF,3,8(R2)                                                       
         AR    RF,RB                                                            
         BASR  RE,RF               GO TO ROUTINE                                
         CLI   ERROR,0                                                          
         BNE   OPTERR                                                           
         LA    R3,SCBLKLQ(R3)                                                   
         BCT   R0,OPT3                                                          
         B     OPT1                                                             
         DROP  R3                                                               
*                                                                               
OPT9     LA    R2,OLDFIL                                                        
O        USING FILD,R2                                                          
         OC    O.FILID,O.FILID     TEST MISSING OLD FILE ID                     
         BNZ   *+8                                                              
         OI    ERROR,ERRNOLD       SET 'MISSING' OLD                            
         LA    R3,NEWFIL                                                        
N        USING FILD,R3                                                          
         OC    N.FILID,N.FILID     TEST MISSING NEW FILE ID                     
         BNZ   *+8                                                              
         OI    ERROR,ERRNNEW                                                    
         CLC   O.FILID,N.FILID                                                  
         BNE   *+8                                                              
         OI    OPT,OPTSFID         SAME FILE ID                                 
         DROP  O,N                                                              
*                                                                               
OPTERR   CLI   ERROR,0             ANY ERRORS ?                                 
         BE    CMPWRK              NO, COMPARE WORKER FILES                     
         LA    R3,ERRTAB           PRINT ERROR MESSAGES                         
         XR    R1,R1                                                            
         IC    R1,ERROR                                                         
*                                                                               
OPTERR3  EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(R3),0                                                          
         BNO   OPTERR5                                                          
         MVC   P(L'ERRTAB-1),1(R3)                                              
         GOTO1 ACREPORT                                                         
*                                                                               
OPTERR5  LA    R3,L'ERRTAB(R3)                                                  
         CLI   0(R3),EOT                                                        
         BNE   OPTERR3                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* OPTION SUBROUTINES                                                 *          
**********************************************************************          
OPTROLD  LA    R5,OLDFIL+(FILID-FILD)                                           
         B     *+8                                                              
OPTRNEW  LA    R5,NEWFIL+(FILID-FILD)                                           
         BAS   RE,BINDX            BUILD FILE INDEX FOR OLD                     
         CLI   ERROR,0                                                          
         BNE   OPTERR                                                           
         B     OPT1                                                             
*                                                                               
OPTRNSR  OI    OPT,OPTNOSR         DON'T PRINT SAME RECORDS                     
         BR    RE                                                               
*                                                                               
OPTRNSL  OI    OPT,OPTNOSL         DON'T PRINT SAME ELEMENTS                    
         BR    RE                                                               
*                                                                               
OPTRFAC  OI    OPT,OPTFACWK        FACWK INPUT                                  
         BR    RE                                                               
*                                                                               
OPTRFOL  OI    OPT,OPTFOLD         FOLD ELEMENTS                                
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD WORKER FILE INDEX                                            *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
         USING UKRECD,R5                                                        
BINDX    NTR1  ,                                                                
         OC    UKINDEX,UKINDEX     TEST, ALREADY HAVE ID                        
         BZ    BINDX9                                                           
         OI    ERROR,ERRDUPF       DUPLICATE FILE ID                            
         B     XIT                                                              
*                                                                               
BINDX3   LA    R2,INDXTAB                                                       
         XR    R1,R1                                                            
         IC    R1,SC1STLEN         R1=LENGTH OF LEFT SIDE                       
         BCTR  R1,0                                                             
*                                                                               
BINDX5   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SC1STFLD    MATCH TABLE TO INPUT FIELD                   
         BE    BINDX7                                                           
         LA    R2,L'OPTAB(R2)                                                   
         CLI   0(R2),EOT                                                        
         BNE   BINDX5                                                           
         OI    ERROR,ERRBADC                                                    
         B     XIT                                                              
*                                                                               
BINDX7   XR    RF,RF                                                            
         ICM   RF,3,8(R2)                                                       
         AR    RF,RB                                                            
         BASR  RE,RF               GO TO ROUTINE                                
*                                                                               
BINDX9   LA    R3,SCBLKLQ(R3)                                                   
         BCT   R0,BINDX3                                                        
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* BUILD INDEX SUBROUTINES                                            *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
         USING UKRECD,R5                                                        
BIORG    MVC   UKUSRID,SC2NDNUM+2  ORIGIN NUMBER                                
         OC    UKUSRID,UKUSRID     TEST, INPUT WAS NUMERIC                      
         BNZR  RE                                                               
         LR    R1,RE                                                            
         BAS   RE,GETID                                                         
         LR    RE,R1                                                            
         OC    UKUSRID,UKUSRID     TEST ID RESOLVED                             
         BNZR  RE                                                               
         OI    ERROR,ERRBADC                                                    
         BR    RE                                                               
*                                                                               
BIPRG    MVC   UKSYSPRG,SC2NDFLD   PROG                                         
         BR    RE                                                               
*                                                                               
BISUB    CLI   SC2NDFLD,C'*'       SUB                                          
         BER   RE                                                               
         MVC   UKSUBPRG,SC2NDFLD                                                
         BR    RE                                                               
*                                                                               
BIDAY    ICM   RF,15,SC2NDNUM      DAY                                          
         CVD   RF,DUB                                                           
         ICM   RF,3,DUB+6                                                       
         SRL   RF,4                                                             
         STC   RF,UKDAY                                                         
         BR    RE                                                               
*                                                                               
BITYP    MVC   UKCLASS,SC2NDFLD    TYPE                                         
         BR    RE                                                               
*                                                                               
BISEQ    MVC   UKFILNO,SC2NDNUM+2  SEQUENCE                                     
         BR    RE                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* GET CONTROL FILE ID                                                *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
         USING UKRECD,R5                                                        
GETID    NTR1  ,                                                                
         LA    R7,CTKEY            READ CTFILE TO GET NUMBER                    
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,CTIKTYPQ                                                  
         MVC   CTIKID,SC2NDFLD     ALPHA ID FROM CARD                           
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,CTIKEY,AIO1,(0,0)                     
         L     R7,AIO1                                                          
         CLC   CTKEY,CTIKEY                                                     
         BNE   XIT                 INVALID ID                                   
         LA    R7,CTIDATA                                                       
         XR    R0,R0                                                            
*                                                                               
         USING CTDSCD,R7                                                        
GETID3   CLI   0(R7),0                                                          
         BE    XIT                                                              
         CLI   0(R7),CTDSCELQ                                                   
         BE    GETID4                                                           
         IC    R0,CTDSCLEN                                                      
         AR    R7,R0                                                            
         B     GETID3                                                           
*                                                                               
GETID4   MVC   UKUSRID,CTDSC                                                    
         B     XIT                                                              
         DROP  R3,R5,R7                                                         
         EJECT                                                                  
**********************************************************************          
* PRINT A FILE/ PURGE FILE AFTER PRINTING                            *          
**********************************************************************          
PRTFIL   DS    0H                                                               
         LHI   R1,L'WRKBUF                                                      
         L     R0,AWRKBUF                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR WORKER FILE BUFFER                     
         MVC   FLGS,NOFLG                                                       
         MVC   P,SPACES                                                         
         ZAP   RCRDN,PZRO                                                       
*                                                                               
         GOTO1 WORKER,DMCB,INDEX,AWRKBUF,OLDFIL                                 
         CLI   8(R1),0                                                          
         BE    PRTFIL3                                                          
         MVC   P+1(L'MSG1),MSG1     POSTING FILE NOT FOUND                      
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
PRTFIL3  LHI   R1,WRKLNQ                                                        
         L     R0,AWRKREC                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR WORKER FILE BUFFER                     
*                                                                               
         GOTO1 WORKER,DMCB,READ,AWRKBUF,OLDFIL,AWRKREC                          
         CLI   8(R1),0             TEST EOF                                     
         BE    PRTFIL4             NO, OK TO PRINT                              
         TM    OPT,OPTPURW         TEST PURGE                                   
         BNO   XIT                                                              
         GOTO1 WORKER,DMCB,PURGE,AWRKBUF,OLDFIL                                 
         B     XIT                                                              
*                                                                               
PRTFIL4  L     R3,AWRKHDR                                                       
         USING FFTELD,R3                                                        
         CLI   FFTEL,FFTELQ        LOOK FOR ACCOUNT HEADER                      
         BNE   PRTFIL5                                                          
         CLI   FFTTYPE,FFTTWFSA                                                 
         BNE   PRTFIL5                                                          
         MVC   P+2(10),=C'REQUESTOR:'                                           
         MVC   P+13(L'QUESTOR),FFTDATA                                          
         LA    RF,P+31                                                          
         MVC   0(4,RF),=C'JOB:'                                                 
         MVC   5(L'ACTKACT,RF),FFTDATA+L'QUESTOR                                
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         ZAP   RCRDN,PZRO                                                       
         B     PRTFIL3                                                          
*                                                                               
         USING PSSUBFD,R3                                                       
PRTFIL5  CLI   PSSBEL,PSSBELQ      TEST TOTAL ELEMENT                           
         BE    PRTFIL3             YES, SKIP IT                                 
         DROP  R3                                                               
*                                                                               
         AP    RCRDN,PONE                                                       
         MVC   PLEFT,SPACES                                                     
         MVC   PLEFT+5(6),=C'RECORD'                                            
         EDIT  RCRDN,(4,PLEFT+13),ALIGN=LEFT                                    
*                                                                               
         XR    R0,R0                                                            
*                                                                               
PRTFIL9  IC    R0,1(R3)                                                         
         GOTO1 HEXPRT,DMCB,(R0),0(R3)                                           
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   PRTFIL9                                                          
         B     PRTFIL3                                                          
         EJECT                                                                  
**********************************************************************          
* COMPARE FILES WORKER FILES                                         *          
**********************************************************************          
CMPWRK   MVI   RCSUBPRG,1                                                       
         TM    OPT,OPTFACWK                                                     
         BO    CMPWRK1             COMPARE RECOVERY FILES                       
         GOTO1 BLDMAP,DMCB,OLDFIL  GET DATA AND BUILD MAPS                      
         GOTO1 BLDMAP,DMCB,NEWFIL                                               
         B     CMPWRK3                                                          
*                                                                               
CMPWRK1  TM    OPT,OPTSFID         TEST SAME FILE                               
         BNO   *+12                                                             
         MVI   RCSUBPRG,2                                                       
         MVI   FLTRCV,1            ONLY 'COPIES'                                
         GOTO1 BLDMAP,DMCB,OLDFIL  GET DATA AND BUILD MAPS                      
         TM    OPT,OPTSFID         TEST SAME FILE                               
         BNO   *+8                                                              
         MVI   FLTRCV,0            EXCLUDE COPIES                               
         GOTO1 BLDMAP,DMCB,NEWFIL                                               
*                                                                               
CMPWRK3  L     R0,ACOMP                                                         
         LHI   R1,L'COMP                                                        
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR TABLE AREA                             
*                                                                               
         L     R7,ACOMP                                                         
         USING COMPD,R7                                                         
*                                                                               
         LA    R2,OLDFIL                                                        
         USING FILD,R2                                                          
         L     R4,FILRLO                                                        
O        USING RLOD,R4             R4=RECORD LOCATOR ENTRY(OLD)                 
*                                                                               
CMPWRK5  XC    COMPD(COMPLNQ),COMPD                                             
         CLI   O.RLOD,EOT          TEST END OF OLD                              
         BE    CMPWRK25                                                         
*                                                                               
CMPWRK6  ICM   RF,15,O.RLOADR                                                   
         LR    R0,RF               R0=A(OLD RECORD)                             
         XR    R1,R1                                                            
         ICM   R1,3,0(RF)          R1=(LENGTH OF OLD RECORD)                    
*                                                                               
         LA    R2,NEWFIL                                                        
         L     R6,FILRLO                                                        
N        USING RLOD,R6             R6=RECORD LOCATOR ENTRY(NEW)                 
*                                                                               
CMPWRK9  CLI   N.RLOD,EOT          TEST END OF NEW                              
         BE    CMPWRK23            YES, "OLD" DELETED                           
         TM    N.RLOSTA,RLOSMAT    TEST ALREADY MATCHED                         
         BNO   CMPWRK11            NO,                                          
CMPWRK10 LA    R6,RLOLNQ(R6)       R6 TO NEXT NEW RECORD LOCATOR                
         B     CMPWRK9                                                          
*                                                                               
CMPWRK11 ICM   RE,15,N.RLOADR      RE=A(NEW RECORD)                             
         CLM   R1,3,0(RE)          TEST SAME LENGTH                             
         BNE   CMPWRK12            NO, TEST HEADER                              
         LR    RF,R1               YES, COMPARE FULL RECORD                     
         CLCL  R0,RE                                                            
         BE    CMPWRK20            RECORD "MATCHED"                             
*                                                                               
CMPWRK12 ICM   R1,15,O.RLOADR                                                   
         AHI   R1,4                R1=A(OLD FIRST ELEMENT)                      
         ICM   RE,15,N.RLOADR                                                   
         AHI   RE,4                RE=A(NEW FIRST ELEMENT)                      
         LA    RF,L'ACTKEY                                                      
         TM    OPT,OPTFACWK                                                     
         BO    CMPWRK14            COMPARE RECOVERY FILES                       
         LA    RF,ACCORFST                                                      
         TM    O.RLOSTA,RLOSOFR    TEST OLD ACCOUNT FILE RECORD                 
         BNO   *+12                                                             
         TM    N.RLOSTA,RLOSOFR                                                 
         BO    CMPWRK14                                                         
*                                                                               
         CLC   0(2,R1),0(RE)       TEST CODE AND LENGTH                         
         BNE   CMPWRK10            NO, LOOK A NEXT                              
         XR    RF,RF                                                            
         IC    RF,1(RE)                                                         
*                                                                               
CMPWRK14 BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),0(RE)       TEST HEADER                                  
         BNE   CMPWRK10                                                         
         OI    COMPSTA,COMPHDR     SET HEADER "MATCHED"                         
         B     CMPWRK21                                                         
*                                                                               
CMPWRK20 OI    COMPSTA,COMPMAT     SET RECORDS "MATCHED"                        
CMPWRK21 MVC   COMPOLD,O.RLONUM                                                 
         MVC   COMPNEW,N.RLONUM                                                 
         OI    N.RLOSTA,RLOSMAT                                                 
         B     CMPWRK24                                                         
*                                                                               
CMPWRK23 MVC   COMPOLD,O.RLONUM    "OLD" DELETED                                
         OI    COMPSTA,COMPDEL                                                  
CMPWRK24 LA    R4,RLOLNQ(R4)       R4 TO NEXT OLD RECORD LOCATOR                
         LA    R7,COMPLNQ(R7)                                                   
         B     CMPWRK5                                                          
*                                                                               
CMPWRK25 LA    R2,NEWFIL                                                        
         L     R6,FILRLO                                                        
*                                                                               
CMPWRK26 CLI   N.RLOD,EOT          TEST END OF NEW                              
         BE    CMPWRK30            YES, "OLD" DELETED                           
         TM    N.RLOSTA,RLOSMAT    TEST ALREADY MATCHED                         
         BO    CMPWRK29            NO,                                          
*                                                                               
         MVC   COMPOLD,N.RLONUM    SET "NEW" ENTRY                              
         MVC   COMPNEW,N.RLONUM                                                 
         MVI   COMPINS,C'I'        SET INSERT                                   
         LA    R7,COMPLNQ(R7)                                                   
*                                                                               
CMPWRK29 LA    R6,RLOLNQ(R6)       R6 TO NEXT NEW RECORD LOCATOR                
         B     CMPWRK26                                                         
*                                                                               
CMPWRK30 MVI   0(R7),EOT           MARK END OF COMP TABLE                       
         L     R7,ACOMP            A(COMPARISON TABLE)                          
         XR    R0,R0               COUNT NUMBER IN TABLE                        
CMPWRK31 CLI   0(R7),EOT                                                        
         BE    CMPWRK33                                                         
         AHI   R0,1                                                             
         LA    R7,COMPLNQ(R7)                                                   
         B     CMPWRK31                                                         
*                                                                               
CMPWRK33 L     R7,ACOMP                                                         
         LA    RF,COMPLNQ                                                       
         USING ACMD,R5                                                          
         L     R5,AMONACC                                                       
         GOTO1 ACMVQSRT,DMCB,(0,(R7)),(R0),(RF),(RF),0,0                        
         B     REPRT               PRINT THE REPORT                             
         DROP  R2,R7,R5,O,N                                                     
                                                                                
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO CONTROL PRINTING OF REPORT                              *          
**********************************************************************          
REPRT    MVI   FORCEHED,C'Y'                                                    
         L     R6,ACOMP                                                         
         USING COMPD,R6                                                         
*                                                                               
REPRT3   CLI   0(R6),EOT           TEST EOT                                     
         BE    SUMR                YES, PRINT SUMMARY REPORT                    
         LA    R4,PLEFT                                                         
         USING PRTD,R4                                                          
         MVC   PLEFT,SPACES                                                     
*                                                                               
         TM    COMPSTA,COMPMAT     TEST RECORDS MATCHED                         
         BNO   REPRT5              NO,                                          
         CLC   COMPOLD,COMPNEW     TEST SEQUENCE IS CORRECT                     
         BNE   REPRT6              NO,                                          
         AP    MATCNT,PONE                                                      
         TM    OPT,OPTNOSR         DON'T PRINT SAME RECORDS ?                   
         BO    REPRT15             NO,                                          
         MVC   PRTACT,SAME                                                      
         GOTO1 PRNT,DMCB,COMPD,=C'PO'                                           
         B     REPRT13                                                          
*                                                                               
REPRT5   TM    COMPSTA,COMPHDR     TEST MATCHED HEADER                          
         BNO   REPRT7                                                           
REPRT6   AP    CHGCNT,PONE                                                      
         GOTO1 MATLM,DMCB,COMPD    MATCH ELEMENTS                               
         GOTO1 PRNT,DMCB,COMPD,=C'CO'                                           
         TM    OPT,OPTFOLD                                                      
         BO    REPRT13                                                          
         GOTO1 PRNT,DMCB,COMPD,=C'CN'                                           
         B     REPRT13                                                          
*                                                                               
REPRT7   CLI   COMPINS,C'I'      INSERTED TO NEW                                
         BNE   REPRT9                                                           
         AP    INSCNT,PONE                                                      
         BAS   RE,PNEWNUM                                                       
         MVC   PRTACT,INSERT                                                    
         GOTO1 PRNT,DMCB,COMPD,=C'PN'                                           
         B     REPRT13                                                          
*                                                                               
REPRT9   TM    COMPSTA,COMPDEL     DELETED FROM OLD                             
         BO    *+6                                                              
         DC    H'0'                UNKNOWN ACTION                               
         AP    DELCNT,PONE                                                      
         BAS   RE,POLDNUM                                                       
         MVC   PRTACT,DELETE                                                    
         GOTO1 PRNT,DMCB,COMPD,=C'PO'                                           
*                                                                               
REPRT13  MVI   P+1,C'*'                                                         
         MVC   P+2(L'P-2),P+1                                                   
         GOTO1 ACREPORT                                                         
REPRT15  LA    R6,COMPLNQ(R6)                                                   
         B     REPRT3                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO PRINT A RECORD                                          *          
*  P1 = A(COMPARISON ENTRY)                                          *          
*  P2 = C'CO' COMPARE OLD TO NEW                                     *          
*     = C'CN' COMPARE NEW TO OLD                                     *          
*     = C'PO' PRINT OLD                                              *          
*     = C'PN' PRINT NEW                                              *          
**********************************************************************          
PRNT     NTR1  ,                                                                
         L     R6,0(R1)                                                         
         USING COMPD,R6                                                         
         L     RF,4(R1)                                                         
*                                                                               
         LA    R7,ACTAB            FIND ENTRY IN ACTION TABLE                   
         USING PACTD,R7                                                         
PRNT3    CLC   PACTACT,0(RF)                                                    
         BE    PRNT5                                                            
         LA    R7,PACTLNQ(R7)                                                   
         CLI   0(R7),EOT                                                        
         BNE   PRNT3                                                            
         DC    H'0'                INVALID ACTION                               
*                                                                               
PRNT5    XR    R3,R3               R3=A(PRIMARY FILE ID)                        
         ICM   R3,3,PACTPID                                                     
         LA    R3,ACWKD(R3)                                                     
         XR    RE,RE               R2=RECORD NUMBER                             
         ICM   RE,3,PACTPRN                                                     
         LA    RE,COMPD(RE)                                                     
         XR    R2,R2                                                            
         ICM   R2,3,0(RE)                                                       
         BAS   RE,GETREC           GET ADDRESS OF RECORD IN R4                  
         L     RE,RECLO            A(RECORD LOCATOR ENTRY)                      
         MVC   RECSTA,RLOSTA-RLOD(RE) SAVE STATUS                               
         MVC   SVDIR,DIR                                                        
         LA    R5,4(R4)            R5=FIRST ELEMENT OR KEY                      
         CLI   PACTACT,C'P'        TEST PRINT ONLY                              
         BE    PONLY               YES,                                         
*                                                                               
         XR    R3,R3               R3=A(SECONDARY FILE ID)                      
         ICM   R3,3,PACTSID                                                     
         LA    R3,ACWKD(R3)                                                     
         XR    RE,RE               R2=RECORD NUMBER                             
         ICM   RE,3,PACTSRN                                                     
         LA    RE,COMPD(RE)                                                     
         XR    R2,R2               R2=RECORD NUMBER                             
         ICM   R2,3,0(RE)                                                       
         BAS   RE,GETREC           GET ADDRESS OF RECORD IN R4                  
         LA    R4,4(R4)            R4=FIRST ELEMENT OR KEY                      
         TM    OPT,OPTFOLD                                                      
         BO    PFOLD                                                            
*                                                                               
         LA    R8,PLEFT                                                         
         USING PRTD,R8                                                          
         CLI   PACTACT+1,C'N'                                                   
         BE    PRNT7                                                            
         BAS   RE,POLDNUM                                                       
         B     PRNT8                                                            
*                                                                               
PRNT7    BAS   RE,PNEWNUM                                                       
PRNT8    TM    OPT,OPTFACWK        TEST FACWK FILES                             
         BO    PRNT10              YES,                                         
         TM    RECSTA,RLOSOFR      OLD FILE RECORD                              
         BNO   PRNT11                                                           
         LA    R2,ACCORFST                                                      
         B     *+8                                                              
*                                                                               
PRNT10   LA    R2,ACTRFST-ACTKEY   LENGTH OF KEY & STATUS                       
         CLI   SVDIR,C'Y'          TEST DIRECTORY RECORD                        
         BNE   *+8                                                              
         LA    R2,ACCKLEN          YES,                                         
*                                                                               
         MVC   PRTACT,CHANGE                                                    
         GOTO1 CMPEL,DMCB,((R2),(R5)),(R4)                                      
         BNE   *+10                                                             
         MVC   PRTACT,SAME                                                      
         GOTO1 HEXPRT,DMCB,(R2),0(R5)                                           
         CLI   SVDIR,C'Y'          TEST DIRECTORY RECORD                        
         BE    XIT                 ALL DONE                                     
*                                                                               
PRNT11   XR    R1,R1                                                            
         ICM   R1,3,PACTSEQ        SET SEQUENCE                                 
         BAS   RE,SRTLXT           SORT THE TABLE                               
         L     R3,ALXTAB           R3=A(ELEMENT X-REF TABLE)                    
         XR    RF,RF                                                            
         ICM   RF,3,PACTSEQ                                                     
         AR    R3,RF               R3=A(PRIMARY ELEMENT FIELD)                  
         L     R2,ALXTAB           R2=A(ELEMENT X-REF TABLE)                    
         ICM   RF,3,PACTXRE                                                     
         AR    R2,RF               R2=A(X-REF ELEMENT FIELD)                    
         XR    R0,R0                                                            
         IC    R0,NLXTAB           R0=NUMBER IN TABLE                           
*                                                                               
PRNT13   STC   R0,NLXREM                                                        
         XR    R5,R5                                                            
         ICM   R5,7,1(R3)          R5=A(PRIMARY ELEMENT)                        
         XR    R4,R4                                                            
         ICM   R4,7,1(R2)          R4=A(X-REF ELEMENT)                          
*                                                                               
         LTR   R5,R5               TEST ANY PRIMARY                             
         BNZ   PRNT15              YES,                                         
         MVC   PRTACT,INSERT       ASSUME INSERT ON NEW                         
         CLC   PACTACT,=C'CO'                                                   
         BE    *+10                                                             
         MVC   PRTACT,DELETE       IF NEW TO OLD IT'S DELETE                    
         XR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         GOTO1 HEXPRT,DMCB,(RF),0(R4)                                           
         B     PRNT21                                                           
*                                                                               
PRNT15   LTR   R4,R4               TEST ANY X-REF ELEMENT                       
         BNZ   PRNT17              YES                                          
         MVC   PRTACT,DELETE                                                    
         CLC   PACTACT,=C'CO'                                                   
         BE    *+10                                                             
         MVC   PRTACT,INSERT                                                    
         B     PRNT19                                                           
*                                                                               
PRNT17   MVC   PRTACT,CHANGE                                                    
         GOTO1 CMPEL,DMCB,(R5),(R4)                                             
         BNE   PRNT19                                                           
         MVC   PRTACT,SAME                                                      
         TM    OPT,OPTNOSL         SUPPRESS SAME ELEMENTS                       
         BNO   PRNT19                                                           
         MVC   PLEFT,SPACES                                                     
         B     PRNT21                                                           
*                                                                               
PRNT19   XR    RF,RF                                                            
         IC    RF,1(R5)                                                         
         GOTO1 HEXPRT,DMCB,(RF),0(R5)                                           
*                                                                               
PRNT21   LA    R3,LXRLNQ(R3)                                                    
         LA    R2,LXRLNQ(R2)                                                    
         XR    R0,R0                                                            
         IC    R0,NLXREM                                                        
         BCT   R0,PRNT13                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT 'FOLD' OPTION                                                 *         
***********************************************************************         
PFOLD    LA    R8,PLEFT                                                         
         USING PRTD,R8                                                          
         LA    R2,ACCORFST                                                      
         TM    RECSTA,RLOSOFR      TEST FILE RECORD                             
         BO    PFOLD2              YES,                                         
         TM    OPT,OPTFACWK        TEST FACWK FILES                             
         BNO   PFOLD5              NO,                                          
         LA    R2,ACTRFST-ACTKEY   LENGTH OF KEY & STATUS                       
         CLI   SVDIR,C'Y'          TEST DIRECTORY RECORD                        
         BNE   *+8                                                              
         LA    R2,ACCKLEN          YES,                                         
*                                                                               
PFOLD2   BAS   RE,POLDNUM                                                       
         GOTO1 CMPEL,DMCB,((R2),(R5)),(R4)                                      
         BNE   PFOLD3                                                           
         BAS   RE,PNEWNUM                                                       
         MVC   PRTACT,SAME                                                      
         GOTO1 HEXPRT,DMCB,(R2),0(R5)                                           
         CLI   SVDIR,C'Y'          TEST DIRECTORY RECORD                        
         BE    XIT                                                              
         B     PFOLD5                                                           
*                                                                               
PFOLD3   MVC   PRTACT,CHANGE                                                    
         GOTO1 HEXPRT,DMCB,(R2),0(R5)                                           
         MVC   FLGS,NOFLG                                                       
         MVC   PRTACT,CHANGE                                                    
         BAS   RE,PNEWNUM                                                       
         GOTO1 HEXPRT,DMCB,(R2),0(R4)                                           
         CLI   SVDIR,C'Y'          TEST DIRECTORY RECORD                        
         BE    XIT                                                              
*                                                                               
PFOLD5   XR    R1,R1                                                            
         ICM   R1,3,PACTSEQ        SET SEQUENCE                                 
         BAS   RE,SRTLXT           SORT THE TABLE                               
         L     R3,ALXTAB           R3=A(ELEMENT X-REF TABLE)                    
         XR    RF,RF                                                            
         ICM   RF,3,PACTSEQ                                                     
         AR    R3,RF               R3=A(PRIMARY ELEMENT FIELD)                  
         L     R2,ALXTAB           R2=A(ELEMENT X-REF TABLE)                    
         ICM   RF,3,PACTXRE                                                     
         AR    R2,RF               R2=A(X-REF ELEMENT FIELD)                    
         XR    R0,R0                                                            
         IC    R0,NLXTAB           R0=NUMBER IN TABLE                           
*                                                                               
PFOLD7   STC   R0,NLXREM           SAVE NUMBER REMAINING                        
         XR    R5,R5                                                            
         ICM   R5,7,1(R3)          R5=A(PRIMARY ELEMENT)                        
         XR    R4,R4                                                            
         ICM   R4,7,1(R2)                                                       
         GOTO1 CMPEL,DMCB,(R5),(R4)                                             
         BNE   PFOLD11                                                          
         BAS   RE,POLDNUM                                                       
         BAS   RE,PNEWNUM                                                       
         MVC   PRTACT,SAME                                                      
         TM    OPT,OPTNOSL         SUPPRESS SAME ELEMENTS ?                     
         BNO   PFOLD9              NO,                                          
         MVC   PLEFT,SPACES                                                     
         B     PFOLD19                                                          
*                                                                               
PFOLD9   XR    RF,RF                                                            
         IC    RF,1(R5)                                                         
         GOTO1 HEXPRT,DMCB,(RF),0(R5)                                           
         B     PFOLD19                                                          
*                                                                               
PFOLD11  LTR   R5,R5               TEST ELEMENT FOUND                           
         BNZ   PFOLD13             YES,                                         
         MVC   PRTACT,INSERT       MUST BE AN *INSERT*                          
         B     PFOLD17                                                          
*                                                                               
PFOLD13  LTR   R4,R4                                                            
         BNZ   PFOLD15                                                          
         MVC   PRTACT,DELETE                                                    
         MVC   FLGS,NOFLG                                                       
         BAS   RE,POLDNUM                                                       
         XR    RF,RF                                                            
         IC    RF,1(R5)                                                         
         GOTO1 HEXPRT,DMCB,(RF),0(R5)                                           
         B     PFOLD19                                                          
*                                                                               
PFOLD15  MVC   PRTACT,CHANGE                                                    
         BAS   RE,POLDNUM                                                       
         XR    RF,RF                                                            
         IC    RF,1(R5)                                                         
         GOTO1 HEXPRT,DMCB,(RF),0(R5)                                           
*                                                                               
PFOLD17  MVC   FLGS,NOFLG                                                       
         MVC   PRTACT,CHANGE                                                    
         BAS   RE,PNEWNUM                                                       
         XR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         GOTO1 HEXPRT,DMCB,(RF),0(R4)                                           
*                                                                               
PFOLD19  LA    R3,LXRLNQ(R3)                                                    
         LA    R2,LXRLNQ(R2)                                                    
         XR    R0,R0                                                            
         IC    R0,NLXREM                                                        
         BCT   R0,PFOLD7                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT KEYS AND ELEMENTS - NO COMPARISON                             *         
***********************************************************************         
PONLY    LA    R8,PLEFT                                                         
         USING PRTD,R8                                                          
         MVC   FLGS,NOFLG                                                       
         CLI   PACTACT+1,C'N'                                                   
         BE    PONLY3                                                           
         BAS   RE,POLDNUM                                                       
         BAS   RE,PNEWNUM                                                       
         B     PONLY5                                                           
*                                                                               
PONLY3   BAS   RE,PNEWNUM                                                       
PONLY5   TM    RECSTA,RLOSOFR      TEST FILE RECORD                             
         BNO   *+12                YES,                                         
         LA    R2,ACCORFST                                                      
         B     PONLY6                                                           
*                                                                               
         TM    OPT,OPTFACWK        TEST FACWK FILES                             
         BNO   PONLY7              NO,                                          
         LA    R2,ACTRFST-ACTKEY   LENGTH OF KEY & STATUS                       
         CLI   SVDIR,C'Y'          TEST DIRECTORY RECORD                        
         BNE   *+8                                                              
         LA    R2,ACCKLEN          YES,                                         
*                                                                               
PONLY6   GOTO1 HEXPRT,DMCB,(R2),0(R5)                                           
         CLI   SVDIR,C'Y'          TEST DIRECTORY RECORD                        
         BE    XIT                                                              
         AR    R5,R2                                                            
*                                                                               
PONLY7   XR    R2,R2                                                            
         IC    R2,1(R5)                                                         
*                                                                               
PONLY9   LTR   R2,R2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 HEXPRT,DMCB,(R2),0(R5)                                           
         AR    R5,R2                                                            
         CLI   0(R5),0                                                          
         BNE   PONLY7                                                           
         B     XIT                                                              
POLDNUM  OC    COMPOLD,COMPOLD                                                  
         BZR   RE                                                               
         LA    R8,PLEFT                                                         
         CLC   PRTACT,CHANGE                                                    
         BE    *+10                                                             
         TM    COMPSTA,COMPOPR     TEST NUMBER ALREADY PRINTED                  
         BOR   RE                                                               
         EDIT  (B2,COMPOLD),(L'PRTOLD,PRTOLD),FILL=0                            
         OI    COMPSTA,COMPOPR     SET ALREADY PRINTED                          
         BR    RE                                                               
*                                                                               
PNEWNUM  OC    COMPNEW,COMPNEW                                                  
         BZR   RE                                                               
         LA    R8,PLEFT                                                         
         CLC   PRTACT,CHANGE                                                    
         BE    *+10                                                             
         TM    COMPSTA,COMPNPR     TEST NUMBER ALREADY PRINTED                  
         BOR   RE                                                               
         EDIT  (B2,COMPNEW),(L'PRTNEW,PRTNEW),FILL=0                            
         OI    COMPSTA,COMPNPR     SET ALREADY PRINTED                          
         BR    RE                                                               
                                                                                
         DROP  R6,R7,R8                                                         
         EJECT                                                                  
**********************************************************************          
* GET WORKER FILE BUILD A MAP                                        *          
*  PARM 1 = A(FILE INFO)                                             *          
**********************************************************************          
BLDMAP   NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING FILD,R2                                                          
         LHI   R1,L'WRKBUF                                                      
         L     R0,AWRKBUF                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR WORKER FILE BUFFER                     
*                                                                               
         BAS   RE,GINDX            GET INDEX RECORD                             
*                                                                               
         XR    R0,R0               R0=CUMULATIVE LENGTH                         
         XR    R3,R3               R3=COUNT RECORD                              
         XR    R4,R4                                                            
         L     RF,AWRKREC                                                       
         ST    RF,AIO                                                           
*                                                                               
BLDMAP3  BAS   RE,GWKR             GET WORKER RECORD                            
         BNE   BLDMAP5                                                          
         AHI   R3,1                COUNT RECORDS                                
         ICM   RF,15,AIO                                                        
         ICM   R4,3,0(RF)          R4= RECORD LENGTH                            
         AR    R0,R4               ADD TO TOTAL LENGTH                          
         B     BLDMAP3                                                          
*                                                                               
BLDMAP5  ST    R3,FILNUM           SAVE NUMBER OF RECORDS                       
         AR    R0,R3               ADD EXTRA BYTE FOR EACH RECORD               
         ST    R0,FILLEN           AND LENGTH OF DATA                           
         MHI   R3,RLOLNQ           #RECORDS X LEN OF LOCATOR ENTRY              
         AR    R0,R3                                                            
         AHI   R0,L'IO1            ADD A FEW EXTRA BYTES                        
         ST    R0,FULL                                                          
         GETMAIN R,LV=(0)          GET SOME STORAGE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                NOT ENOUGH STORAGE AVAILABLE                 
*                                                                               
         L     R5,ADMASTC                                                       
         USING MASTD,R5                                                         
         LA    R6,MCUSRDMP                                                      
         OC    MCUSRDMP(4),MCUSRDMP                                             
         BZ    *+8                                                              
         LA    R6,8(R6)                                                         
         LR    RF,R1                                                            
         ST    RF,0(R6)                                                         
         AR    RF,R0                                                            
         ST    RF,4(R6)                                                         
         DROP  R5                                                               
*                                                                               
         ST    R1,FILADR           A(FILE DATA)                                 
         A     R1,FILLEN                                                        
         AHI   R1,8                                                             
         ST    R1,FILRLO           A(RECORD LOCATOR TABLE)                      
*                                                                               
         L     R0,FILADR                                                        
         L     R1,FULL                                                          
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR FILE STORAGE AREA                      
*                                                                               
         LHI   R1,L'WRKBUF                                                      
         L     R0,AWRKBUF                                                       
         MVCL  R0,RE               CLEAR WORKER FILE BUFFER                     
*                                                                               
         BAS   RE,GINDX                                                         
*                                                                               
         L     R5,FILRLO           R5=A(RECORD LOCATOR TABLE)                   
         USING RLOD,R5                                                          
*                                                                               
         L     R3,FILADR           READ INTO STORAGE                            
         ST    R3,AIO                                                           
         XR    R4,R4                                                            
         XR    R0,R0                                                            
*                                                                               
BLDMAP7  BAS   RE,GWKR             GET WORKER FILE RECORD                       
         BNE   XIT                                                              
         AHI   R0,1                RECORD #                                     
         STCM  R0,3,RLONUM                                                      
         STCM  R3,15,RLOADR        RECORD ADDRESS                               
         MVC   RLOSTA,RECSTA       RECORD STATUS                                
         LA    R5,RLOLNQ(R5)                                                    
         MVI   0(R5),EOT           MARK END OF LOCATOR TABLE                    
*                                                                               
         ICM   R4,3,0(R3)                                                       
         AR    R3,R4               R3=NEXT AREA                                 
         MVI   0(R3),0             MARK EOR                                     
         LA    R3,1(R3)            NEXT RECORD AREA                             
         ST    R3,AIO                                                           
         B     BLDMAP7                                                          
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* MATCH ELEMENTS ACROSS RECORDS                                      *          
*  P1 =1 A(RECORD COMPARISON ENTRY)                                  *          
**********************************************************************          
MATLM    NTR1  ,                                                                
         L     RF,0(R1)                                                         
         USING COMPD,RF                                                         
         XR    R2,R2                                                            
         ICM   R2,3,COMPNEW        R2=RECORD NUMBER (NEW)                       
         LA    R3,NEWFIL           R3=A(FILE INFO - NEW)                        
         BAS   RE,GETREC           GET A(RECORD) INTO R4                        
         LA    R5,4(R4)            R5=A(FIRST NEW ELEMENT)                      
         L     RE,RECLO            A(RECORD LOCATOR ENTRY)                      
         MVC   RECSTA,RLOSTA-RLOD(RE) SAVE STATUS                               
*                                                                               
         XR    R2,R2                                                            
         ICM   R2,3,COMPOLD        R2=RECORD NUMBER (OLD)                       
         LA    R3,OLDFIL           R3=A(FILE INFO - OLD)                        
         BAS   RE,GETREC           GET A(RECORD) INTO R4                        
         LA    R3,4(R4)            R3=A(FIRST OLD ELEMENT)                      
         L     RE,RECLO            A(RECORD LOCATOR ENTRY)                      
         MVC   RECSTA,RLOSTA-RLOD(RE) SAVE STATUS                               
         TM    RECSTA,RLOSOFR      TEST FILE RECORD                             
         BO    MATLM1                                                           
         TM    OPT,OPTFACWK        TEST PROCESSING AN ACCOUNT FILE              
         BO    MATLM3                                                           
         B     MATLM4                                                           
         DROP  RF                                                               
*                                                                               
MATLM1   LA    R3,ACCORFST(R3)        R3 TO FIRST OLD ELEMENT                   
         LA    R5,ACCORFST(R5)        R5 TO FIRST NEW ELEMENT                   
         B     MATLM4                                                           
*                                                                               
MATLM3   LA    R3,ACTRFST-ACTKEY(R3)  R3 TO FIRST OLD ELEMENT                   
         LA    R5,ACTRFST-ACTKEY(R5)  R5 TO FIRST NEW ELEMENT                   
         CLI   DIR,C'Y'                                                         
         BE    XIT                                                              
*                                                                               
MATLM4   XC    OLDX,OLDX                                                        
         XR    RF,RF                                                            
         L     R0,ALXTAB           CLEAR ELEMENT TABLE                          
         LHI   R1,L'LXTAB                                                       
         MVCL  R0,RE                                                            
*                                                                               
         L     R7,ALXTAB           BUILD ELEMENT REFERENCE MAP                  
         USING LXRD,R7                                                          
         LA    RF,0(R5)            RF=NEW ELEMENT                               
         XR    R2,R2               R2=NUMBER IN TABLE                           
         XR    R0,R0                                                            
*                                                                               
MATLM5   STCM  RF,15,LXRNEW        SET 'NEW' ELEMENT ADDRESS                    
         AHI   R2,1                                                             
         LA    RE,0(R3)            RE=OLD ELEMENT                               
         LA    R1,OLDX                                                          
MATLM6   CLI   0(R1),C'X'          TEST OLD ELEMENT ALREADY MATCHED             
         BE    MATLM7              YES, SKIP TO NEXT 'OLD'                      
         CLC   0(1,RE),0(RF)       OLD ELEMENT CODE VS. NEW                     
         BNE   MATLM7                                                           
         STCM  RE,15,LXROLD        SET ADDRESSES OF MATCHED ITEMS               
         MVI   0(R1),C'X'          SET OLD MATCHED                              
         B     MATLM9                                                           
*                                                                               
MATLM7   LA    R1,1(R1)            NEXT OLDX FLAG                               
         IC    R0,1(RE)            NEXT 'OLD' ELEMENT                           
         AR    RE,R0                                                            
         CLI   0(RE),0             TEST EOR - OLD RECORD                        
         BNE   MATLM6                                                           
         MVI   LXROLD,X'FF'        SET 'NOT FOUND' - ON OLD                     
*                                                                               
MATLM9   LA    R7,LXRLNQ(R7)                                                    
         IC    R0,1(RF)                                                         
         AR    RF,R0               RF=NEXT 'NEW' ELEMENT                        
         CLI   0(RF),0                                                          
         BNE   MATLM5                                                           
*                                                                               
         LA    RE,0(R3)            RE=OLD ELEMENT                               
         LA    R1,OLDX                                                          
*                                                                               
MATLM11  CLI   0(R1),C'X'          TEST ELEMENT MATCHED                         
         BE    MATLM13             YES,                                         
         STCM  RE,15,LXROLD        SET ADDRESSES OF OLD                         
         MVI   LXRNEW,X'FF'        NOT ON 'NEW'                                 
         AHI   R2,1                UPDATE THE COUNT                             
         LA    R7,LXRLNQ(R7)                                                    
*                                                                               
MATLM13  LA    R1,1(R1)            NEXT OLDX                                    
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   MATLM11                                                          
         STC   R2,NLXTAB           SAVE NUMBER IN TABLE                         
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* COMPARE ELEMENTS                                                   *          
*  P1 = BYTE 0     LENGTH IF NOT AN ELEMENT                          *          
*     =      1-3   A(DATA TO BE PRINTED)                             *          
*  P2 =            A(ELEMENT TO BE COMPARED)                         *          
**********************************************************************          
CMPEL    NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         XC    FLGS,FLGS                                                        
         XR    RF,RF                                                            
         ICM   RF,1,0(R1)          RF=DATA LENGTH IF NOT ELEMENT                
*                                                                               
         XR    R1,R1                                                            
         LTR   R2,R2               TEST FIRST ELEMENT                           
         BNZ   CMPEL1              OK, HAVE DATA                                
         IC    R1,1(R3)            NO, USE LENGTH OF SECOND                     
         LA    R2,FFS                                                           
         B     CMPEL3                                                           
*                                                                               
CMPEL1   IC    R1,1(R2)                                                         
CMPEL3   LTR   RF,RF                                                            
         BZ    *+6                                                              
         LR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLGS(0),0(R2)       MOVE FIRST ELEMENT                           
*                                                                               
         LTR   R3,R3                                                            
         BNZ   CMPEL5                                                           
         IC    R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLGS(0),FFS                                                      
         B     CMPEL7                                                           
*                                                                               
CMPEL5   IC    R1,1(R3)                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         LR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    FLGS(0),0(R3)       SET BITS THAT ARE DIFFERENT                  
         OC    FLGS,FLGS                                                        
         BNZ   *+14                                                             
         MVC   FLGS,NOFLG                                                       
         B     CMPELX                                                           
*                                                                               
CMPEL7   LA    R4,FLGS                                                          
         LA    R2,FLGS2                                                         
         LA    R0,L'FLGS                                                        
         XR    R3,R3                                                            
         XR    R5,R5                                                            
*                                                                               
CMPEL9   CLI   0(R4),0                                                          
         BNE   CMPEL11                                                          
         MVI   0(R4),C' '          CHANGES ZEROS TO SPACES                      
         MVI   0(R2),C' '                                                       
         XR    R5,R5               SET TO 'NOT IN ERROR STRING'                 
         B     CMPEL15                                                          
*                                                                               
CMPEL11  LTR   R5,R5               TEST ALREADY IN ERROR STRING                 
         BZ    CMPEL13             NO, SET UP FIRST                             
         MVI   0(R4),C' '          CHANGES ZEROS TO SPACES                      
         MVI   0(R2),C'.'                                                       
         B     CMPEL15                                                          
*                                                                               
CMPEL13  STC   R3,BYTE             R3=DISPLACEMENT                              
         GOTO1 HEXOUT,DMCB,BYTE,HALF,1,0                                        
         MVC   0(1,R4),HALF        SET PRINTABLE DISPLACEMENT                   
         MVC   0(1,R2),HALF+1                                                   
         LA    R5,1                SET 'IN ERROR STRING'                        
*                                                                               
CMPEL15  LA    R4,1(R4)                                                         
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,CMPEL9                                                        
CMPELX   CLC   FLGS,NOFLG                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT CHARACTER AND HEX FORMAT                           *         
* P1 = WIDTH OF THE DATA                                              *         
* P3 = A(DATA)                                                        *         
***********************************************************************         
HEXPRT   NTR1  ,                   PRINTING ROUTINE                             
         LM    R3,R4,0(R1)                                                      
         MVC   P(L'PLEFT),PLEFT                                                 
         MVC   PLEFT,SPACES                                                     
         LA    R5,FLGS                                                          
*                                                                               
HEXPRT3  LA    R7,P                                                             
         USING PRTD,R7                                                          
         LR    R2,R3                                                            
         CHI   R2,MXWIDTH                                                       
         BNH   *+8                                                              
         LA    R2,MXWIDTH          R2=WIDTH OF PRINTING THIS LINE               
         BCTR  R2,0                                                             
*                                                                               
         LA    RE,3                RE=LINES TO BE PRINTED                       
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),SPACES      TEST FLAG DIFFERENCES                        
         BE    *+8                                                              
         LA    RE,2(RE)            NEED 2 EXTRA LINES                           
         XR    RF,RF                                                            
         IC    RF,LINE                                                          
         AR    RF,RE                                                            
         CLM   RF,1,MAXLINES                                                    
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'       FORCE NEW LINE                               
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PRTREC(0),0(R4)     MOVE DATA TO PRINT LINE                      
         EX    R2,*+8                                                           
         B     *+10                                                             
         TR    PRTREC(0),OUTTAB    TRANSLATE TO PRINTABLE                       
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R2,1(R2)                                                         
         GOTO1 HEXOUT,DMCB,(R4),HEXWORK,(R2),SEP                                
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PRTREC(0),HEXWORK   MOVE ZONES HALF TO PLINE                     
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    RF,HEXWORK+1(R2)                                                 
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PRTREC(0),0(RF)     MOVE NUMERIC HALF TO PLINE                   
         GOTO1 ACREPORT                                                         
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),SPACES      TEST FLAG DIFFERENCES                        
         BE    HEXPRT7                                                          
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PRTREC(0),0(R5)     SET FLAGS                                    
         GOTO1 ACREPORT                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   PRTREC(0),L'FLGS(R5)                                             
         GOTO1 ACREPORT                                                         
                                                                                
*                                                                               
HEXPRT7  GOTO1 ACREPORT            SKIP A LINE                                  
         LA    R2,1(R2)                                                         
         AR    R4,R2               POINT TO NEXT INPUT CHUNK                    
         AR    R5,R2                                                            
         SR    R3,R2               DECREMENT DATA WIDTH                         
         BP    HEXPRT3                                                          
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* GET ADDRESS OF RECORD                                              *          
*   R2 = RECORD NUMBER                                                          
*   R3 = A(FILE INFO)                                                           
**********************************************************************          
GETREC   BCTR  R2,0                   R2=RECORD NUMBER                          
         MHI   R2,RLOLNQ                                                        
         L     R4,FILRLO-FILD(R3)     R3=A(FILE INFO)                           
         AR    R4,R2                                                            
         ST    R4,RECLO               SAVE A(RECORD LOCATOR ENTRY)              
         ICM   R4,15,RLOADR-RLOD(R4)  R4=A(RETURN RECORD)                       
         MVI   DIR,C'N'               ASSUME NOT A DIRECTORY                    
         CLC   0(2,R4),DIRLEN                                                   
         BH    *+8                                                              
         MVI   DIR,C'Y'               YES, IT IS A DIRECTORY                    
         BR    RE                                                               
                                                                                
**********************************************************************          
* SORT ELEMENT X-REF TABLE                                           *          
*  NTRY R1 = DISPLACEMENT TO SORT FIELD                              *          
**********************************************************************          
SRTLXT   STM   RE,R8,SVREG                                                      
SRTLXT1  MVI   BYTE,0                                                           
         L     R2,ALXTAB                                                        
         LR    R3,R2               R3=A(START OF ENTRY)                         
         AR    R2,R1               R2=A(SORT FIELD)                             
         XR    R0,R0                                                            
         ICM   R0,1,NLXTAB         NUMBER IN TABLE                              
         BZ    SRTLXTX                                                          
*                                                                               
SRTLXT3  LA    RE,LXRLNQ(R2)       RE=A(NEXT SORT FIELD)                        
         LA    RF,LXRLNQ(R3)       RF=A(NEXT ENTRY)                             
         BCT   R0,SRTLXT5          TEST EOT                                     
         CLI   BYTE,1              TEST SEQUENCE CHANGED                        
         BE    SRTLXT1             YES, DO IT AGAIN                             
         B     SRTLXTX             ALL DONE                                     
*                                                                               
SRTLXT5  CLC   0(L'LXROLD,R2),0(RE)                                             
         BNH   SRTLXT7                                                          
         XC    0(LXRLNQ,R3),0(RF)  SWITCH THEM                                  
         XC    0(LXRLNQ,RF),0(R3)                                               
         XC    0(LXRLNQ,R3),0(RF)                                               
         MVI   BYTE,1                                                           
SRTLXT7  LA    R2,LXRLNQ(R2)                                                    
         LA    R3,LXRLNQ(R3)                                                    
         B     SRTLXT3                                                          
*                                                                               
SRTLXTX  LM    RE,R8,SVREG                                                      
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* WORKER FILE ROUTINES                                               *          
**********************************************************************          
         USING FILD,R2                                                          
GINDX    STM   RE,R8,SVREG                                                      
         TM    OPT,OPTFACWK                                                     
         BO    GINDXF                                                           
         GOTO1 WORKER,DMCB,INDEX,AWRKBUF,FILID                                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     WRKX                                                             
*                                                                               
GINDXF   GOTO1 DATAMGR,DMCB,BUFFER,FACWRK,FILID,AIO,AWRKBUF                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,INDEX,FACWRK,FILID,AIO,AWRKBUF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     WRKX                                                             
*                                                                               
GWKR     STM   RE,R8,SVREG                                                      
         MVI   RECSTA,0                                                         
         TM    OPT,OPTFACWK                                                     
         BO    GWKRF                                                            
         GOTO1 WORKER,DMCB,READ,AWRKBUF,FILID,AIO                               
         L     RE,AIO                                                           
         CLC   4(2,RE),=X'2F00'    TEST MEDIA RECORD                            
         BNE   *+8                                                              
         OI    RECSTA,RLOSOFR      SET OLD FILE RECORD                          
         CLI   8(R1),0                                                          
         B     WRKX                                                             
*                                                                               
GWKRF    GOTO1 DATAMGR,DMCB,READ,FACWRK,FILID,AFIO,AWRKBUF                      
         CLI   8(R1),0                                                          
         BNE   WRKX                                                             
         L     RF,AFIO                                                          
         LA    RF,4(RF)                                                         
         USING RECVHDR,RF                                                       
         MVC   FILQ,RFILTY                                                      
         CLI   FILQ,DIRQ           ACCDIR                                       
         BE    *+12                                                             
         CLI   FILQ,MSTQ           ACCMST                                       
         BNE   GWKRF                                                            
         CLC   RRECTY,FLTRCV       FILTER ONLY COPIES                           
         BE    GWKRF3              YES, TAKE COPY                               
         CLI   FLTRCV,0            ANY FILTER ?                                 
         BNE   GWKRF               YES, SKIP RECORD                             
         CLI   RRECTY,1            IS RECORD A 'COPY'                           
         BE    GWKRF               YES, SKIP RECORD                             
*                                                                               
GWKRF3   L     RF,AFIO                                                          
         XR    R1,R1                                                            
         ICM   R1,3,0(RF)          LENGTH (INCLUDING RECVHDR)                   
         SHI   R1,L'RECVHDR        REDUCE LENGTH                                
         L     R3,AIO                                                           
         STCM  R1,3,0(R3)          SET NEW LENGTH                               
         SHI   R1,4                R1=LENGTH FOR MOVE                           
         LA    RE,L'RECVHDR+4(RF)  RE=A(ACC FILE RECORD)                        
         LR    RF,R1                                                            
         L     R0,AIO                                                           
         AHI   R0,4                                                             
         MVCL  R0,RE               MOVE RECORD(WITHOUT RECOVERY DATA)           
         CR    RB,RB                                                            
*                                                                               
WRKX     LM    RE,R8,SVREG                                                      
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO PRINT SUMMARY REPORT                                    *          
**********************************************************************          
SUMR     MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,3                                                       
         LA    R2,OLDFIL                                                        
O        USING FILD,R2                                                          
         LA    R3,NEWFIL                                                        
N        USING FILD,R3                                                          
*                                                                               
         MVC   P+1(20),=CL20'NUMBER OF RECORDS'                                 
         L     R4,O.FILNUM          NUMBER OF 'OLD' RECORDS                     
         EDIT  (R4),(7,P+30)                                                    
         L     R4,N.FILNUM          NUMBER OF 'NEW' RECORDS                     
         EDIT  (R4),(7,P+40)                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+1(20),=CL20'NUMBER OF BYTES'                                   
         L     R4,O.FILLEN                                                      
         EDIT  (R4),(7,P+30)                                                    
         L     R4,N.FILLEN          NUMBER OF 'NEW' RECORDS                     
         EDIT  (R4),(7,P+40)                                                    
         GOTO1 ACREPORT                                                         
         DROP  O,N                                                              
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+1(20),=CL20'MATCHED RECORDS'                                   
         EDIT  MATCNT,(7,P+30)                                                  
         EDIT  MATCNT,(7,P+40)                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+1(20),=CL20'OLD FILE DELETIONS'                                
         EDIT  DELCNT,(7,P+30)                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+1(20),=CL20'NEW FILE INSERTIONS'                               
         EDIT  INSCNT,(7,P+40)                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
         CP    CHGCNT,PZRO                                                      
         BNE   SUMR3                                                            
         MVC   P+4(20),=CL20'NO CHANGED RECORDS'                                
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
SUMR3    MVC   P+1(20),=CL20'CHANGED RECORDS'                                   
         EDIT  CHGCNT,(7,P+30)                                                  
         EDIT  CHGCNT,(7,P+40)                                                  
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DATA CONSTANTS  AND LITERAL POOL                                   *          
**********************************************************************          
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
AFIO     DC    A(FIO)                                                           
AWRKREC  DC    A(WRKREC)                                                        
AWRKBUF  DC    A(WRKBUF)                                                        
AWRKHDR  DC    A(WRKHDR)                                                        
ALXTAB   DC    A(LXTAB)                                                         
ACOMP    DC    A(COMP)             A(COMPARISON TABLE)                          
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
INDEX    DC    CL8'INDEX   '                                                    
PURGE    DC    CL8'PURGE   '                                                    
READ     DC    CL8'READ    '                                                    
RE00     DC    CL8'RE00    '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
FACWRK   DC    CL8'FACWRK  '                                                    
BUFFER   DC    CL8'BUFFER  '                                                    
EOFC     DC    C'/*'                                                            
EOT      EQU   X'FF'                                                            
ALL      EQU   X'FF'                                                            
MXWIDTH  EQU   L'PRTREC                                                         
SEP      DC    C'SEP'              HEXOUT PARAMETER                             
*                                                                               
FILQ     DC    X'00'                                                            
DIRQ     EQU   X'69'                                                            
MSTQ     EQU   X'6A'                                                            
*                                                                               
DIRLEN   DC    AL2(ACCKLEN+4)      LENGTH OF DIRECTORY RECORD                   
*                                                                               
MATCNT   DC    PL4'0'              MATCHED RECORDS                              
INSCNT   DC    PL4'0'              NEW FILE INSERTS                             
DELCNT   DC    PL4'0'              OLD FILE DELETES                             
CHGCNT   DC    PL4'0'              CHANGED RECORDS                              
PONE     DC    PL1'1'                                                           
PZRO     DC    PL1'0'                                                           
FFS      DC    255X'FF'                                                         
*                                                                               
SAME     DC    C'  *SAME*   '                                                   
CHANGE   DC    C' *CHANGE*  '                                                   
INSERT   DC    C' *INSERT*  '                                                   
DELETE   DC    C' *DELETE*  '                                                   
*                                                                               
OPTAB    DS    0XL10                                                            
         DC    CL8'OLD     ',AL2(OPTROLD-ACWK02)                                
         DC    CL8'NEW     ',AL2(OPTRNEW-ACWK02)                                
         DC    CL8'FACWK   ',AL2(OPTRFAC-ACWK02)                                
         DC    CL8'SAMER=N ',AL2(OPTRNSR-ACWK02)                                
         DC    CL8'SAMEL=N ',AL2(OPTRNSL-ACWK02)                                
         DC    CL8'FOLD=Y  ',AL2(OPTRFOL-ACWK02)                                
         DC    AL1(EOT)                                                         
*                                                                               
INDXTAB  DS    0XL10                                                            
         DC    CL8'ORIGIN  ',AL2(BIORG-ACWK02)                                  
         DC    CL8'PROG    ',AL2(BIPRG-ACWK02)                                  
         DC    CL8'SUB     ',AL2(BISUB-ACWK02)                                  
         DC    CL8'DAY     ',AL2(BIDAY-ACWK02)                                  
         DC    CL8'TYPE    ',AL2(BITYP-ACWK02)                                  
         DC    CL8'SEQ     ',AL2(BISEQ-ACWK02)                                  
         DC    AL1(EOT)                                                         
*                                                                               
OPT      DC    X'00'               OPTIONS                                      
OPTFACWK EQU   X'80'               FACWK FILES                                  
OPTNOSR  EQU   X'40'               DON'T PRINT SAME RECORDS                     
OPTNOSL  EQU   X'20'               DON'T PRINT SAME ELEMENTS                    
OPTFOLD  EQU   X'10'               FOLD (PRINT OLD/NEW TOGETHER)                
OPTPRNT  EQU   X'08'               PRINT ONE(OLD) FILE                          
OPTSFID  EQU   X'04'               SAME FILE ID                                 
OPTPURW  EQU   X'02'               PURGE THE WORKER FILE                        
*                                                                               
ACTAB    DS    0H                                                               
         DC    C'CO'               COMPARE OLD VS. NEW                          
         DC    AL2(OLDFIL-ACWKD,COMPOLD-COMPD)                                  
         DC    AL2(NEWFIL-ACWKD,COMPNEW-COMPD)                                  
         DC    AL2(LXROLD-LXRD,LXRNEW-LXRD)                                     
*                                                                               
         DC    C'CN'               COMPARE NEW VS. OLD                          
         DC    AL2(NEWFIL-ACWKD,COMPNEW-COMPD)                                  
         DC    AL2(OLDFIL-ACWKD,COMPOLD-COMPD)                                  
         DC    AL2(LXRNEW-LXRD,LXROLD-LXRD)                                     
*                                                                               
         DC    C'PO'               PRINT OLD                                    
         DC    AL2(OLDFIL-ACWKD,COMPOLD-COMPD)                                  
         DC    AL2(0,0)                                                         
         DC    AL2(0,0)                                                         
*                                                                               
         DC    C'PN'               PRINT NEW                                    
         DC    AL2(NEWFIL-ACWKD,COMPNEW-COMPD)                                  
         DC    AL2(0,0)                                                         
         DC    AL2(0,0)                                                         
*                                                                               
         DC    X'FF'                                                            
*                                                                               
MSG1     DC    C'POSTING FILE NOT FOUND'                                        
*                                                                               
ERRTAB   DS    0CL31                                                            
         DC    AL1(ERRBADC),CL30'BAD CARD'                                      
         DC    AL1(ERRNOLD),CL30'MISSING "OLD" KEY'                             
         DC    AL1(ERRNNEW),CL30'MISSING "NEW" KEY'                             
         DC    AL1(ERRDUPF),CL30'DUPLICATE FILE DEFINITION'                     
         DC    AL1(EOT)                                                         
*                                                                               
OUTTAB   DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  00-0F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  10-1F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  20-2F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  30-3F                    
         DC    XL16'404B4B4B4B4B4B4B4B4B4A4B4C4D4E4F'  40-4F                    
         DC    XL16'504B4B4B4B4B4B4B4B4B5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60614B4B4B4B4B4B4B4B6A6B6C6D6E6F'  60-6F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B797A7B7C7D7E7F'  70-7F                    
         DC    XL16'4B8182838485868788894B4B4B4B4B4B'  80-8F                    
         DC    XL16'4B9192939495969798994B4B4B4B4B4B'  90-9F                    
         DC    XL16'4BA1A2A3A4A5A6A7A8A94B4B4B4B4B4B'  A0-AF                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C94B4B4B4B4B4B'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D94B4B4B4B4B4B'  D0-DF                    
         DC    XL16'E04BE2E3E4E5E6E7E8E94B4B4B4B4B4B'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'  F0-FF                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* IO AREAS AND STORAGE BUFFERS                                       *          
**********************************************************************          
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO1** '                                                    
IO1      DS    XL3000                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO2** '                                                    
IO2      DS    XL3000                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'*FACIO* '                                                    
FIO      DS    XL(4*1024)                                                       
*                                                                               
         DS    0D                                                               
         DC    CL8'*WRKREC*'                                                    
WRKREC   DS    0F                  WORKER FILE RECORD                           
WRKLEN   DS    H                   LENGTH                                       
         DS    H                   N/D                                          
WRKHDR   DS    0X                  HEADER                                       
         DS    XL(4*1024)                                                       
WRKLNQ   EQU   *-WRKREC                                                         
*                                                                               
         DS    0D                                                               
         DC    CL8'*COMP*'                                                      
COMP     DS    XL(5000*COMPLNQ)                                                 
*                                                                               
         DS    0D                                                               
         DC    CL8'*WRKBUF*'                                                    
WRKBUF   DS    XL(14*1024)                                                      
*                                                                               
*                                                                               
MXELM    EQU   50                                                               
         DS    0D                                                               
LXTAB    DS    XL(MXELM*LXRLNQ)   ELEMENT X-REF TABLE                           
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER WORKING STORAGE                                     *          
**********************************************************************          
ACWKD    DSECT                                                                  
SCANNER  DS    V                                                                
*                                                                               
         DS    0D                                                               
OLDFIL   DS    XL(FILLNQ)          'OLD' FILE INFO                              
*                                                                               
         DS    0D                                                               
NEWFIL   DS    XL(FILLNQ)          'NEW' FILE INFO                              
*                                                                               
OLDN     DS    F                                                                
NEWN     DS    F                                                                
*                                                                               
STATUS   DS    XL1                                                              
FLTRCV   DS    XL1                 RECOVERY FILTER                              
*                                                                               
DIR      DS    C                   DIRECTORY (Y/N)                              
SVDIR    DS    C                                                                
RECSTA   DS    X                                                                
RECLO    DS    A                   A(RECORD LOCATOR ENTRY)                      
*                                                                               
AIO      DS    A                                                                
*                                                                               
ERROR    DS    XL1                                                              
ERRBADC  EQU   X'80'               BAD CARD                                     
ERRNOLD  EQU   X'40'               NO OLD ID                                    
ERRNNEW  EQU   X'20'               NO NEW ID                                    
ERRDUPF  EQU   X'10'               DUPLICATE FILE DEFINITION                    
*                                                                               
CTKEY    DS    CL(L'CTIKEY)                                                     
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
MXBLK    EQU   10                                                               
BLOCK    DS    (MXBLK)CL(SCBLKLQ)                                               
*                                                                               
OLDX     DS    XL(MXELM*+1)                                                     
*                                                                               
NLXTAB   DS    XL1                 NUMBER IN TABLE                              
NLXREM   DS    XL1                 NUMBER REMAINING TO PRINT                    
*                                                                               
FLGS     DS    CL255                                                            
FLGS2    DS    CL255                                                            
NOFLG    DS    CL255               SPACES                                       
*                                                                               
HEXWORK  DS    CL(MXWIDTH*2)       DUMMY PRINT LINE                             
PLEFT    DS    CL(PRTHLNQ)         LEFT SIDE OF PRINT                           
RCRDN    DS    PL3                 RECORD NUMBER                                
*                                                                               
SVREG    DS    11F                 RE-R8                                        
*                                                                               
ACWKX    EQU   *                                                                
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER FILE INFO                                           *          
**********************************************************************          
FILD     DSECT                                                                  
FILID    DS    XL(L'UKINDEX)       WORKER FILE ID                               
FILNUM   DS    XL4                 NUMBER OF RECORDS                            
FILLEN   DS    XL4                 LENGTH OF FILE(NUMBER OF BYTES)              
FILADR   DS    XL4                 A(FILE)                                      
FILRLO   DS    XL4                 A(RECORD LOCATOR MAP)                        
FILLNQ   EQU   *-FILD                                                           
                                                                                
                                                                                
**********************************************************************          
* DSECT TO COVER RECORD LOCATOR ENTRY                                *          
**********************************************************************          
RLOD     DSECT                                                                  
RLONUM   DS    XL2                 RECORD NUMBER                                
RLOADR   DS    XL4                 ADDRESS                                      
RLOSTA   DS    XL1                 RECORD STATUS                                
RLOSOFR  EQU   X'80'               OLD FILE RECORD (KEY & RECORD)               
RLOSMAT  EQU   X'08'               RECORD MATCHED                               
RLOLNQ   EQU   *-RLOD                                                           
                                                                                
                                                                                
**********************************************************************          
* DSECT TO COVER COMPARISON ENTRY                                    *          
**********************************************************************          
COMPD    DSECT                                                                  
COMPOLD  DS    XL2                 OLD RECORD NUMBER                            
COMPINS  DS    C                   I(NSERTED TO   'NEW')                        
COMPNEW  DS    XL2                 NEW RECORD NUMBER                            
COMPSTA  DS    XL1                 STATUS                                       
COMPMAT  EQU   X'80'               RECORDS MATCH                                
COMPHDR  EQU   X'40'               HEADER MATCHES                               
COMPDEL  EQU   X'10'               DELETED  FROM 'OLD'                          
COMPOPR  EQU   X'08'               OLD NUMBER PRINTED                           
COMPNPR  EQU   X'04'               NEW NUMBER PRINTED                           
COMPLNQ  EQU   *-COMPD                                                          
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE ELEMENT X-REF TABLE                             *          
**********************************************************************          
LXRD     DSECT                                                                  
LXRNEW   DS    XL4                 A(NEW ELEMENT)                               
LXROLD   DS    XL4                 A(OLD ELEMENT)                               
LXRLNQ   EQU   *-LXRD                                                           
                                                                                
                                                                                
**********************************************************************          
* DSECT TO COVER THE PRINT ACTION TABLE                              *          
**********************************************************************          
PACTD    DSECT                                                                  
PACTACT  DS    CL2                 ACTION                                       
PACTPID  DS    XL2                 PRIMARY FILE ID                              
PACTPRN  DS    XL2                 PRIMARY RECORD NUMBER                        
PACTSID  DS    XL2                 SECONDARY FILE ID                            
PACTSRN  DS    XL2                 SECONDARY RECORD NUMBER                      
PACTSEQ  DS    XL2                 SEQUENCE FIELD                               
PACTXRE  DS    XL2                 X-REF ELEMENT                                
PACTLNQ  EQU   *-PACTD                                                          
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE PRINT RECORD INFO                               *          
**********************************************************************          
PRTD     DSECT                                                                  
         DS    XL1                                                              
PRTOLD   DS    CL5                 OLD NUMBER                                   
         DS    XL3                                                              
PRTNEW   DS    CL5                 NEW NUMBER                                   
         DS    XL2                                                              
PRTACT   DS    CL11                ACTION                                       
         DS    XL2                                                              
PRTHLNQ  EQU   *-PRTD                                                           
         DS    XL2                                                              
PRTREC   DS    XL(L'P-4-(*-PRTD))                                               
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DMRCVRDHR                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMRCVRHDR                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPWK02 06/16/08'                                      
         END                                                                    
