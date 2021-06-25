*          DATA SET ACCLB14    AT LEVEL 024 AS OF 08/16/00                      
*PHASE T62114A                                                                  
CLB14    TITLE '- FORMAT OPTIONS'                                               
CLB14    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB14*,R8,R7,CLEAR=YES,RR=RE                                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         USING OVERWRKD,RC                                                      
         USING FBLKD,OFBLK                                                      
         ST    RE,BORELO                                                        
         L     R5,ALSVALS                                                       
         LH    R6,=Y(BSDICT-TWAD)                                               
         LA    R6,TWAD(R6)                                                      
         USING BSDICT,R6                                                        
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     LSTFRST             FIRST FOR THIS LIST                          
         B     SCRFRST             FIRST FOR THIS SCREEN                        
         B     SCRLAST             LAST FOR THIS SCREEN                         
         B     EXITY               FIRST FOR VALIDATE THIS LINE                 
         B     VALCLM              VALIDATE COLUMN                              
         B     EXITY               LAST FOR VALIDATE THIS LINE                  
         B     DISCLM              DISPLAY COLUMN                               
         B     GETFRST             GET FIRST RECORD FOR LIST                    
         B     GETNEXT             GET NEXT RECORD                              
         B     EXIT                SET UP MY OWN HEADING                        
         B     EXIT                VALIDATE SELECT TABLE                        
         B     EXIT                DISPLAY COLUMN TOTAL                         
         EJECT                                                                  
***********************************************************************         
* FIRST FOR THIS SCREEN                                               *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  DS    0H                                                               
*                                                                               
         TM    BCINDS1,BCINACT     TEST FIRST TIME IN                           
         BO    *+12                                                             
         TM    BCINDS2,BCINTRS                                                  
         BZ    SFRST20                                                          
*                                                                               
         CLI   CUCTRY,CTRYGER      TEST NOT IN GERMANY                          
         BE    SFRST02                                                          
         XC    FOSLNGW,FOSLNGW     REMOVE LANGUAGE FIELD                        
         XC    FOSLNGX,FOSLNGX                                                  
         OI    FOSLNGH+FHATD,FHATPR                                             
         OI    FOSLNGH+FHIID,FHIIVA                                             
*                                                                               
SFRST02  CLI   TLACT,ACTLFT        TEST SELECTION FROM FORMAT LIST              
         BE    SFRST10                                                          
         CLI   CSFORMAT,0          TEST HAVE BILLING FORMAT CODE                
         BNE   SFRST04                                                          
         LA    RF,FOSFMTH          ASK USER TO ENTER THE KEY                    
         ST    RF,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         B     EXITL                                                            
*                                                                               
         PUSH  USING                                                            
         USING PBCRECD,IOKEY       DISPLAY CURRENT BILLING FORMAT               
SFRST04  XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVC   PBCKFMT,CSFORMAT                                                 
         MVC   PBCKLANG,CSFMLANG                                                
         GOTO1 DISKEY,BOPARM,PBCKEY                                             
         NI    FOSFMTH+FHIID,FF-FHIIVA                                          
         B     SFRST20                                                          
         POP   USING                                                            
*                                                                               
SFRST10  MVC   LSFMTDA,TLDA                                                     
         MVC   IODAOVER,LSFMTDA                                                 
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DISKEY,BOPARM,AIO1                                               
         B     SFRST30                                                          
*                                                                               
SFRST20  GOTO1 VALKEY              VALIDATE THE KEY                             
         BNE   SCRFRSTN                                                         
*                                                                               
SFRST30  L     RE,=A(OPTTAB)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS    SET OUTPUT BASE ADDRESS                      
         MVC   SFLTOPS,LSOPS                                                    
*                                                                               
         L     RE,=A(ALOVAL)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
*                                                                               
         MVC   SLSGENM,LSGENM      SAVE COLUMN VALUES                           
         MVC   SCLMHEAD,ACLMHEAD                                                
         MVC   SCLMDATA,ACLMDATA                                                
         GOTO1 ASETCLM,BOPARM,('CLMLFTQ',0)                                     
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         OC    BOWORK2(L'GOCBDFMT),GOCBDFMT-GOBBLOCK(RF)                        
         BNZ   *+10                                                             
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
         GOTO1 AVALOPT,BOPARM,OPTTAB,BOWORK2,0                                  
         BNE   EXITL                                                            
*                                                                               
         MVC   LSFMTDIS,LSCLM                                                   
         XC    LSCLM,LSCLM         SET UP DIS=1 LIST                            
         MVI   LSCLMN,1                                                         
         MVI   LSCLMLST,C' '                                                    
         MVI   LSCLMLST,C'1'-LSCLMPRO                                           
*                                                                               
         MVC   LSGENM,SLSGENM      RESTORE COLUMN VALUES                        
         MVC   ACLMHEAD,SCLMHEAD                                                
         MVC   ACLMDATA,SCLMDATA                                                
         BNE   SCRFRSTN                                                         
*                                                                               
         MVC   IODAOVER,LSFMTDA                                                 
         LH    R1,=Y(IO4)                                                       
         GOTO1 AIO,IOACCMST+IOGETRUP(R1)                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFMTBLK,BOPARM,('FBGET',FBLKD),AIO4                              
*                                                                               
         CLC   SFLTOPS,LSOPS                                                    
         BE    EXITY                                                            
         B     EXITH               FILT OPTIONS CHANGED - RESTART               
*                                                                               
SCRFRSTN MVC   LSOPS(L'SFLTOPS),LSOPS   RESTORE OLD FILTERS IF ERROR            
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* LAST FOR SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  DS    0H                                                               
         TM    LSINDS1,LSILINUP    TEST ANY LINES CHANGED                       
         BZ    EXITY                                                            
         TM    LSINDS1,LSILINER    TEST FOR ANY ERRORS                          
         BO    EXITY                                                            
         GOTO1 AFMTBLK,BOPARM,('FBPUT',FBLKD),AIO4                              
         LH    R1,=Y(IO4)                                                       
         GOTO1 AIO,IOACCMST+IOPUT(R1)                                           
         BE    EXITY                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE THE FORMAT KEY                                  *         
*                                                                     *         
* EXIT:   IOKEY = KEY                                                 *         
*       LSFMTDA = D/A                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   NTR1  ,                                                                
         TM    FOSFMTH+FHIID,FHIIVA                                             
         BZ    *+12                                                             
         TM    FOSLNGH+FHIID,FHIIVA                                             
         BO    EXITY               BOTH FIELDS VALIDATED - EXIT OKAY            
         OI    BCINDS2,BCIHLDLP                                                 
*                                                                               
         PUSH  USING                                                            
         USING PBCRECD,IOKEY       INITIALIZE KEY                               
         XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
*                                                                               
         MVI   FVMINL,1            VALIDATE FORMAT NUMBER                       
         GOTO1 AFVAL,FOSFMTH                                                    
         BNE   EXITN                                                            
         TM    FVIIND,FVINUM                                                    
         BZ    VKEY02                                                           
         OC    BCFULL(3),BCFULL                                                 
         BNZ   VKEY02                                                           
         CLI   BCFULL+3,0                                                       
         BNE   *+14                                                             
VKEY02   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITN                                                            
         MVC   PBCKFMT,BCFULL+3                                                 
*                                                                               
         TM    FOSLNGH+FHATD,FHATPR  TEST LANGUAGE PROTECTED                    
         BO    VKEY10                                                           
         MVI   FVILEN,1                                                         
         GOTO1 AFVAL,FOSLNGH+FHOID                                              
         BNE   EXITN                                                            
*                                                                               
         L     R3,ALANG                                                         
         LA    R3,6(R3)                                                         
         USING LANGTABD,R3                                                      
VKEY04   CLI   LANGTABD,FF                                                      
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFELANG)                                           
         B     EXITN                                                            
         GOTO1 CMPWRD,LANGSHR                                                   
         BE    VKEY06                                                           
         GOTO1 CMPWRD,LANGSHRN                                                  
         BE    VKEY06                                                           
         GOTO1 CMPWRD,LANGFUL                                                   
         BE    VKEY06                                                           
         GOTO1 CMPWRD,LANGFULN                                                  
         BE    VKEY06                                                           
         LA    R3,LANGTABL(R3)                                                  
         B     VKEY04                                                           
*                                                                               
VKEY06   MVC   PBCKLANG,LANGCODE                                                
         CLC   PBCKLANG,CULANG                                                  
         BNE   *+12                                                             
         MVI   PBCKLANG,0                                                       
         B     VKEY10                                                           
         CLI   PBCKLANG,LANGENG                                                 
         BNE   VKEY10                                                           
         MVI   PBCKLANG,LANGEUK                                                 
         DROP  R3                                                               
*                                                                               
VKEY10   GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    VKEY12                                                           
         LA    RF,FOSFMTH                                                       
         ST    RF,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITN                                                            
*                                                                               
VKEY12   MVC   LSFMTDA,PBCKDA                                                   
         GOTO1 DISKEY,BOPARM,PBCRECD                                            
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISLAY THE KEY                                           *         
*                                                                     *         
* NTRY: P1 = A(KEY)                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING PBCRECD,R2                                                       
*                                                                               
         EDIT  PBCKFMT,(3,FOSFMT),0,ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1           
         OI    FOSFMTH+FHOID,FHOITR                                             
         OI    FOSFMTH+FHIID,FHIIVA                                             
*                                                                               
         TM    FOSLNGH+FHATD,FHATPR                                             
         BO    DISKEYX                                                          
         L     R3,ALANG                                                         
         LA    R3,6(R3)                                                         
         MVC   BOBYTE1,PBCKLANG                                                 
         CLI   BOBYTE1,0                                                        
         BNE   *+14                                                             
         MVC   BOBYTE1,CULANG                                                   
         B     DKEY02                                                           
         CLI   BOBYTE1,LANGEUK                                                  
         BNE   DKEY02                                                           
         MVI   BOBYTE1,LANGENG                                                  
         USING LANGTABD,R3                                                      
DKEY02   CLI   LANGTABD,FF                                                      
         BE    EXITN                                                            
         CLC   LANGCODE,BOBYTE1                                                 
         BE    DKEY10                                                           
         LA    R3,LANGTABL(R3)                                                  
         B     DKEY02                                                           
*                                                                               
DKEY10   MVC   FOSLNG,LANGFULN                                                  
         OI    FOSLNGH+FHOID,FHOITR                                             
         OI    FOSLNGH+FHIID,FHIIVA                                             
         DROP  R3                                                               
*                                                                               
DISKEYX  B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                  *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   DS     0H                                                              
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     DISHED      00      HEADING                                      
         B     DISOPT      01      OPTION                                       
         SPACE 1                                                                
***********************************************************************         
* DISPLAY HEADING                                                     *         
***********************************************************************         
         SPACE 1                                                                
DISHED   MVC   FVIFLD(L'TLFHED),TLFHED                                          
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OPTION                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISOPT   XR    RF,RF                                                            
         IC    RF,TLFOPT#                                                       
         GOTO1 AFMTBLK,BOPARM,('FBDIS',FBLKD),(RF)                              
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLFMAXL          RF=REQUIRED LENGTH OF FIELD                  
         LA    RF,FHDAD+FHDAD(RF)                                               
         STC   RF,FVIHDR+FHLND                                                  
         MVC   FVIHDR+FHATD(L'FHAT),TLFFHAT                                     
         OI    FVIHDR+FHATD,FHATXH                                              
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                 *         
***********************************************************************         
         SPACE 1                                                                
VALCLM   DS     0H                                                              
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         DC    2H'0'       00      HEADING                                      
         B     VALOPT      01      OPTION                                       
         SPACE 1                                                                
***********************************************************************         
* VALIDATE OPTOIN                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALOPT   XR    RF,RF                                                            
         IC    RF,TLFOPT#                                                       
         GOTO1 AFMTBLK,BOPARM,('FBVAL',FBLKD),(RF)                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  DS    0H                                                               
         MVC   SLSGENM,LSGENM      SAVE COLUMN VALUES                           
         MVC   SCLMHEAD,ACLMHEAD                                                
         MVC   SCLMDATA,ACLMDATA                                                
         GOTO1 ASETCLM,BOPARM,('CLMLFTQ',0)                                     
         MVI   SFMT#,0                                                          
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* GET FIRST/NEXT LIST RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
GETFRST  DS    0H                                                               
GETNEXT  CLC   LSFMTN,SFMT#                                                     
         BE    GETNEXTN                                                         
         XR    RF,RF                                                            
         IC    RF,SFMT#                                                         
         LA    RE,1(RF)                                                         
         STC   RE,SFMT#                                                         
         SLL   RF,1                                                             
         LA    RF,LSFMTLST(RF)                                                  
         GOTO1 ASETCLM,BOPARM,(RF)                                              
         BNE   GETNEXT             ?? (READ ONLY COLS??)                        
         L     R2,ACLMDATA                                                      
         USING CLMTABD,R2                                                       
         CLI   CLMRTN,64                                                        
         BNL   GETNEXT                                                          
*                                                                               
         XR    R3,R3                                                            
         IC    R3,CLMRTN                                                        
         MH    R3,=Y(FOPTABL)                                                   
         A     R3,AFOPTAB                                                       
         USING FOPTABD,R3                                                       
         MVC   TLFOPT#,CLMRTN                                                   
         MVC   TLFMAXL,CLMFWDTH                                                 
         MVC   TLFFHAT,CLMFFHAT                                                 
*                                                                               
         MVI   TLFHED,AC#ESCL                                                   
         MVC   TLFHED+1(L'FOPDIC),FOPDIC                                        
         MVI   TLFHED+3,L'TLFHED                                                
         GOTO1 VDICTAT,BOPARM,C'SL  ',TLFHED                                    
*                                                                               
GETNEXTY B     EXITY                                                            
         SPACE 1                                                                
GETNEXTN MVC   LSGENM,SLSGENM      RESTORE COLUMN VALUES                        
         MVC   ACLMHEAD,SCLMHEAD                                                
         MVC   ACLMDATA,SCLMDATA                                                
         B     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* COMPARE INPUT WITH SOME KIND OF A WORD                              *         
*                                                                     *         
* NTRY: R1=A(WORD)                                                    *         
***********************************************************************         
         SPACE 1                                                                
CMPWRD   LA    RF,2                                                             
         CLI   FVXLEN,2                                                         
         BH    *+8                                                              
         IC    RF,FVXLEN                                                        
         EX    RF,*+6                                                           
         BR    RE                                                               
         CLC   FVIFLD(0),0(R1)                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT POINTS                                                 *         
***********************************************************************         
         SPACE 1                                                                
EXITY    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,FF                                                             
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DEFCLM   DS    0XL1                ** DEFAULT COLUMN LIST **                    
         DC    AL1(LFT#01)                                                      
         DC    AL1(LFT#02)                                                      
         DC    AL1(LFT#03)                                                      
         DC    AL1(LFT#04)                                                      
         DC    AL1(LFT#05)                                                      
         DC    AL1(LFT#06)                                                      
         DC    AL1(LFT#07)                                                      
         DC    AL1(LFT#23)                                                      
         DC    AL1(LFT#08)                                                      
         DC    AL1(LFT#09)                                                      
         DC    AL1(LFT#10)                                                      
         DC    AL1(LFT#11)                                                      
         DC    AL1(LFT#12)                                                      
         DC    AL1(LFT#18)                                                      
         DC    AL1(LFT#19)                                                      
         DC    AL1(LFT#22)                                                      
         DC    AL1(LFT#28)                                                      
         DC    AL1(LFT#13)                                                      
         DC    AL1(LFT#24)                                                      
         DC    AL1(LFT#25)                                                      
         DC    AL1(LFT#26)                                                      
         DC    AL1(LFT#27)                                                      
         DC    AL1(LFT#14)                                                      
         DC    AL1(LFT#15)                                                      
         DC    AL1(LFT#16)                                                      
         DC    AL1(LFT#17)                                                      
         DC    AL1(LFT#20)                                                      
         DC    AL1(LFT#21)                                                      
         DC    AL1(EOT)                                                         
DEFCLML  EQU   *-DEFCLM                                                         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
***********************************************************************         
* OPTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
OPTTAB   DS    0X                                                               
*                                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,50,0)                                            
         DC    AL1(1)                                                           
         DC    AL2(OPTDISQ,0)                                                   
         DC    CL4'++'                                                          
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTION INPUT                                             *           
*********************************************************************           
         SPACE 1                                                                
         DS    0H                                                               
         DROP                                                                   
ALOVAL   NMOD1 0,**ALOV**,CLEAR=YES                                             
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         DC    H'0'                                                             
*        B     *(RF)                                                            
         SPACE 1                                                                
VALX     XMOD1                                                                  
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBCOLS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FACTRY                                                                        
         PRINT OFF                                                              
       ++INCLUDE FACTRY                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* FALANG                                                                        
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBECD                                                       
         SPACE 1                                                                
         ORG   OSVALS                                                           
*                                                                               
SFLTOPS  DS    CL(LSFLTOPL)        SAVED FILTERING OPTIONS                      
*                                                                               
         DS    (OSVALSL-(*-OSVALS))X                                            
         SPACE 1                                                                
LSVALSD  DSECT                                                                  
         ORG   LSOPS                                                            
LSFMTDA  DS    AL4                 FORMAT RECORD DISK-ADDRESS                   
LSFMTDIS DS    0XL(L'LSCLM)        FORMAT DIS=                                  
LSFMTN   DS    XL1                 NUMBER IN LIST                               
LSFMTLST DS    (LSCLMMAX)XL2                                                    
         SPACE 1                                                                
*                                                                               
OVERWRKD DSECT                                                                  
OFBLK    DS    XL(FBLKL)                                                        
SFMT#    DS    XL1                                                              
SLSGENM  DS    XL(L'LSGENM)                                                     
SCLMHEAD DS    XL(L'ACLMHEAD)                                                   
SCLMDATA DS    XL(L'ACLMDATA)                                                   
         DS    0X                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACCLB14   08/16/00'                                      
         END                                                                    
