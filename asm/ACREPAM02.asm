*          DATA SET ACREPAM02  AT LEVEL 061 AS OF 01/08/19                      
*PHASE ACAM02A                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE CUREDIT                                                                
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
                                                                                
ACAM02A  TITLE 'TO SEARCH FOR ELM IN RECORDS LIKE TIME/ORD/COMPANY'             
                                                                                
***********************************************************************         
* **KEEP** TO FIND OUT AN ELEMENT IF IT IS PRESENT IN RECORD WHICH    *         
* DOES NOT HAVE RECORD TYPE & SUB-TYPE LIKE BELOW -                   *         
* QOPT3 = Y RUN ACROSS ENTIRE ACC FILE                             Y/N*         
* QOPT4 = Y UPDATE ORDERS WITH NEGATIVE PENDING ORDER COUNT        Y/N*         
* QOPT5 = Y UPDATE ORDERS WITH BLANK WORKCODE/POSTIVE PENDING CNT  Y/N*         
* QOPT7=Y DUMP RECORDS                                                *         
***********************************************************************         
                                                                                
ACAM02A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACAM**,R8,CLEAR=Y                                            
         L     RA,0(,R1)                                                        
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RA=A(WORK AREA SAVED AREA)                   
*                                                                               
         MVI   FCRDTRNS,NOQ                                                     
         MVI   FCRDHIST,NOQ                                                     
         MVI   FCRDACC,NOQ                                                      
         MVI   FCRDTIME,NOQ                                                     
         MVI   FCRDORD,NOQ                                                      
         MVI   FCRDEST,NOQ                                                      
         CLI   MODE,RUNLAST                                                     
         JE    RUNL                                                             
         J     EXIT                                                             
                                                                                
EXITL    CLI   *,FF                SET CC=LOW                                   
         J     EXIT                                                             
EXITH    CLI   *,0                 SET CC=HIGH                                  
         J     EXIT                                                             
EXITE    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LAST FOR RUN                                                        *         
***********************************************************************         
         SPACE 1                                                                
RUNL     XR    RF,RF                                                            
         LH    RF,=Y(IOAREA-WORKD)                                              
         LA    RE,WORKD                                                         
         AR    RF,RE                                                            
         ST    RF,AIOAREA                                                       
*                                                                               
         MVI   BYTE,0                                                           
         CLI   RCWRITE,YESQ        WRITING TO FILE?                             
         BNE   *+8                                                              
         OI    BYTE,X'80'                                                       
         XC    OPTFLG,OPTFLG                                                    
*                                                                               
RUNL10   CLI   QOPT2,YESQ          ARE WE WRITING TO TAPE INSTEAD?              
         BNE   RUNL15                                                           
         OPEN  (TAPEOUT,OUTPUT)                                                 
         TM    TAPEOUT+48,X'10'    TEST WAS OKAY?                               
         BO    RUNL15                                                           
         ABEND 206,DUMP                                                         
*                                                                               
RUNL15   CLI   QOPT4,YESQ                                                       
         JNE   *+8                                                              
         OI    OPTFLG,OPT4Q                                                     
         CLI   QOPT5,YESQ                                                       
         JNE   *+8                                                              
         OI    OPTFLG,OPT5Q                                                     
         TM    OPTFLG,BOTHQ                                                     
         JZ    RUNLX                                                            
*                                                                               
RUNL20   CLI   QOPT3,YESQ          ARE WE RUNNING ACROSS ENTIRE ACC             
         JNE   RUNL25                                                           
         XC    QCOMPANY,QCOMPANY   YES,INITIALIZE COMP CODE                     
RUNL25   XC    IOKEY,IOKEY         INITIALIZE KEY                               
         LA    R2,IOKEY            R2 = A(IOKEY)                                
*                                                                               
         USING PLINED,R6           PRINT LINE DSECT                             
         LA    R6,P                                                             
         MVC   PRTLINE(PLNQ),SPACES CLEAR PRINT LINE                            
         USING ORDRECD,R2          LIMIT LIST RECORD DSECT                      
*                                                                               
RUNL30   XC    ORDKEY,ORDKEY       INITIALIZE RECORD KEY                        
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,QCOMPANY    RUN ONLY FOR SELECTIVE COMP-CODE             
*                                                                               
RUNL40   GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,ORDKEY,ORDKEY,0                       
         BE    RUNL50                                                           
         DC    H'0'                                                             
*                                                                               
RUNL45   GOTOR DATAMGR,DMCB,DMRSEQ,ACCDIR,ORDKEY,ORDKEY,0                       
         BE    RUNL50                                                           
         DC    H'0'                                                             
*                                                                               
RUNL50   CLI   ORDKCPY,X'FE'       REACHED END OF DATABASE                      
         BH    RUNLX               EXIT THE PROGRAM                             
         CLI   ORDKTYP,ORDKTYPQ                                                 
         BH    RUNLX               PASSED END OF ORDER RECORDS                  
         CLI   QOPT3,YESQ          ARE WE RUNNING ACROSS ENTIRE ACC             
         JE    RUNL60              YES:READ NEXT                                
         CLC   ORDKCPY,QCOMPANY    CHECK TO RUN ONLY FOR SELECTIVE CMP          
         BH    RUNLX               IF HIGH PASSED COMPANY ORDER RECS            
*                                                                               
RUNL60   MVC   SVDA,ORDKDA                                                      
         GOTOR DATAMGR,DMCB,(BYTE,GETREC),ACCMST,SVDA,AIOAREA,DMWORK            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,ORDRFST                                                       
         USING OAMELD,R3                                                        
*                                                                               
RUNL70   CLI   OAMEL,0             RECORD END?                                  
         BE    RUNL45              READ NEXT RECORD                             
*                                                                               
         CLI   OAMEL,OAMELQ                                                     
         BNE   RUNL90                                                           
*                                                                               
RUNL75   DS    0H                  UPDATING OPTION 5?                           
         TM    OPTFLG,OPT5Q                                                     
         JZ    RUNL80                                                           
*                                                                               
         TM    OAMSTAT,OAMSXTRA    IS EXTRA ELEMENT BIT ON                      
         JZ    RUNL80              NO, FETCH NEXT                               
         CLC   OAMWORK,SPACES      WORKCODE CHECK TO SPACES                     
         JNE   RUNL80              NOT SPACES, FETCH NEXT                       
         CLI   OAMIPND,X'00'                                                    
         JE    RUNL80                                                           
*MN                                                                             
         TM    OAMSTAT,OAMSXTRA    NOT PART OF THE ORIGINAL ORDER               
         BZ    RUNL80                                                           
*MN                                                                             
         TM    OAMIPND,X'80'       OAMIPND > 0                                  
         JNO   RUNL85              NO, ZERO OUT PENDING INVOICE LOGS            
*                                                                               
RUNL80   DS    0H                                                               
         TM    OPTFLG,OPT4Q        UPDATING OPTION 4?                           
         JZ    RUNL90                                                           
*                                                                               
         TM    OAMIPND,X'80'       OAMIPND < 0                                  
         JNO   RUNL90              NO, FETCH NEXT ELEMENT                       
*                                                                               
RUNL85   MVC   SAVEPND,OAMIPND                                                  
*                                                                               
         XC    OAMIPND,OAMIPND                                                  
         B     RUNL100             YES:PRINT THE RECORD                         
*                                                                               
RUNL90   LLC   RE,OAMLN            GET NEXT ELEMENT                             
         AR    R3,RE                                                            
         B     RUNL70              VALIDATE NEXT ELEMENT                        
*                                                                               
RUNL100  CLI   QOPT2,YESQ          ARE WE WRITING TO TAPE INSTEAD?              
         BE    RUNL110             PRINT DATA                                   
         CLI   RCWRITE,YESQ        WRITING TO FILE?                             
         BNE   RUNL110             PRINT DATA                                   
*                                                                               
         GOTOR DATAMGR,DMCB,(BYTE,PUTREC),ACCMST,SVDA,AIOAREA,DMWORK            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RUNL110  GOTO1 HEXOUT,DMCB,ORDKCPY,PCMPY_CD,L'ORDKCPY                           
         MVC   P_ORDNO,ORDKORD                                                  
         MVC   P_WORKCD,OAMWORK                                                 
         GOTO1 HEXOUT,DMCB,SVDA,P_DA,L'SVDA                                     
         GOTO1 HEXOUT,DMCB,SAVEPND,P_PND,L'SAVEPND                              
         EDITR (P6,OAMINUM),P_INVNUM,0,ALIGN=LEFT,MINUS=YES,           X        
               ,ZERO=NOBLANK                                                    
         EDITR (P6,OAMIVAL),P_AMIVAL,0,ALIGN=LEFT,MINUS=YES,           X        
               ,ZERO=NOBLANK                                                    
         EDITR (P6,OAMAMNT),P_AMT,0,ALIGN=LEFT,MINUS=YES,ZERO=NOBLANK           
         GOTOR ACREPORT            WRITE REPORT WITH DETAIL LINE                
         B     RUNL90              FETCH NEXT ELEMENT                           
*                                                                               
RUNLX    B     EXIT                EXIT THE PROGRAM                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)            RESET RC                                     
         CLI   QOPT7,YESQ                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
K        EQU   1024                                                             
NOQ      EQU   C'N'                                                             
YESQ     EQU   C'Y'                                                             
FF       EQU   X'FF'                                                            
DELELQ   EQU   FF                                                               
MAXRECLN EQU   2000                                                             
GETREC   DC    C'GETREC  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ADDREC   DC    C'ADDREC  '                                                      
PUTREC   DC    C'PUTREC  '                                                      
WRITE    DC    C'DMWRT   '                                                      
WARNING  DC    C'**ELEMENTS IN DOESNT MATCH OUT!!**'                            
*                                                                               
PZERO    DC    P'0'                                                             
*                                                                               
***********************************************************************         
* ADDRESS CONSTANTS                                                   *         
***********************************************************************         
VRECTYP  DC    V(ACRECTYP)                                                      
VBINSRCH DC    V(BINSRCH)                                                       
VSORTER  DC    V(SORTER)                                                        
VHELLO   DC    V(HELLO)                                                         
ADUMP    DC    A(DUMP)                 PRINTABLE ROUTINE                        
PRNTBL   DC    V(PRNTBL)               DUMP                                     
VHEXOUT  DC    V(HEXOUT)                                                        
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=VB,LRECL=6024,BLKSIZE=32760                                
         DS    0F                                                               
*                                                                               
         DC    C'*BUFFREC'                                                      
BUFFREC  DS    0X           BUFFER AREA                                         
         DS    XL4                                                              
BUFFKEY  DS    XL(2*K)                                                          
***********************************************************************         
WORKD    DSECT                                                                  
MSG      DS    CL20         MESSAGE FOR DUMP                                    
HELLO    DS    A                                                                
*                                                                               
AIOAREA  DS    A            ADDRESS OF IO AREA                                  
SVDA     DS    XL4          SAVED DISK ADDRESS                                  
SEQNO    DS    H            SEQUENCE NO.                                        
SAVEREF  DS    CL6          SAVED EXPENSE NO.                                   
*                                                                               
MYBYTE1  DS    CL1                                                              
RUNIND   DS    XL1          RUN INDICATOR                                       
RUNINI   EQU   X'80'        INITIALISED TSAR                                    
SAVECSQ  DS    CL1          SAVED CIDEL SEQUENCE                                
         DS    0H                                                               
SAVELEN  DS    XL2          SAVED TOTAL CIDEL LENGTH                            
*                                                                               
         DS    0H                                                               
ELEMENT  DS    XL256                                                            
OPTFLG   DS    XL1          RUN OPTION SETTINGS                                 
OPT4Q    EQU   X'80'        PENDING ORDER=NEGATIVE                              
OPT5Q    EQU   X'40'        PENDING ORDER=POSITIVE, BLANK WORKCODE              
BOTHQ    EQU   X'C0'                                                            
*AVECMCD DS    XL1          SAVE COMPANY CODE                                   
SAVEPND  DS    XL1          SAVE PENDING ORDER COUNT                            
SAVEKEY  DS    XL64         SAVE KEY                                            
TSARBLK  DS    XL(TSPXTNL)                                                      
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                                                             
IOAREA   DS    CL2000                                                           
IOLNQ    EQU   *-IO                                                             
*                                                                               
         DS    XL2                                                              
WORKX    EQU   *-WORKD                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLINE  DS    0C                                                               
         DS    CL4                                                              
PCMPY_CD DS    CL2                                                              
         DS    CL7                                                              
P_ORDNO  DS    CL6                                                              
         DS    CL7                                                              
P_WORKCD DS    CL2                                                              
         DS    CL11                                                             
P_PND    DS    CL2                                                              
         DS    CL7                                                              
P_AMT    DS    CL12                                                             
P_INVNUM DS    CL12                                                             
         DS    CL1                                                              
P_AMIVAL DS    CL12                                                             
         DS    CL2                                                              
P_DA     DS    CL8                                                              
         DS    CL2                                                              
PLNQ     EQU   *-PRTLINE           LENGTH OF DSECT                              
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
* DMLDDEFN                                                                      
*        PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACRECEQUS                                                                     
*        PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
*        PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
*        PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
*        PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
*        PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDCOREQUS                                                                     
*        PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDTSARD                                                                       
*        PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
* DDCOMFACS                                                                     
*        PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061ACREPAM02 01/08/19'                                      
         END                                                                    
