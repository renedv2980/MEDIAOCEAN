*          DATA SET DDDECON    AT LEVEL 002 AS OF 09/03/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE DDDECONA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
         ENTRY SSB                                                              
         ENTRY UTL                                                              
********************************************************************            
*                                                                               
* CREATE A RECOVERY TAPE FROM TWO INPUT FILE TAPES                              
*                                                                               
********************************************************************            
         TITLE 'DDDECON - BUILD A RECOVERY TAPE'                                
DECON    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**DECON,=V(REGSAVE)                                            
         L     R7,=V(CPRINT)                                                    
         USING DPRINT,R7                                                        
         LARL  R8,GLOBALS                                                       
*                                                                               
         BRAS  RE,CARD                                                          
         LLC   RF,KEYLEN                                                        
         SHI   RF,1                                                             
         STC   RF,KEYLEN          EX LENGTH                                     
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,ISRECLN                                                     
         AHI   RF,4                                                             
         STC   RF,INDIRLEN        Indirect key                                  
*                                                                               
         LA    R4,INOLD                                                         
         LA    R5,INNEW                                                         
         LA    R6,OUT                                                           
         OPEN  ((R4),INPUT,(R5),INPUT,(R6),OUTPUT)                              
*                                                                               
         L     R4,=A(OREC)         R4=OLD RECORD                                
         L     R5,=A(NREC)         R5=NEW RECORD                                
*                                                                               
         XC    RLEN(RREC-RLEN),RLEN                                             
         MVC   RDATE,DATE                                                       
         B     GETBOTH                                                          
         EJECT                                                                  
**********************************************************************          
* READ OLD AND NEW FILES, COMPARE AND GENERATE RECOVERY FILE                    
**********************************************************************          
GETBOTH  LA    R1,INOLD                                                         
         LR    R0,R4                                                            
         LA    R9,GETNEW           EOF RETURN                                   
         GET   (R1),(R0)                                                        
         AP    FILOIN,=P'1'                                                     
*                                                                               
         MVI   ISINDIRO,NO                                                      
         LLC   RF,INDIRLEN                                                      
         CH    RF,0(R4)                                                         
         JL    *+8                 DIE FOR TESTING                              
         MVI   ISINDIRO,YES                                                     
*                                                                               
GETNEW   LA    R1,INNEW                                                         
         LR    R0,R5                                                            
         LA    R9,COMPARE          EOF RETURN                                   
         GET   (R1),(R0)                                                        
         AP    FILNIN,=P'1'                                                     
*                                                                               
         MVI   ISINDIRN,NO                                                      
         LLC   RF,INDIRLEN                                                      
         CH    RF,0(R5)                                                         
         JL    *+8                 DIE FOR TESTING                              
         MVI   ISINDIRN,YES                                                     
         B     COMPARE                                                          
*                                                                               
GETOLD   LA    R1,INOLD                                                         
         LR    R0,R4                                                            
         LA    R9,COMPARE          EOF RETURN                                   
         GET   (R1),(R0)                                                        
         AP    FILOIN,=P'1'                                                     
*                                                                               
         MVI   ISINDIRO,NO                                                      
         LLC   RF,INDIRLEN                                                      
         CH    RF,0(R4)                                                         
         JL    *+8                 DIE IF IS FOR TESTING                        
         MVI   ISINDIRO,YES                                                     
*                                                                               
COMPARE  LLC   RF,KEYLEN                                                        
         EX    RF,CLCKEY                                                        
*                                                                               
         BH    INSERT              OLD HIGH, INSERT NEW                         
         BL    DELETE              OLD LOW, DELETE OLD                          
*                                                                               
SAME     EX    RF,CLCKEYFF                                                      
         BE    OPDONE              YES, ALL DONE                                
*                                                                               
SAME1    LA    RF,15                                                            
         EX    RF,CLCKEY00                                                      
         BE    GETBOTH             First record on tape                         
*                                                                               
SAME2    AP    FILEQU,=P'1'                                                     
         CLC   0(2,R4),0(R5)       SEE IF LENGTH CHANGED                        
         BNE   CHANGE              YES, RECORD CHANGED                          
         LR    R0,R4               SET UP FOR CLCL                              
         LH    R1,0(R4)            GET LENGTH                                   
         LR    RE,R5                                                            
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    GETBOTH             SAME, GET NEXT PAIR                          
*                                                                               
CHANGE   AP    FILEQC,=P'1'                                                     
         AP    MYSEQ,=P'100'       SET SEQUENCE NUMBER                          
         MVC   RSIN,MYSEQ                                                       
         MVC   RTIME,MYSEQ                                                      
         LA    R0,RREC             SET UP TO WRITE OLD PRE-AMEND (COPY)         
         LH    R1,0(R4)            GET LENGTH                                   
         SH    R1,=Y(4)            RECORD LENGTH SANS PREFIX                    
         LA    RE,RREC-RLEN(,R1)   TOTAL LENGTH OF RECOVERY RECORD              
         STH   RE,RLEN                                                          
         LA    RE,4(,R4)                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   RFILTY,FILEDA                                                    
         CLI   ISINDIRO,YES                                                     
         JNE   *+10                                                             
         MVC   RFILTY,FILEIS                                                    
         MVI   RRECTY,1            1=COPY, 2=CHANGE, 3=ADD                      
         LA    R1,OUT              WRITE RECOVERY RECORD                        
         LA    R0,RLEN                                                          
         PUT   (R1),(R0)                                                        
         AP    RCVCPY,=P'1'                                                     
         AP    FILOUT,=P'1'                                                     
         LA    R0,RREC             SET UP TO WRITE NEW (CHANGE)                 
         LH    R1,0(R5)            GET LENGTH                                   
         SH    R1,=Y(4)            RECORD LENGTH SANS PREFIX                    
         LA    RE,RREC-RLEN(,R1)   TOTAL LENGTH OF RECOVERY RECORD              
         STH   RE,RLEN                                                          
         LA    RE,4(,R5)                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVI   RRECTY,2            1=COPY, 2=CHANGE, 3=ADD                      
         LA    R1,OUT              WRITE RECOVERY RECORD                        
         LA    R0,RLEN                                                          
         PUT   (R1),(R0)                                                        
         AP    RCVCHG,=P'1'                                                     
         AP    FILOUT,=P'1'                                                     
         B     GETBOTH             GET NEXT PAIR                                
*                                                                               
DELETE   AP    FILNHI,=P'1'                                                     
         LLC   RF,DSPDELEL         DISPLACMENT TO DELETE INDICATOR              
         LA    RF,4(R4,RF)                                                      
         TM    0(RF),X'80'         TEST ALREADY DELETED                         
         BZ    DELETE2             NO, SKIP                                     
         AP    FILNHD,=P'1'                                                     
         B     GETOLD              YES, IGNORE                                  
*                                                                               
DELETE2  AP    MYSEQ,=P'100'       SET SEQUENCE NUMBER                          
         MVC   RSIN,MYSEQ                                                       
         MVC   RTIME,MYSEQ                                                      
         LA    R0,RREC             SET UP TO WRITE OLD PRE-AMEND (COPY)         
         LH    R1,0(R4)            GET LENGTH                                   
         SH    R1,=Y(4)            RECORD LENGTH SANS PREFIX                    
         LA    RE,RREC-RLEN(,R1)   TOTAL LENGTH OF RECOVERY RECORD              
         STH   RE,RLEN                                                          
         LA    RE,4(,R4)                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   RFILTY,FILEDA                                                    
         CLI   ISINDIRO,YES                                                     
         JNE   *+10                                                             
         MVC   RFILTY,FILEIS                                                    
         MVI   RRECTY,1            1=COPY, 2=CHANGE, 3=ADD                      
         LA    R1,OUT              WRITE RECOVERY RECORD                        
         LA    R0,RLEN                                                          
         PUT   (R1),(R0)                                                        
*                                                                               
         AP    RCVCPY,=P'1'        Copy                                         
         AP    FILOUT,=P'1'                                                     
         LA    R2,RREC                                                          
         LLC   RF,DSPDELEL                                                      
         LA    R2,4(RF,R2)                                                      
         OI    0(R2),X'80'         SET NEW (CHANGE)=OLD DELETED   00133         
*        OI    RREC+RECSTAT-RECKEY,X'80' SET NEW (CHANGE)=OLD DELETED           
         MVC   RFILTY,FILEDA                                                    
         MVI   RRECTY,2            1=COPY, 2=CHANGE, 3=ADD                      
         LA    R1,OUT              WRITE RECOVERY RECORD                        
         LA    R0,RLEN                                                          
         PUT   (R1),(R0)                                                        
         AP    RCVCHG,=P'1'        Change                                       
         AP    FILOUT,=P'1'                                                     
         B     GETOLD              GET NEXT OLD                                 
*                                                                               
INSERT   AP    FILOHI,=P'1'                                                     
         AP    MYSEQ,=P'100'       SET SEQUENCE NUMBER                          
         MVC   RSIN,MYSEQ                                                       
         MVC   RTIME,MYSEQ                                                      
         LA    R0,RREC             SET UP TO WRITE NEW (ADD)                    
         LH    R1,0(R5)            GET LENGTH                                   
         SH    R1,=Y(4)            RECORD LENGTH SANS PREFIX                    
         LA    RE,RREC-RLEN(,R1)   TOTAL LENGTH OF RECOVERY RECORD              
         STH   RE,RLEN                                                          
         LA    RE,4(,R5)                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   RFILTY,FILEDA                                                    
         CLI   ISINDIRN,YES                                                     
         JNE   *+10                                                             
         MVC   RFILTY,FILEIS                                                    
         MVI   RRECTY,3            1=COPY, 2=CHANGE, 3=ADD                      
         LA    R1,OUT              WRITE RECOVERY RECORD                        
         LA    R0,RLEN                                                          
         PUT   (R1),(R0)                                                        
         AP    RCVADD,=P'1'                                                     
         AP    FILOUT,=P'1'                                                     
         B     GETNEW              GET NEXT NEW                                 
         EJECT                                                                  
**********************************************************************          
* SUBROUTINES ETC                                                               
**********************************************************************          
ENDINO   DS    0H                  END OF OLD FILE INPUT                        
         LLC   RF,KEYLEN                                                        
         EX    RF,MVCOLD                                                        
         BR    R9                                                               
*                                                                               
ENDINN   DS    0H                  END OF NEW FILE INPUT                        
         LLC   RF,KEYLEN                                                        
         EX    RF,MVCNEW                                                        
         BR    R9                                                               
*                                                                               
OPDONE   LA    R4,INOLD                                                         
         LA    R5,INNEW                                                         
         LA    R6,OUT                                                           
         CLOSE ((R4),,(R5),,(R6))                                               
*                                                                               
         MVC   TITLE(7),FILNAME                                                 
         MVC   TITLE+7(12),=C' RECONSTRUCT'                                     
         LA    R3,CNTRS               A(Start of counters)                      
         LA    R4,CNTRSLNQ            Table entry length                        
         LA    R5,CNTRSX              A(end of counters)                        
*                                                                               
OPD2     MVC   SPACING,=C'BL02'                                                 
         OI    5(R3),X'0F'                                                      
         UNPK  P+1(10),0(6,R3)                                                  
         MVC   P+13(14),6(R3)                                                   
         GOTO1 PRINTER                                                          
         BXLE  R3,R4,OPD2                                                       
         GOTO1 PRINTER                                                          
         XBASE                                                                  
         LTORG                                                                  
                                                                                
**********************************************************************          
* Read input cards                                                              
**********************************************************************          
CARD     NTR1  BASE=*,LABEL=*                                                   
CARDNXT  SAM24                                                                  
         GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   =C'XX',C                                                         
         JE    CARDEND             DONE                                         
         CLC   =C'/*',C                                                         
         JE    CARDEND             DONE                                         
         CLI   C,C'*'                                                           
         JE    CARDNXT             COMMENT                                      
         CLC   =C'DDSIO=',C                                                     
         JE    CARDDSIO                                                         
         CLC   =C'DSPACE=',C                                                    
         JE    CARDSPAC                                                         
         CLC   =C'FILE=',C                                                      
         JE    CARDFILE                                                         
         CLC   =C'DATE=',C                                                      
         JE    CARDDATE                                                         
         J     CARDNXT                                                          
*                                                                               
CARDDSIO L     R2,=V(DDSIO)                                                     
         MVC   0(8,R2),C+6         SAVE VERSION OF DDSIO TO LOAD                
         J     CARDNXT                                                          
*                                                                               
CARDSPAC L     R2,=V(SSB)                                                       
         USING SSBD,R2                                                          
         MVC   SSODSPAC,C+7        SET DSPACE VALUE                             
         J     CARDNXT                                                          
         DROP  R2                                                               
*                                                                               
CARDDATE DS    0H                                                               
         GOTOR =V(DATCON),DMCB,(0,C+5),(3,DATE),0,0                             
         J     CARDNXT                                                          
*                                                                               
CARDFILE DS    0H                                                               
CARDPAIR MVC   FILNAME,C+5                                                      
         CLC   =C'ACCFIL',FILNAME                                               
         JNE   *+10                                                             
         MVC   FILNAME,=CL7'ACCMST'                                             
*                                                                               
         ICM   R3,15,VFILETAB                                                   
         JNZ   CARDNXT             Already set                                  
         GOTOR =V(DMFATABS),DMCB                                                
         MVC   VFILETAB,4(R1)                                                   
         L     R3,VFILETAB                                                      
         USING FILTABD,R3                                                       
CARDPA10 CLI   DMFLNUM,X'FF'       End of table                                 
         JE    *+2                 Not a valid entry                            
         CLC   DMFLNAME,FILNAME                                                 
         JE    CARDPA20                                                         
         AHI   R3,DMFLLEN          Next entry                                   
         J     CARDPA10                                                         
*                                                                               
CARDPA20 TM    DMFLTYP,DMFLDA      DA or IS                                     
         BZ    CARDPA25            Must be IS                                   
         ST    R3,@DAFILE          Save DA entry                                
         MVC   FILEDA,DMFLNUM      Must be DA                                   
         MVC   FILEIS,DMFLPAIR     IS number                                    
         MVC   KEYLEN,DMFLKEYL     Key length                                   
         MVC   DSP1STEL,DMFLELD    Disp to 1st elem                             
         MVC   DSPDELEL,DMFLDELD   Disp to logical delete                       
         XR    R3,R3                                                            
         ICM   R3,1,FILEIS                                                      
         JZ    CARDNXT                                                          
         MHI   R3,DMFLLEN                                                       
         A     R3,VFILETAB                                                      
         ST    R3,@ISFILE          Save IS entry                                
         MVC   ISRECLN,DMFLMINI    Save IS record length                        
         J     CARDNXT                                                          
*                                                                               
CARDPA25 ST    R3,@ISFILE                                                       
         MVC   FILEIS,DMFLNUM      Must be IS                                   
         MVC   FILEDA,DMFLPAIR     DA number                                    
         MVC   KEYLEN,DMFLKEYL     Key length                                   
         MVC   ISRECLN,DMFLMINI    Save IS record length                        
         MVC   DSP1STEL,DMFLELD    Disp to first elem                           
         MVC   DSPDELEL,DMFLDELD   Disp to logical delete                       
         XR    R3,R3                                                            
         ICM   R3,1,FILEDA                                                      
         JZ    CARDNXT                                                          
         MHI   R3,DMFLLEN          Point to DA entry                            
         A     R3,VFILETAB                                                      
         ST    R3,@DAFILE                                                       
         MVC   DSP1STEL,DMFLELD    Use DA instead, disp to first elem           
         MVC   DSPDELEL,DMFLDELD   Use DA instead, disp to logical del          
         J     CARDNXT                                                          
*                                                                               
CARDEND  XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* GLOBAL VALUES                                                                 
***********************************************************************         
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
                                                                                
GLOBALS  DS    0D                                                               
CNTRS    DS    0D                                                               
FILNIN   DC    PL6'0',CL14'NEW FILE IN'                                         
CNTRSLNQ EQU   *-CNTRS                                                          
FILOIN   DC    PL6'0',CL14'OLD FILE IN'                                         
FILNHI   DC    PL6'0',CL14'NEW HIGH, DEL'                                       
FILNHD   DC    PL6'0',CL14'NEW HIGH, DROP'                                      
FILOHI   DC    PL6'0',CL14'OLD HIGH, INS'                                       
FILEQU   DC    PL6'0',CL14'KEYS EQUAL'                                          
FILEQC   DC    PL6'0',CL14'KEYS EQUAL, CHG'                                     
FILOUT   DC    PL6'0',CL14'RCVR OUT'                                            
RCVADD   DC    PL6'0',CL14'RECOVERY ADDS'                                       
RCVCPY   DC    PL6'0',CL14'RECOVERY CPYS'                                       
RCVCHG   DC    PL6'0',CL14'RECOVERY CHGS'                                       
CNTRSX   EQU   *-1                                                              
*                                                                               
PRINTER  DC    V(PRINTER)                                                       
VFILETAB DC    A(0)                                                             
         EJECT                                                                  
CLCKEY   CLC   4(0,R4),4(R5)       COMPARE OLD AND NEW        KEY               
CLCKEYFF CLC   4(0,R4),KEYFF       SAME, SEE IF BOTH EOF                        
CLCKEY00 CLC   4(0,R4),KEY00       SAME, SEE IF BOTH EOF                        
MVCOLD   MVC   4(0,R4),KEYFF                                                    
MVCNEW   MVC   4(0,R5),KEYFF                                                    
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
MYSEQ    DC    PL4'0'                                                           
KEYFF    DC    42X'FF'                                                          
KEY00    DC    42X'00'                                                          
KEYLEN   DC    X'00'                                                            
*                                                                               
FILEDA   DC    X'00'                                                            
FILEIS   DC    X'00'                                                            
*                                                                               
ISRECLN  DC    AL2(0)               Length of IS record                         
*                                                                               
DSP1STEL DC    AL1(0)                                                           
DSPDELEL DC    AL1(0)                                                           
*                                                                               
INDIRLEN DS    AL1(0)                                                           
ISINDIRO DS    AL1(NO)                                                          
ISINDIRN DS    AL1(NO)                                                          
*                                                                               
@DAFILE  DC    A(0)                                                             
@ISFILE  DC    A(0)                                                             
*                                                                               
FILNAME  DC    CL7' '                                                           
*                                                                               
DATE     DC    X'000000'            YYMMDD (BINARY)                             
*                                                                               
C        DS    CL80                                                             
         EJECT                                                                  
         PRINT NOGEN                                                            
INNEW    DCB   DDNAME=INNEW,DSORG=PS,MACRF=(GM),RECFM=VB,              X        
               BLKSIZE=32760,BUFNO=2,EODAD=ENDINN                               
*              BLKSIZE=32760,LRECL=8000,BUFNO=2,EODAD=ENDINN                    
*                                                                               
INOLD    DCB   DDNAME=INOLD,DSORG=PS,MACRF=(GM),RECFM=VB,              X        
               BLKSIZE=32760,BUFNO=2,EODAD=ENDINO                               
*              BLKSIZE=32760,LRECL=8000,BUFNO=2,EODAD=ENDINO                    
*                                                                               
OUT      DCB   DDNAME=OUT,DSORG=PS,MACRF=(PM),RECFM=VB,                X        
               BLKSIZE=27648,LRECL=8200,BUFNO=2                                 
*              BLKSIZE=32760,LRECL=8200,BUFNO=2                                 
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*RCVREC*'                                                    
RLEN     DS    H                                                                
         DS    H                                                                
       ++INCLUDE DMRCVRHDR                                                      
RREC     DS    8192C                                                            
*                                                                               
         DC    0D                                                               
         DC    C'**UTL-OFFLINE***'                                              
UTL      DC    XL256'00'                                                        
*                                                                               
         DC    0D                                                               
         DC    C'**SSB-OFFLINE***'                                              
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   (SSOXTND-SSOOFF)+SSB                                             
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   (SSOSTAT2-SSOOFF)+SSB                                            
         DC    AL1(SSOSNRCV)       SET RECOVERY OFF                             
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    CL8'*OLDREC*'                                                    
OREC     DS    16384C                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'*NEWREC*'                                                    
NREC     DS    16384C                                                           
         EJECT                                                                  
*RECRECD  DSECT                                                                 
*RECKEY   DS    0CL20               KEY                                         
*FILKTYP  DS    CL1                 RECORD TYPE                                 
*RECKAGY  DS    CL1                 AGENCY                                      
*RECKREST DS    CL18                                                            
*                                  KEY LENGTH                                   
*RECLEN   DS    XL2                 RECORD LEN                                  
*RECSTAT  DS    XL4                 STATUS BYTES                                
*                                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DMFILTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMFILTABD                                                      
         PRINT ON                                                               
* FASSBOFF                                                                      
SSBD     DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDDECON   09/03/19'                                      
         END                                                                    
