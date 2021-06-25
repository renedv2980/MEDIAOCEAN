*          DATA SET SRVTL00    AT LEVEL 004 AS OF 03/18/09                      
*PHASE T10C00A                                                                  
         TITLE '$VTL - VTAM LOGON - BUILD UTL ENTRY'                            
VTL      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**$VTL**,CLEAR=YES                                   
         USING WORKD,RC            RC=A(W/S)                                    
         SAM31                                                                  
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(PARMS)                                  
         L     R9,SRQASYSF                                                      
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,VSSB                                                          
         USING SSBD,R8             R8=A(SSB)                                    
         MVC   RECLEN,SSBTWAL                                                   
         MVC   CHKDSP,=H'16960'    18K TEMPSTR RECS =Y(CHKPTDSP)                
         CLC   RECLEN,=H'14336'                                                 
         BNE   *+10                                                             
         MVC   CHKDSP,=H'12800'    14K TEMPSTR RECS                             
*                                                                               
         GOTO1 VTICTOC,DUB,C'SSET' DISABLE TIMERS                               
*                                                                               
         SR    R0,R0               GET DATE AND TIME                            
         SR    R1,R1                                                            
         TIME  BIN                 R0=TIME,R1=DATE                              
         STM   R0,R1,MVSTIME                                                    
         OI    MVSDATE+3,X'0F'                                                  
         XC    UTLCOUNT,UTLCOUNT                                                
         NI    SSBVTFL1,255-SSBVTTCP-SSBVTTBP                                   
         B     VTL1                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* PASS UTL TO FIND A TERMINAL AWAITING BUILD AND/OR CHECKPOINT        *         
***********************************************************************         
         SPACE 1                                                                
VTL1     L     R5,VUTL             SET BXLE REGS FOR UTL PASS                   
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
                                                                                
         USING UTLD,R5             R5=A(UTL)                                    
VTL2     TM    TSTAT5,TST5TBP      TEST TERMINAL BUILD PENDING                  
         BO    BLD                                                              
VTL2X    EQU   *                                                                
                                                                                
VTL3     TM    TSTAT5,TST5TCP      TEST TERMINAL CHECKPOINT PENDING             
         BO    CHK                                                              
VTL3X    EQU   *                                                                
                                                                                
VTL4     BXLE  R5,R6,VTL2          BUMP TO NEXT UTL ENTRY                       
                                                                                
VTLA     LH    R1,UTLCOUNT         BUMP NUMBER OF UTL PASSES                    
         LA    R1,1(R1)                                                         
         STH   R1,UTLCOUNT                                                      
         CH    R1,MAXCOUNT         TEST MAXIMUM UTL PASSES EXCEEDED             
         BNL   VTLX                                                             
         TM    SSBVTFL1,SSBVTTCP+SSBVTTBP                                       
         BZ    VTLX                                                             
         NI    SSBVTFL1,255-SSBVTTCP-SSBVTTBP                                   
         B     VTL1                BACK FOR ANOTHER PASS                        
*                                                                               
VTLX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TERMINAL BUILD PENDING - CALL V(TERMBLD) TO BUILD UTL/PRQ ENTRY     *         
***********************************************************************         
         SPACE 1                                                                
BLD      XC    PARA(20),PARA       SET PARAM FOR V(TERMBLD)                     
         ST    R5,PARA+4                                                        
         GOTO1 VTERMBLD,PARA                                                    
         CLI   PARA,0              TEST IF VALID TERMINAL BUILD                 
         BNE   BLD1                NO                                           
         OI    TSTAT5,TST5TCP      YES SET WE ALSO NOW WANT CHECKPOINT          
         B     BLD2                                                             
BLD1     OI    TSTAT5,TST5TBF      SET TERMINAL BUILD FAILED IN UTL             
         NI    TSTAT5,255-TST5TCP  TURN OFF CHECKPOINT PENDING IN UTL           
BLD2     NI    TSTAT5,255-TST5TBP  TURN OFF BUILD PENDING IN UTL                
BLDX     B     VTL2X                                                            
         EJECT                                                                  
***********************************************************************         
* TERMINAL CHECKPOINT PENDING - BUILD AND WRITE TERM CHKPNT RECORD    *         
***********************************************************************         
         SPACE 1                                                                
CHK      L     R3,SRQATIA          USE TIA FOR CHECKPOINT RECORD AREA           
         LR    R0,R3                                                            
         LH    R1,RECLEN                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   16(8,R3),=C'*TRMCHK*' SET HEADER TO SHOW T10C CHKPNT             
         MVC   24(2,R3),TNUM                                                    
         MVI   27(R3),X'0C'                                                     
         MVC   28(4,R3),MVSDATE                                                 
         MVC   32(4,R3),MVSTIME                                                 
         MVC   36(8,R3),TSYM                                                    
*                                                                               
CHK1     AH    R3,CHKDSP           R3=A(CHECKPOINT AREA)                        
         USING CHKPTD,R3                                                        
         MVC   CHTDDATE,MVSDATE    SET DATE/TIME OF CHECKPOINT RECORD           
         MVC   CHTDTIME,MVSTIME                                                 
*                                                                               
CHK2     MVC   CHUTLNUM,TNUM       SAVE STATIC UTL DATA                         
         MVC   CHUTLTYP,TTYPE                                                   
         MVC   CHUTLSYM,TSYM                                                    
         MVC   CHUTLSTA,TSTAT1                                                  
         NI    CHUTLSTA,TSTATMST+TSTATRSV+TSTATWEB                              
         MVC   CHUTLOFF,TOFFCODE                                                
         MVC   CHUTLCTR,TCTRY                                                   
         MVC   CHUTLCID,TCID                                                    
         MVC   CHUTLCFN,TCFN                                                    
*                                                                               
CHK4     L     R1,SSBSEQ           SAVE SOME SSB DATA                           
         LA    R1,1(R1)                                                         
         ST    R1,SSBSEQ           BUMP SAVED SEQUENCE NUMBER                   
         MVC   CHSSBSIN,SSBSIN                                                  
         MVC   CHSSBSEQ,SSBSEQ                                                  
         MVC   CHSSBSID,SSBSYSID                                                
         MVC   CHSSBDMP,SSBDMPNO                                                
         MVC   CHSSBTRM,SSBTRMS                                                 
*                                                                               
CHK5     SR    R4,R4               DEVICE IS A PRINTER                          
         ICM   R4,7,TPRNT                                                       
         BZ    CHKW                                                             
         USING PRQD,R4             R4=A(PRINTER QUEUE ENTRY)                    
         MVC   CHUTLSV,TSVDATA     SAVE PRINTER STATIC DATA                     
         L     R3,SRQATIA          SET PRINTER CHECKPOINT LOGO                  
         MVC   16(8,R3),=C'*PRTCHK*'                                            
         CLC   PRENTRY(2),=X'FFFF' TEST NEW STYLE PRINTER QUEUE                 
         BE    CHKP                                                             
         LA    R3,64(R3)           R3=A(PRINTER QUEUE COPY IN CHKPNT)           
         LA    RE,PRENTRY-PRHDR                                                 
         ZIC   R1,PRQNEMAX                                                      
         LA    R0,L'PRENTRY                                                     
         MR    R0,R0                                                            
         AR    R1,RE               R1=LENGTH OF PRINTER QUEUE                   
         LR    R0,R3                                                            
         LR    RE,R4                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY CORE QUEUE TO TIA CHECKPOINT            
         LA    RE,PRQAUTL-PRHDR(R3)                                             
         XC    0(2,RE),0(RE)       OVERWRITE COPY A(UTL) WITH TNUM              
         MVC   2(2,RE),TNUM                                                     
*                                                                               
CHKW     SR    R0,R0               WRITE CHECKPOINT RECORD TO DISK              
         ICM   R0,3,TNUM           SET TRM NUM AND PAGE                         
         L     R3,SRQATIA                                                       
         MVC   DMCB+20(2),=C'L='   SPECIAL PARM FOR TWA0 WRITE                  
         MVC   DMCB+22(2),RECLEN                                                
         TM    TSTAT5,TST5TCP      TEST CHECKPOINT STILL PENDING                
         BZ    CHKX                                                             
         NI    TSTAT5,255-TST5TCP                                               
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(R0),(R3)                    
         CLI   8(R1),0                                                          
         BE    CHKX                                                             
*                                                                               
CHKX     B     VTL3X                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRINTER QUEUE ENTRIES AND MOVE TO CHECKPOINT AREA          *         
***********************************************************************         
         SPACE 1                                                                
CHKP     LA    R3,64(R3)           R3=A(PRINTER QUEUE COPY IN CHKPNT)           
*                                                                               
CHKP3    XC    HALF,HALF           HALF=LAST ENTRY NUMBER                       
         ZIC   R0,PRQNE                                                         
         LR    R1,R0               R1=NUMBER OF ENTRYS                          
         CH    R1,=H'1'                                                         
         BH    CHKP6                                                            
         BE    CHKP5                                                            
*                                                                               
CHKP4    OC    PNTRY,PNTRY         CHECK ZERO ENTRY QUEUE                       
         BZ    CHKP8                                                            
         OI    PNQOK,X'01'         INVALID FIRST ENTRY                          
         XC    PNTRY,PNTRY                                                      
         B     CHKP8                                                            
*                                                                               
CHKP5    OC    PNNEXT,PNNEXT       CHECK SINGLE ENTRY QUEUE                     
         BZ    CHKP5A                                                           
         OI    PNQOK,X'02'         INVALID LAST ENTRY (NON ZERO LINK)           
         XC    PNNEXT,PNNEXT                                                    
CHKP5A   OC    PNTRY(8),PNTRY                                                   
         BNZ   CHKP8                                                            
         OI    PNQOK,X'01'         INVALID FIRST ENTRY                          
         XC    PNTRY,PNTRY                                                      
         SR    R1,R1                                                            
         B     CHKP8                                                            
*                                                                               
CHKP6    OC    PNTRY(8),PNTRY      CHECK MULTIPLE ENTRY QUEUE                   
         BNZ   CHKP7                                                            
         OI    PNQOK,X'01'         INVALID FIRST ENTRY                          
         XC    PNTRY,PNTRY                                                      
         SR    R1,R1                                                            
         B     CHKP8                                                            
*                                                                               
CHKP7    LA    R3,PRQDL(R3)        R3=A(NEXT PRQ ENTRY SLOT IN TWA)             
         BCTR  R0,0                R0=NUM OF POOL ENTRIES                       
         LA    RE,PNTRY            RE=A(PREV PRQ ENTRY)                         
         LA    R1,1                R1=ACTUAL NUMBER OF QUEUE ENTRIES            
*                                                                               
CHKP7A   SR    RF,RF               GET NEXT ENTRY NUMBER                        
         ICM   RF,3,PNNEXT-PNTRY(RE)                                            
         BZ    CHKP8                                                            
         STH   RF,HALF1            HALF1=NEXT ENTRY NUMBER                      
         BCTR  RF,0                INDEX INTO ENTRY POOL                        
         MH    RF,=Y(L'PNTRY)                                                   
         LA    RF,6(RF)                                                         
         A     RF,VPRQENTS         RF=A(CURR PRQ ENTRY)                         
         OC    0(8,RF),0(RF)       CHECK VALID ENTRY                            
         BNZ   CHKP7B                                                           
         OI    PNQOK,X'04'         FORWARD LINK POINTS TO ZERO ENTRY            
         XC    PNNEXT-PNTRY(2,RE),PNNEXT-PNTRY(RE)                              
         LA    R0,L'PNTRY                                                       
         SR    R3,R0                                                            
         XC    PNNEXT-PNTRY(2,R3),PNNEXT-PNTRY(R3)                              
         AR    R3,R0                                                            
         B     CHKP8                                                            
CHKP7B   MVC   0(L'PNTRY,R3),0(RF) MOVE POOL ENTRY TO TWA SLOT                  
         LA    R1,1(R1)                                                         
         LA    R3,L'PNTRY(R3)      BUMP TO NEXT SLOT IN TWA                     
         MVC   HALF,HALF1          HALF=LAST ENTRY NUMBER                       
         LR    RE,RF                                                            
         BCT   R0,CHKP7A                                                        
*                                                                               
CHKP7C   OC    PNNEXT-PNTRY(2,RE),PNNEXT-PNTRY(RE)                              
         BZ    CHKP8                                                            
         OI    PNQOK,X'02'         INVALID LAST ENTRY (NON ZERO LINK)           
         XC    PNNEXT-PNTRY(2,RE),PNNEXT-PNTRY(RE)                              
         LA    R0,L'PNTRY                                                       
         SR    R3,R0                                                            
         XC    PNNEXT-PNTRY(2,R3),PNNEXT-PNTRY(R3)                              
         AR    R3,R0                                                            
*                                                                               
CHKP8    CLM   R1,1,PRQNE          TEST VALID NUM OF QUEUE ENTRIES              
         BE    CHKP8A                                                           
         OI    PNQOK,X'80'         SET INVALID COUNT                            
         STC   R1,PRQNE                                                         
CHKP8A   CLC   PNLAST,HALF         TEST VALID LAST QUEUE ENTRY NUMBER           
         BE    CHKP8B                                                           
         OI    PNQOK,X'40'         SET INVALID LAST ENTRY NUMBER                
         MVC   PNLAST,HALF                                                      
CHKP8B   L     R3,SRQATIA          POINT TO QUEUE COPY IN TIA                   
         LA    R3,64(R3)                                                        
         MVC   0(PRQDL,R3),PRHDR   MOVE FIXED PART OF PRQ TO TIA                
         LA    RE,PRQAUTL-PRHDR(R3)                                             
         XC    0(2,RE),0(RE)       OVERWRITE COPY A(UTL) WITH TNUM              
         MVC   2(2,RE),TNUM                                                     
*                                                                               
CHKPX    B     CHKW                WRITE PRINTER CHKPNT TO DISK                 
         EJECT                                                                  
         LTORG                                                                  
MAXCOUNT DC    H'10'                                                            
SPACES   DC    CL80' '                                                          
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
DMCB     DS    6F                                                               
PARA     DS    6F                                                               
MVSTIME  DS    F                                                                
MVSDATE  DS    F                                                                
UTLCOUNT DS    H                                                                
RECLEN   DS    H                                                                
CHKDSP   DS    H                                                                
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* FACHKPT                                                                       
       ++INCLUDE FACHKPT                                                        
         EJECT                                                                  
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAPRQ                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAPRQ                                                          
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRVTL00   03/18/09'                                      
         END                                                                    
