*          DATA SET SRRCV00    AT LEVEL 016 AS OF 09/22/19                      
*PHASE T10100A,*                                                                
*INCLUDE SQUASHER                                                               
         TITLE 'ONLINE RECOVERY/RESTORE MODULE'                                 
         PRINT NOGEN                                                            
***********************************************************************         
* THIS PROGRAM IS CALLED BY FASTART AND THE $START SERVICE REQUEST AND*         
* ALSO BY THE SYSTEM ABEND ROUTINE WHEN AN ON-LINE APPLICATION PROGRAM*         
* ABNORMALLY TERMINATES.                                              *         
*                                                                     *         
* **NOTE** DOES NOT SUPPORT SHARED MEMORY BUFFERS                     *         
*                                                                     *         
* THE PURPOSE IS TO RESTORE THE FILES TO THE STATUS THEY HAD AT THE   *         
* TIME THE LAST SUCCESSFUL ON-LINE TRANSACTION COMPLETED.             *         
* THE RECOVERY FILE IS READ AND THE RECORDS ARE TREATED AS FOLLOWS    *         
* COPIES  (X'01') ARE WRITTEN BACK                                    *         
* CHANGES (X'02') ARE IGNORED                                         *         
* ADDS    (X'03') ARE ERASED OR LOGICALLY DELETED                     *         
*                                                                     *         
* NOTE THAT THE 'POINTER' BIT (X'80' IN RRECTY) IS IGNORED            *         
***********************************************************************         
                                                                                
***********************************************************************         
* PARAMETER LIST IS AS FOLLOWS                                        *         
* CL1   TASK-ID OF FAILING TASK (OR ZERO TO RECOVER ALL TASKS)        *         
* XL3   SIN OF FAILING TASK     (OR ZERO TO RECOVER ALL TASKS)        *         
* XL1   SYSTEM NUMBER TO BE RECOVERED                                 *         
* AL3   A(SYSFAC)                                                     *         
* XL1   RETURN - ERROR NUMBER OR X'00' IF OK                          *         
* AL3   RETURN - A(ERROR MESSAGE CL40)                                *         
* XL2   N/D                                                           *         
* XL2   RETURN - NUM OF BACKOUTS                                      *         
***********************************************************************         
* R7 = A(RECOVERY DTF)                                                          
***********************************************************************         
RECOVERY CSECT                                                                  
         NMOD1 WRKX-WRKD,**SRCVR*,CLEAR=YES                                     
         USING WRKD,RC                                                          
*                                                                               
         LH    RF,=Y(INPUT-WRKD)   NON-ADDRESSIBLE NOW                          
         AR    RF,RC                                                            
         ST    RF,AINPUT                                                        
*                                                                               
         XC    RCVCRCV,RCVCRCV     CLEAR NUMBER OF BACKOUTS COUNT               
         XC    RCVCBLK,RCVCBLK                                                  
         XC    ERRNUM,ERRNUM                                                    
         XC    ENQINFO,ENQINFO                                                  
         MVI   ERRMSG,C' '                                                      
         MVC   ERRMSG+1(L'ERRMSG-1),ERRMSG                                      
         ST    R1,SVPARAM          SAVE PARAM ADDRESS                           
         L     RA,4(R1)                                                         
         USING SYSFACD,RA                                                       
         L     R9,VSSB                                                          
         USING SSBD,R9                                                          
         OC    SSBSIN,SSBSIN                                                    
         BZ    EXIT                EXIT IF NOT A RESTART                        
         SR    R0,R0                                                            
         IC    R0,SSBTASKS                                                      
         STH   R0,NUMTASKS         SAVE NUMBER OF TASKS                         
         MVC   SYSID,SSBSYSIX                                                   
*                                                                               
RC1      MVC   SE#,4(R1)           SAVE SE NUMBER                               
         L     R3,VSELIST          FIND SELIST ENTRY                            
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SELISTD,R3                                                       
RC1A     CLC   SESYS,SE#           MATCH ON SE                                  
         BE    RC1B                                                             
         BXLE  R3,R4,RC1A                                                       
         B     ERR5                                                             
*                                                                               
RC1B     ST    R3,SVSE             SAVE SELIST ENTRY ADDRESS                    
         MVC   ERRMSG(7),SENAME    SAVE SE NAME IN ERROR MESSAGE                
         OC    SEFILES,SEFILES     TEST NO FILES                                
         BZ    EXIT                                                             
*                                                                               
RC2      XC    DMCB,DMCB           FIND SYSFLES LIST ENTRY                      
         LA    RE,DMREAD                                                        
         ST    RE,DM1                                                           
         LA    RE,SYSFLES                                                       
         ST    RE,DM2                                                           
         MVC   DM3+3(1),SE#        SET SYSTEM NUMBER                            
         LA    R1,DMCB                                                          
         L     RF,VDATAMGR                                                      
         BASR  RE,RF                                                            
         L     R4,DM4              THIS IS A(SYSFLES) ON RETURN                 
         USING SYSFLSTD,R4                                                      
         ST    R4,@SYSFIL                                                       
*                                                                               
         MVC   SVSYSFLH,SYSFLSTD   SAVE SYSFLES HEADER                          
*        LR    R0,RA                                                            
*        SRL   R0,24                                                            
*        STC   R0,@SYSFIL          SAVE SYSTEM NUMBER                           
         IC    R0,SYSF#FLS+1       GET NUMBER OF FILES                          
         LA    R4,SYSFLIST         POINT TO FIRST FILE                          
RC2A     TM    SYSFIND1,SFRCV      IS IT THE RECOVERY FILE                      
         BO    RC3                 YES                                          
         AHI   R4,SYSFLNQ                                                       
         BCT   R0,RC2A                                                          
         B     EXIT                EXIT IF NO RECOVERY FILE                     
*                                                                               
RC3      L     R7,SYSFADTF-1       R7=A(DTF) RECOVERY DTF                       
         ST    R7,@RCVDTF                                                       
         DROP  R4                  SYSFLSTD                                     
*                                                                               
         USING DTFPHD,R7                                                        
         MVC   VMNTRYLN,=AL2(VRCVLNQ)                                           
         MVI   VERMONT,VMONTREG                                                 
         TM    DIND,DINDVRX        EXTEND VERMONT                               
         BZ    RC4                                                              
         MVI   VERMONT,VMONTXTN                                                 
         MVC   VMNTRYLN,=AL2(VRCXLNQ)                                           
*                                                                               
RC4      TM    SVSYSFLH+1,X'01'    TEST IF THIS SYSTEM PROT VIA ENQ/DEQ         
         BZ    RC5                                                              
         CLI   SVSYSFLH+0,X'0A'    TEST IF THIS IS CONTROL SYSTEM               
         BNE   RC5                                                              
* (MAYBE NOT NEEDED FOR GLOBAL CONTROL SYSTEM BUT DOES NO HARM)                 
         MVC   ENQID,=C'CTRL'                                                   
         MVC   ENQCMND,=C'ENQCTL  '                                             
         GOTO1 VDATAMGR,DMCB,(0,ENQCMND),(C'T',ENQID)                           
         TM    8(R1),X'01'                                                      
         BO    RC5                 SYSTEM IS ALREADY ENQUEUED                   
         GOTO1 VDATAMGR,DMCB,(0,ENQCMND),(C'E',ENQID)                           
         MVI   ENQFLG,C'E'         SET ENQUEUED SYSTEM                          
                                                                                
RC5      TM    DTFOPEN,DTF_RO      EXIT IF READ ONLY RECOVERY FILE              
         BO    EXIT                                                             
*                                                                               
         XC    Q1(24),Q1           SET PARM LIST FOR RECOVERY READ              
         MVC   Q1,VREAD                                                         
         LA    RE,RECVREC                                                       
         ST    RE,Q2                                                            
         ST    R7,Q4                                                            
         LA    RE,RECDA            SET A(DA)                                    
         ST    RE,Q5                                                            
*                                                                               
*        CHECK FOR DSPACE SYSTEM                                                
*                                                                               
         TM    DTFFLAG,DTFGLOB     TEST FOR DATASPACE BUFFER                    
         BZ    RC5A                                                             
         OI    DFLAG,DFLAGVRB      FLAG VERMONT RECOVERY                        
         OI    DFLAG,DFLAGGLO      FLAG GLOBAL SYSTEM                           
         B     RDSPACE             GO HANDLE FROM DSPACE BUFFER                 
                                                                                
***********************************************************************         
* READ LAST RECORD ON LOCAL RECOVERY FILE                             *         
***********************************************************************         
RC5A     MVC   FULL,=X'00010000'                                                
         TM    DTFTYPE,DTFTBIGF    20 BIT                                       
         BZ    RC5B                                                             
         MVC   FULL,=X'00001000'                                                
*                                                                               
RC5B     CLC   DNEXT,FULL          TEST NO RECS ON FILE 16 OR 20 BIT            
         BE    EXIT                                                             
*        JE    *+2                 DEBUG                                        
         MVC   RECDA,DNEXT         SET LAST REC ADDRESS                         
         MVI   RECDA+3,0                                                        
         TM    DTFFLAG,DTFTBIGF    20-BIT RCVR                                  
         BZ    RC5C                                                             
         MVC   RECDA+3(1),RECDA+2  TTTTTBRR -> TTTTT00B                         
         NI    RECDA+2,X'F0'                                                    
         NI    RECDA+3,X'0F'                                                    
*                                                                               
RC5C     LA    R2,DMCB             POINT TO DUMMY DMCB                          
         XC    DMCB,DMCB                                                        
         LA    R1,Q1                                                            
         L     RF,VDMOD000                                                      
         BASR  RE,RF               READ CURRENT RECORD                          
         OC    Q3(2),Q3                                                         
         BNZ   ERR1                DISK READ ERROR RCVR FILE                    
         OC    DBLKSZ,DBLKSZ                                                    
         BZ    RC5X                NOT BLOCKED                                  
         CLC   RECDA(3),RECVREC    CHECK DA IN BLOCK AGREES WITH DNEXT          
         BNE   ERR1                AND GET HIGH RECORD NUM IN BLOCK             
         MVC   RECDA+3(1),RECVREC+3                                             
*                                                                               
RC5X     TM    DIND,DINDVRB        VERMONT BUFFER?                              
*        BZ    RC6                                                              
         JZ    *+2                 EVERYTHING IS VERMONT                        
         OI    DFLAG,DFLAGVRB      FLAG VERMONT RECOVERY                        
         OI    DFLAG,DFLAGACT      FLAG ACTIVE BUFFER IN CORE                   
         B     RDSPACE             SET UP AS IF DSPACE                          
*&&DO                                                                           
***********************************************************************         
* BUILD SELIST ENTRY IF SESIN IS ZERO ON RESTART & DATA ON RCVRY FILE *         
***********************************************************************         
RC6      OC    SESIN,SESIN         TEST SYSTEM RESTART                          
         BNZ   RC7                                                              
         L     R4,SVPARAM                                                       
         OC    0(4,R4),0(R4)       TEST CALL BY FASTART                         
         BNZ   RC7                 NO                                           
         MVC   SESIN,SSBSIN                                                     
         MVC   SERCVSEQ,SSBSEQ                                                  
*                                                                               
         ZIC   R5,SSBTASKS                                                      
         LA    R4,SERCVDA          COPY RCVRY DNEXT INTO EACH TASK ADDR         
         MVC   0(4,R4),RECDA                                                    
         LA    R4,4(R4)                                                         
         BCT   R5,*-10                                                          
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF LAST RECOVERY RECORDS AND TASK ID'S                   *         
***********************************************************************         
RC7      L     R4,SVPARAM                                                       
         OC    0(4,R4),0(R4)       TEST SYSTEM RESTART                          
         BZ    RC8                 YES                                          
         MVC   TASKIDS(1),0(R4)    NO - BUILD SINGLE ENTRY TASK LIST            
         L     RE,0(R4)                                                         
         LA    RE,0(RE)                                                         
         BCTR  RE,0                SET TCBSIN-1 FOR COMPARE                     
         ST    RE,TASKSINS                                                      
         L     R5,VTCB                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING TCBD,R5                                                          
         LA    RF,SERCVDA          LOCATE LOW D/A FOR TASK                      
         L     RE,SVPARAM                                                       
         CLC   0(1,RE),TCBID+6                                                  
         BE    *+14                                                             
         LA    RF,4(RF)                                                         
         BXLE  R5,R6,*-14                                                       
         DC    H'0'                                                             
         MVC   LOWADR,0(RF)        SET LOW D/A FOR TASK                         
         OC    LOWADR,LOWADR       TEST LAST D/A KNOWN                          
         BNZ   *+10                                                             
         MVC   LOWADR,=X'00010000' NO - SET BOF AS LOW D/A                      
         B     RCA                                                              
*                                                                               
RC8      LA    RE,TASKSINS         BUILD LIST OF TASK SINS & IDS                
         LA    RF,TASKIDS                                                       
         L     R5,VTCB                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING TCBD,R5                                                          
RC8A     MVC   0(4,RE),TCBSINL     SET LAST SIN PROCESSED BY TASK               
         MVC   0(1,RF),TCBID+6     SET TASK ID                                  
         LA    RE,4(RE)                                                         
         LA    RF,1(RF)                                                         
         BXLE  R5,R6,RC8A                                                       
*                                                                               
RC9      LH    RF,NUMTASKS         FIND LOWEST D/A FOR SE RCVR FILE             
         LA    RE,SERCVDA                                                       
         MVC   LOWADR(4),=X'FFFFFFFF'                                           
RC9A     CLC   0(4,RE),LOWADR                                                   
         BH    *+10                                                             
         MVC   LOWADR(4),0(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   RF,RC9A                                                          
         OC    LOWADR,LOWADR       TEST LAST D/A KNOWN                          
         BNZ   *+10                                                             
         MVC   LOWADR,=X'00010000' NO - SET BOF AS LOW D/A                      
         B     RCA                 GO DO THE BUSINESS                           
                                                                                
***********************************************************************         
* INITIALISE FOR BLOCKED OR UNBLOCKED RECOVERY FILE                   *         
***********************************************************************         
RCA      L     R7,@RCVDTF          POINT TO SYSTEM RECOVERY FILE                
         USING DTFPHD,R7                                                        
         SR    R0,R0                                                            
         ICM   R0,3,DBLKSZ         TEST IF BLOCKED                              
         BZ    RUNB                NO                                           
         MVC   P1(24),Q1           YES SAVE READ PARAMETER LIST                 
         MVC   Q1,VDARPT           AND CALL DADDS FOR BLKSIZE                   
         ST    R0,Q3                                                            
         MVC   Q4,@RCVDTF                                                       
         LA    R1,Q1                                                            
         L     RF,VDADDS                                                        
         BASR  RE,RF                                                            
         ICM   R0,3,Q3+2                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STH   R0,BLKSTRK          SAVE BLOCKS PER TRACK                        
*                                                                               
         MVC   Q1(24),P1           RESET PARM LIST FOR BLOCKED I/O              
         B     RBLK                                                             
*&&                                                                             
***********************************************************************         
* EXIT                                                                          
***********************************************************************         
EXIT     CLI   ENQFLG,C'E'         IF ENQUEUED SYSTEM MUST DEQUEUE IT           
         BNE   EXIT1                                                            
         GOTO1 VDATAMGR,DMCB,(0,ENQCMND),(C'D',ENQID)                           
EXIT1    L     R1,SVPARAM                                                       
         LH    R0,RCVCRCV          R0=NUMBER OF BACKOUTS                        
         MVC   8(4,R1),ERRNUM      RETURN ZERO FOR OK OR ERROR NUM              
         CLI   8(R1),0                                                          
         BNE   *+8                                                              
         ST    R0,08(R1)                                                        
         ST    R0,12(R1)           RETURN NUMBER OF BACKOUTS                    
         XMOD1 1                                                                
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* BLOCKED RECOVERY FILE                                               *         
* BLK+0(3) DISK ADDR OF BLOCK TTTTBB                                  *         
* BLK+3(1) HIGHEST RECORD NUMBER IN BLOCK                             *         
* BLK+4(2) NUMBER OF BYTES USED IN BLOCK                              *         
* BLK+6    LENGTH OF FIRST REC (XL2) FOLLOWED BY FIRST RECORD DATA    *         
***********************************************************************         
RBLK     MVI   RWRTPNDG,0          INIT WRITE PENDING FLAG                      
         XC    RCVCBLK,RCVCBLK                                                  
         XC    RECNUMS(256),RECNUMS                                             
         XC    RECNUMS+256(256),RECNUMS+256                                     
*                                                                               
RBLK1    LA    R1,RECVREC          BUILD LIST OF RECORDS IN THIS BLOCK          
         LR    R8,R1                                                            
         AH    R8,RECVREC+4        R8=A(END OF LAST RECORD IN BLOCK)            
         LA    R6,RECVREC+6        R6=A(NEXT RECORD IN BLOCK)                   
         LA    R7,RECNUMS+2        R7=A(NEXT RECORD IN TABLE)                   
*                                                                               
RBLK2    CR    R6,R8               TEST END OF BLOCK                            
         BNL   RBLK3                                                            
         LR    R0,R6                                                            
         SR    R0,R1               R0=DISPLACEMENT OR REC IN BLOCK              
         STH   R0,0(R7)                                                         
         ICM   R0,3,0(R6)          GET RECORD LENGTH                            
         AR    R6,R0               BUMP TO NEXT RECORD IN BLOCK                 
         LA    R7,2(R7)            BUMP TO NEXT ENTRY IN TABLE                  
         LTR   R0,R0                                                            
         BNZ   RBLK2                                                            
*                                                                               
RBLK3    LA    R0,RECNUMS+2        GET NUMBER OF LAST REC IN BLOCK              
         SR    R7,R0                                                            
         SRL   R7,1                                                             
         STH   R7,RECNUMS+0        SAVE IN ZEROTH ENTRY IN TABLE                
         STC   R7,RECDA+3          SET RECORD NUMBER IN DISK ADDRESS            
         LTR   R7,R7                                                            
         BZ    RBLKNXT             THIS BLOCK IS EMPTY                          
*                                                                               
RBLK4    CLC   RECDA(4),LOWADR     EXIT IF LOW RECORD REACHED                   
         BH    RBLK4A                                                           
         TM    RWRTPNDG,X'01'                                                   
         BZ    EXIT                                                             
         OI    RWRTPNDG,X'80'      SET DISK ADDR TOO LOW EXIT                   
         B     RBLKNXT1                                                         
RBLK4A   SR    R1,R1               LOCATE RECORD IN BLOCK                       
         IC    R1,RECDA+3                                                       
         SLL   R1,1                                                             
         LA    R1,RECNUMS(R1)      INDEX INTO RECNUMS TABLE                     
         SR    R6,R6                                                            
         ICM   R6,3,0(R1)          EXTRACT DISPLACEMENT INTO BLOCK              
         BZ    ERR1                                                             
         LA    R6,RECVREC(R6)                                                   
         SR    R1,R1                                                            
         ICM   R1,3,0(R6)                                                       
         SHI   R1,2                                                             
         BNP   ERR1                                                             
         ST    R1,P3               P3=L'RECOVERY RECORD                         
         LA    R6,2(R6)            R6=A(RECOVERY RECORD IN BLOCK)               
         USING RCVRREC,R6                                                       
*                                                                               
RBLK5    CLC   RSIN+1(3),=AL3(1)   EXIT IF SIN GOES TOO LOW                     
         BH    RBLK5A                                                           
         TM    RWRTPNDG,X'01'                                                   
         BZ    EXIT                                                             
         OI    RWRTPNDG,X'40'      SET SIN TOO LOW EXIT                         
         B     RBLKNXT1                                                         
RBLK5A   CLI   RTASKID,RBACKOUT    IGNORE LOGICALLY DELETED RECORDS             
         BE    RBLKNXT                                                          
*                                                                               
RBLK6    GOTO1 PROCESS             GO PROCESS RECOVERY RECORD                   
         B     *+4(RF)                                                          
         B     RBLKDEL             LOGICALLY DELETE                             
         B     RBLKNXT             IGNORE                                       
         B     ERR6                SIN CHANGED                                  
         B     ERR3                DISK READ ERROR                              
*                                                                               
RBLKDEL  MVI   RTASKID,RBACKOUT    LOGICALLY DELETE RECORD                      
         LH    RF,RCVCBLK                                                       
         LA    RF,1(RF)                                                         
         STH   RF,RCVCBLK          BUMP NUMBER RECS 1N THIS BLOCK               
         OI    RWRTPNDG,X'01'      SET BLOCK WRITE PENDING                      
*                                                                               
RBLKNXT  SR    R1,R1               FIND NEXT BACKWARD RECORD                    
         IC    R1,RECDA+3                                                       
         SH    R1,=H'1'                                                         
         BNP   *+12                NO MORE RECORDS IN THIS BLOCK                
         STC   R1,RECDA+3                                                       
         B     RBLK4                                                            
         TM    RWRTPNDG,X'01'      TEST IF WRITE PENDING                        
         BZ    RBLKNXT2                                                         
*                                                                               
RBLKNXT1 L     R7,@RCVDTF          WRITE BACK UPDATED RECOVERY BLOCK            
         L     RE,DBLK                                                          
         CLC   0(3,RE),RECVREC     IS THIS BLOCK THE CURRENT BUFFER             
         BNE   RBLKNX1A            NO                                           
         SR    RF,RF               YES MOVE UPDATED BLOCK BACK TO BUFF          
         ICM   RF,3,DBLKSZ                                                      
         LR    R1,RF                                                            
         LA    R0,RECVREC                                                       
         MVCL  RE,R0                                                            
RBLKNX1A MVC   Q1,VWRITE           WRITE BACK BLOCK                             
         MVI   RECDA+3,0                                                        
         GOTO1 VDMOD000,Q1                                                      
         OC    Q3(2),Q3                                                         
         BNZ   ERR2                                                             
         LH    RF,RCVCRCV          BUMP NUMBER OF BACKOUTS                      
         AH    RF,RCVCBLK                                                       
         STH   RF,RCVCRCV                                                       
         TM    RWRTPNDG,X'F0'      EXIT IF END OF RECOVERY FLAGS ON             
         BNZ   EXIT                                                             
*                                                                               
RBLKNXT2 SR    R1,R1               BACK TO PREV BLOCK ON TRACK                  
         IC    R1,RECDA+2                                                       
         SH    R1,=H'1'                                                         
         BNP   RBLKNXT3            NO MORE BLOCKS ON THIS TRACK                 
         STC   R1,RECDA+2                                                       
         B     RBLKNXT4                                                         
*                                                                               
RBLKNXT3 SR    R1,R1               BACK TO LAST REC ON PREV TRACK               
         ICM   R1,3,RECDA                                                       
         SH    R1,=H'1'                                                         
         BNP   EXIT                EXIT IF BOF                                  
         STH   R1,RECDA                                                         
         MVC   RECDA+2(1),BLKSTRK+1                                             
*                                                                               
RBLKNXT4 MVC   Q1,VREAD            READ RECOVERY FILE BLOCK                     
         MVI   RECDA+3,0                                                        
         GOTO1 VDMOD000,Q1                                                      
         OC    Q3(2),Q3                                                         
         BNZ   ERR1                                                             
         B     RBLK                                                             
         EJECT                                                                  
***********************************************************************         
* UNBLOCKED RECOVERY FILE                                             *         
***********************************************************************         
RUNB     LA    R6,RECVREC          R6=A(UNBLOCKED RECOVERY RECORD)              
         USING RCVRREC,R6                                                       
         CLC   RECDA(3),LOWADR     EXIT IF LOW RECORD REACHED                   
         BNH   EXIT                                                             
         CLC   RSIN+1(3),=AL3(1)   EXIT IF SIN GOES TOO LOW                     
         BNH   EXIT                                                             
         CLI   RTASKID,RBACKOUT    IGNORE LOGICALLY DELETED RECORDS             
         BE    RUNBNXT                                                          
         MVC   P1(24),Q1                                                        
*                                                                               
         GOTO1 PROCESS             GO PROCESS RECOVERY RECORD                   
         B     *+4(RF)                                                          
         B     RUNBDEL             LOGICALLY DELETE                             
         B     RUNBNXT             IGNORE                                       
         B     ERR6                SIN CHANGED                                  
         B     ERR3                DISK READ ERROR                              
*                                                                               
RUNBDEL  MVI   RTASKID,RBACKOUT    LOGICALLY DELETE RECORD                      
         MVC   Q1,VWRITE                                                        
         GOTO1 VDMOD000,Q1                                                      
         OC    Q3(2),Q3                                                         
         BNZ   ERR2                DISK WRITE ERROR RCVR FILE                   
         LH    RF,RCVCRCV                                                       
         LA    RF,1(RF)                                                         
         STH   RF,RCVCRCV          BUMP NUMBER OF BACKOUTS                      
*                                                                               
RUNBNXT  XC    Q1(24),Q1           NOW READ RECOVERY FILE                       
         MVC   Q1,VDABACK                                                       
         LA    RE,RECVREC                                                       
         ST    RE,Q2                                                            
         MVC   Q4,@RCVDTF                                                       
         LA    RE,RECDA                                                         
         ST    RE,Q5                                                            
         LA    R1,Q1                                                            
         L     RF,VDADDS                                                        
         BASR  RE,RF                                                            
         OC    Q3(2),Q3                                                         
         BNZ   RUNBNXT1                                                         
         MVC   Q1,VREAD                                                         
         L     RF,VDMOD000                                                      
         MVI   8(R2),0             RESET NOT FOUND IN DUMMY DMCB                
         BASR  RE,RF                                                            
         TM    8(R2),X'10'         TEST DMCB FOR NO REC FND                     
         BZ    RUNB                                                             
         B     RUNBNXT                                                          
*                                                                               
RUNBNXT1 TM    Q3+1,X'04'          TEST BOF                                     
         BZ    ERR1                NO DISK READ ERROR RCVR FILE                 
         B     EXIT                YES HAVE FINISHED                            
*&&                                                                             
**********************************************************************          
*        RECOVER DATASPACE AND VERMONT BUFFERS                       *          
**********************************************************************          
RDSPACE  L     R4,SVPARAM                                                       
         MVC   ACTVID,0(R4)        MOVE IN SIN PASSED                           
         MVC   ACTVID(1),SYSID     SYSID IN FIRST BYTE                          
         XC    GINS(10*8),GINS     CLEAR GIN LIST                               
*                                                                               
         TM    DFLAG,DFLAGGLO      DATASPACE GET BUFFER ADDR                    
         BO    RDS10               GLOBAL YES                                   
*                                                                               
* ADD SHARE MEMORY CODE HERE                                                    
*                                                                               
         MVC   ARECBUF,DBLK        NON DATASPACE BUFFER IN DBLK                 
         B     RDCHECK                                                          
*                                                                               
RDS10    XC    DMCB,DMCB           DSPACE RESOURCE NUMBER                       
         OI    DMCB,X'02'          SET RECOVERY IN PROCESS LOCK                 
         MVC   DMCB+3(1),SE#                                                    
         GOTO1 VLOCKSPC,DMCB       ON RETURN WE OWN THIS RESOURCE               
         L     R2,4(R1)                                                         
         USING DMSPACED,R2                                                      
         MVC   ARECBUF,DSPECB                                                   
         SAC   512                 POINT TO DSPACE BUFFER                       
         LAM   AR2,AR2,SSBALET                                                  
         SR    R2,R2                                                            
         L     R2,ARECBUF                                                       
         USING DMSYSHDR,R2                                                      
         L     R2,DSYABUFF                                                      
         ST    R2,ARECBUF                                                       
         USING VRCVHDR,R2                                                       
         LA    RE,RECVREC          COPY RECORD FROM DSPACE                      
         LH    RF,VRCVNXT          LENGTH OF BLOCK                              
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
         SAC   0                   TURN OFF ACCESS MODE                         
         OI    DFLAG,DFLAGACT      SET ACTIVE BUFFER IN CORE                    
         B     RDCHECK                                                          
         DROP  R2                                                               
                                                                                
**********************************************************************          
* READ PRIOR RECORD                                                             
**********************************************************************          
RDNEXT   LA    RE,RECVREC                                                       
         LA    R2,DMCB                                                          
         MVC   Q1,VREAD            READ RECOVERY FILE BLOCK                     
         MVC   RECDA,PREVDA                                                     
         MVI   RECDA+3,0                                                        
*                                                                               
RDNEXT10 TM    DTFTYPE,DTFTBIGF    20-BIT RCVR                                  
         BZ    RDNEXT20                                                         
         MVC   RECDA+3(1),RECDA+2  TTTTTBRR TO TTTTT00B                         
         NI    RECDA+2,X'F0'                                                    
         NI    RECDA+3,X'0F'                                                    
*                                                                               
RDNEXT20 GOTO1 VDMOD000,Q1                                                      
         OC    Q3(2),Q3                                                         
         BNZ   ERR1                                                             
         XC    PREVDA,PREVDA                                                    
         NI    DFLAG,255-DFLAGACT  CLEAR ACTIVE BIT                             
                                                                                
**********************************************************************          
*  CHECK FOR THIS TASK/SYSTEM IN ACTIVE HEADER                                  
*  PROCESS ALL RECOVERY RECORDS IN THIS BLOCK FOR THIS SIN                      
**********************************************************************          
RDCHECK  LA    RE,RECVREC          SAVE A(ACTIVE) BLOCK                         
         ST    RE,ARECBLK                                                       
*                                                                               
         XC    RCVCBLK,RCVCBLK     CLEAR COUNT AND TABLE                        
         XC    RECNUMS(256),RECNUMS                                             
         XC    RECNUMS+256(256),RECNUMS+256                                     
         LA    R3,RECNUMS          BUILD TABLE OF OFFSETS                       
*                                                                               
         L     R2,ARECBLK                                                       
         USING VRCVHDR,R2                                                       
RDC004   AH    R2,VRCV1STR         POINT TO FIRST RECORD                        
         AHI   R2,VRCVHL1Q         ADD JUST A BIT MORE                          
         USING DMRCVRD,R2                                                       
RDC010   LR    R1,R2                                                            
         S     R1,ARECBLK          CALC OFFSET                                  
         STCM  R1,3,0(R3)                                                       
         LA    R3,2(R3)            NEXT TABLE ENTRY                             
         SR    R1,R1                                                            
         ICM   R1,3,DMRCVLN        GET LENGTH                                   
         BZ    RDC030                                                           
         AR    R2,R1               NEXT RECORD                                  
         B     RDC010                                                           
         DROP  R2                                                               
*                                                                               
RDC030   LA    R0,VRCVMAXQ         10 MAS FOR NOW                               
         L     R3,ARECBLK          FIND 1ST ACTIVE ENTRY                        
*                                                                               
         USING VRCVHDR,R3                                                       
         LA    R3,VRCVNTRY         POINT TO FIRST VERMONT ENTRY                 
*                                                                               
         USING VRCVNTRY,R3                                                      
RDC040   LLH   RF,VMNTRYLN                                                      
         BCTR  RF,0                                                             
         EX    RF,OCVMONT          IGNORE ZERO ENTRIES                          
         BZ    RDC051                                                           
         CLC   VRCVOFFL(4),=X'FFFFFFFF'  FOR NON-GLOBAL OVERNIGHT JOBS          
         BNE   RDC042                                                           
         EX    RF,XCVMONT          CLEAR OFFLINE FF'S                           
         B     RDC051                                                           
*                                                                               
RDC042   OC    ACTVID+1(3),ACTVID+1 TEST IF BACKOUT ALL                         
         BNZ   RDC044                                                           
         CLC   VRCVFID#,ACTVID     ALL FOR THIS SYSTEM                          
         BE    RDPROC                                                           
                                                                                
RDC044   CLC   VRCVID#,ACTVID      OR JUST THIS JOB/SIN                         
         BE    RDPROC                                                           
         B     RDC051              GET NEXT                                     
*                                                                               
RDCGONXT DS    0H                                                               
         LLH   RF,VMNTRYLN         LENGTH OF ENTRY                              
         BCTR  RF,0                                                             
         TM    DFLAG,DFLAGACT      IF THIS IS ACTIVE BUFFER                     
         BZ    RDC051                                                           
         EX    RF,XCVMONT          REMOVE ACTIVE ENTRY                          
*                                                                               
RDC051   AH    R3,VMNTRYLN         NEXT                                         
         BCT   R0,RDC040                                                        
         B     RDWRITE                                                          
         DROP  R3                                                               
                                                                                
************************************************************                    
*        WRITE BACK RECOVERY BLOCK                         *                    
************************************************************                    
RDWRITE  LA    RE,RECVREC                                                       
         OC    0(3,RE),0(RE)       DON'T WRITE ZERO DA                          
         BZ    RDSPACEX                                                         
         TM    DFLAG,DFLAGACT      ACTIVE BUFFER MUST GO BACK                   
         BNO   RDW010                                                           
*                                                                               
         TM    DFLAG,DFLAGGLO      TEST FOR DATASPACE BUFFER                    
         BNO   RDW002                                                           
         SAC   512                 SET ACCESS REG MODE FOR R2                   
         LAM   AR2,AR2,SSBALET                                                  
*                                                                               
*AH3     PUT SHARED MEMORY CODE HERE                                            
*        SAM31                                                                  
RDW002   L     R2,ARECBUF          GET A(BUFFER) LOCAL OR GLOBAL                
         LA    RE,RECVREC          COPY RECORD TO BUFFER                        
         USING VRCVHDR,RE                                                       
         LH    RF,VRCVNXT          USE LENGTH FROM RECVREC                      
         LR    R3,RF                                                            
         MVCL  R2,RE                                                            
         SAC   0                   TURN OFF ACCESS MODE                         
         DROP  RE                                                               
*                                                                               
RDW010   ICM   R0,3,DBLKSZ         SET BLOCK LEN                                
         ST    R0,Q3                                                            
         LA    R2,DMCB                                                          
         LA    RE,RECVREC                                                       
         MVC   Q1,VWRITE           WRITE RECOVERY FILE BLOCK                    
         MVC   RECDA,0(RE)                                                      
         MVI   RECDA+3,0                                                        
*                                                                               
         TM    DTFTYPE,DTFTBIGF    20-BIT RCVR                                  
         BZ    RDW010B                                                          
         MVC   RECDA+3(1),RECDA+2  TTTTTBRR -> TTTTT00B                         
         NI    RECDA+2,X'F0'                                                    
         NI    RECDA+3,X'0F'                                                    
*                                                                               
RDW010B  GOTO1 VDMOD000,Q1                                                      
         OC    Q3(2),Q3                                                         
         BZ    RDW020                                                           
*                                                                               
         TM    DFLAG,DFLAGNOP      IS THE SYSTEM NOP                            
         BZ    ERR2                FILE IS PROBABLY FULL                        
         NI    DFLAG,255-DFLAGNOP  ALLOW ONE WRITE ERROR EOF                    
*                                                                               
RDW020   OC    PREVDA,PREVDA       ANY PREVIOUS RECORD TO DO                    
         BNZ   RDNEXT                                                           
         B     RDSPACEX            NO SO UNLOCK AND EXIT                        
                                                                                
************************************************************                    
*        PROCESS ACTIVE ENTRY                              *                    
************************************************************                    
         USING VRCVNTRY,R3                                                      
RDPROC   L     R1,ARECBLK                                                       
         CLC   VRCVLINK(3),0(R1)   IS RECORD IN BLOCK                           
         BNE   RDP060                                                           
*                                                                               
         XR    R4,R4               INDEX INTO RECNUMS TO FIND RECORD            
         IC    R4,VRCVLINK+3       GET RECORD NUMBER                            
         BCTR  R4,0                                                             
         SLL   R4,1                                                             
         LA    R4,RECNUMS(R4)                                                   
         LH    R1,0(R4)            R1 = DISPLACEMENT TO RECORD                  
*                                                                               
         L     R6,ARECBLK          POINT R6 TO RECORD                           
         AR    R6,R1               A(RECOVERY RECORD)                           
         USING DMRCVRD,R6                                                       
         SR    R1,R1                                                            
         ICM   R1,3,DMRCVLN        GET RECLEN IN R1                             
         ST    R1,P3                                                            
*                                                                               
         LA    R6,DMRCVHDR         POINT TO RECOVERY HEADER                     
         USING RCVRREC,R6                                                       
         CLI   RTASKID,RBACKOUT    IGNORE LOGICALLY DELETED RECORDS             
         BE    RDP050                                                           
*                                                                               
         TM    RTIME,RTIMEEXT      HEADER EXTENSION?                            
         BZ    RDP040              NO, SO NO GIN                                
         L     RF,P3               GET RECORD LENGTH                            
         AR    RF,R6               ADD A(RCVRREC), 6 BYTES PAST START           
         AHI   RF,-(6+1)           BACK UP TO LAST BYTE                         
         CLI   0(RF),RXLENQ        CHECK LONG ENOUGH FOR GIN                    
         BL    RDP040              SHOULDN'T NEED THIS CHECK                    
         LLC   R1,0(,RF)           LAST BYTE IS EXTENSION LENGTH                
         BCTR  R1,0                                                             
         SR    RF,R1               RF POINTS TO EXTENSION                       
         USING RCVREXTD,RF                                                      
         TM    RGESTAT,RGESUSSS    WAS IT SENT TO USS                           
         BZ    RDP040              NO, SO DON'T SEND ROLLBACK                   
         LR    R1,R0               R0=GINS INDEX                                
         BCTR  R1,0                                                             
         SLL   R1,3                                                             
         LA    R1,GINS(R1)         INDEX INTO GINS                              
         OC    0(8,R1),0(R1)       DO WE HAVE A GIN FOR TASK                    
         BZ    *+16                                                             
         CLC   0(8,R1),RGIN        YES, IS IT THE SAME                          
         BE    RDP040                                                           
         DC    H'0'                DEBUG. GINS MUST BE SAME                     
         MVC   0(8,R1),RGIN        SAVE GIN FROM THIS RECORD                    
         DROP  RF                                                               
*                                                                               
RDP040   GOTO1 PROCESS             GO PROCESS RECOVERY RECORD                   
*                                                                               
RDP050   MVI   RTASKID,RBACKOUT    LOGICALLY DELETE RECORD                      
*                                                                               
         LH    R1,RCVCRCV          BUMP RECOVERED COUNT                         
         AHI   R1,1                                                             
         STH   R1,RCVCRCV                                                       
*                                                                               
RDP052   SHI   R6,DMRCVLNQ         RECORD STARTS WITH PREV DA                   
         USING DMRCVRD,R6                                                       
         OC    DMRCVLNK,DMRCVLNK                                                
         BZ    RDP070                                                           
         MVC   VRCVLINK,DMRCVLNK   BACK UP TO PREVIOUS                          
         B     RDPROC                                                           
*                                                                               
RDP060   CLC   VRCVLINK(3),PREVDA  SAVE THE HIGHEST PREVIOUS DA                 
         BL    *+10                                                             
         MVC   PREVDA(3),VRCVLINK                                               
         DROP  R6                                                               
*                                                                               
RDP070   B     RDCGONXT            GO BACK FOR MORE                             
                                                                                
**********************************************************************          
* DONE SO EXIT AND/OR UNLOCK RESOURCE                                           
**********************************************************************          
RDSPACEX TM    DFLAG,DFLAGGLO      EXIT IF NOT DSPACE                           
         BNO   EXIT                                                             
*                                                                               
         XC    DMCB,DMCB           ELSE UNLOCK DSPACE RESOURCE                  
         MVC   DMCB+3(1),SE#                                                    
         OI    DMCB,X'10'                                                       
         GOTO1 VLOCKSPC,DMCB       UNLOCK RESOURCE                              
*                                                                               
         OC    GINS(10*8),GINS     ANY GINS FOUND?                              
         BZ    EXIT                NO                                           
         CLI   SE#,1               DON'T DO USS BACKOUT IF NOT A                
         BNH   EXIT                USER SYSTEM                                  
         ICM   RF,15,VRCVRUSS      USS QUEUE WRITE BACKOUT                      
         BZ    EXIT                NOT LINKED IN                                
         XC    DMCBUSSQ,DMCBUSSQ   UNIX QUEUE PARAMETERS                        
         LA    RE,2                COMMAND TO SEND BACKOUT                      
         ST    RE,P1USS            P1USS=COMMIT COMMAND                         
         MVC   P2USS+3(1),SE#      P2USS=SE NUMBER                              
         LA    R1,DMCBUSSQ                                                      
         LA    R0,10                                                            
         LA    R3,GINS                                                          
RDP080   OC    0(8,R3),0(R3)       SEND USS ROLLBACK FOR EACH GIN               
         BZ    RDP085                                                           
         ST    R3,P3USS            P3USS=A(GIN)                                 
         BASR  RE,RF               WRITE ROLLBACK                               
RDP085   LA    R3,8(,R3)                                                        
         BCT   R0,RDP080                                                        
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ERROR - PASS BACK ERROR NUMBER AND MESSAGE TEXT                    *          
**********************************************************************          
ERR1     LA    RE,1                                                             
         MVC   ERRMSG+18(16),=CL16'RCVR READ ERROR'                             
         L     R1,Q4                                                            
         B     ERRX                                                             
ERR2     LA    RE,2                                                             
         MVC   ERRMSG+18(16),=CL16'RCVR WRITE ERROR'                            
         L     R1,Q4                                                            
         B     ERRX                                                             
ERR3     LA    RE,3                                                             
         MVC   ERRMSG+18(16),=CL16'DISK READ ERROR'                             
         L     R1,P4                                                            
         B     ERRX                                                             
ERR4     LA    RE,4                                                             
         MVC   ERRMSG+18(16),=CL16'DISK WRITE ERROR'                            
         L     R1,P4                                                            
         B     ERRX                                                             
ERR5     LA    RE,5                                                             
         MVC   ERRMSG(7),=7C'?'                                                 
         MVC   ERRMSG+18(16),=CL16'UNKNOWN SYSTEM'                              
         B     ERRX1                                                            
ERR6     LA    RE,6                                                             
         MVC   ERRMSG+18(16),=CL16'SIN HAS CHANGED'                             
         B     ERRX1                                                            
*                                                                               
ERRX     MVI   ERRMSG+7,C'/'                                                    
         MVC   ERRMSG+8(7),22(R1)                                               
ERRX1    MVI   ERRMSG+16,C'-'                                                   
*                                                                               
ERRXX    LA    RF,ERRMSG                                                        
         ST    RF,ERRNUM                                                        
         STC   RE,ERRNUM                                                        
         LA    R0,L'ERRMSG                                                      
         GOTO1 =V(SQUASHER),DMCB,ERRMSG,(C' ',(R0)),RR=RB                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS RECOVERY FILE RECORD                                        *         
* R6=A(RECORD) AND P3 CONTAINS LENGTH                                 *         
***********************************************************************         
PROCESS  NTR1                                                                   
         USING RCVRREC,R6                                                       
         TM    DFLAG,DFLAGVRB      NO CHECKING FOR VERMONTS                     
         JZ    *+2                                                              
*        BO    RCDSPACE                                                         
*&&DO                                                                           
         TM    RTIME,RTIMEFID      TEST NEW STYLE TIME FIELD                    
         BZ    RC10                                                             
         TM    SVSYSFLH+1,X'01'    TEST SYSTEM PROT VIA ENQ/DEQ                 
         BZ    RC10                                                             
         L     R9,VSSB                                                          
         MVC   FACID,RTIME+3       GET FACPAK ID FROM RTIME+3 BYTE              
         TM    FACID,X'F0'         EITHER IN FIRST OR SECOND NIBBLE             
         BZ    RC08                IN SECOND NIBBLE                             
         ZIC   RF,FACID            IN FIRST NIBBLE                              
         SRL   RF,4                                                             
         STC   RF,FACID                                                         
RC08     CLC   SSBSYSID,FACID      IGNORE IF NOT THIS FACPAK'S REC              
         BNE   RCIGNORE                                                         
*                                                                               
RC10     LA    RE,TASKIDS          SEARCH FOR RECOVERY FILE TASK ID             
         LA    RF,TASKSINS                                                      
         LA    R1,TASKDONE                                                      
RC11     CLI   0(RE),0             TEST E-O-L                                   
         BE    RCIGNORE                                                         
         CLC   RTASKID,0(RE)       MATCH TASK ID TO RECOVERY FILE               
         BE    RC12                                                             
         LA    RE,1(RE)            BUMP TASK ID POINTER                         
         LA    RF,4(RF)            BUMP TASK SIN POINTER                        
         LA    R1,4(R1)            BUMP TASK DONE POINTER                       
         B     RC11                                                             
*                                                                               
RC12     CLC   RSIN+1(3),1(RF)     IS RECORD TO BE RECOVERED                    
         BNH   RCIGNORE            NO                                           
         OC    0(4,R1),0(R1)       TEST FIRST SIN PROCESSED                     
         BNZ   *+10                                                             
         MVC   1(3,R1),RSIN+1      NO - SET FIRST SIN VALUE                     
         CLC   1(3,R1),RSIN+1                                                   
         BNE   RCERR6              SIN HAS CHANGED                              
*&&                                                                             
RCDSPACE MVC   TRECTY,RRECTY       DON'T CHANGE IN RECORD                       
         NI    TRECTY,X'7F'        TURN OFF 'POINTER' BIT                       
         CLI   TRECTY,CHANGE                                                    
         BE    RCDELETE            DELETE CHANGES FROM RECOVERY FILE            
***********************************************************************         
* FIND FILE FROM SYSTEM TRYING TO RECOVERY                                      
***********************************************************************         
         L     R4,@SYSFIL          SEARCH FOR FILE IN SYSFLES LIST              
         USING SYSFLSTD,R4                                                      
         SR    R8,R8                                                            
         IC    R8,SYSF#FLS+1       NUMBER OF FILES                              
         LA    R4,SYSFLIST                                                      
RC12A    CLC   RFILTY,SYSFILE#     MATCH FILE NUMBER                            
         BE    RC12B                                                            
         AHI   R4,SYSFLNQ          NEXT FILE                                    
         BCT   R8,RC12A                                                         
         B     RCDELETE            ERASE INVALID RCVR REC                       
*                                                                               
RC12B    L     RE,SYSFADTF-1       GET DTF ADDRESS                              
         ST    RE,P4                                                            
         L     RF,VDMOD000         SET V(DMCNTL)                                
         LA    R1,P1               SET A(PARAMS)                                
         TM    SYSFIND1,SFISF      TEST FILE TYPE                               
         BO    RC16                IS FILE                                      
         B     RC14                DA FILE                                      
         DROP  R4                                                               
                                                                                
***********************************************************************         
* DIRECT ACCESS FILE                                                  *         
***********************************************************************         
RC14     LA    RE,RVCHR            READ DA FILE RECORD INTO INPUT AREA          
         ST    RE,P5                                                            
         MVC   P1,VREAD                                                         
         L     RE,AINPUT                                                        
         ST    RE,P2                                                            
         BASR  RE,RF               GO READ RECORD                               
         CLI   DM3,0                                                            
         BE    RC14A                                                            
         TM    DM3,X'90'                                                        
         BZ    RCERR3              DISK READ ERROR DA FILE                      
         CLI   TRECTY,ADD                                                       
         BE    RCDELETE            EOF AND NOTFOUND OK FOR ADD                  
         B     RCERR3                                                           
*                                                                               
RC14A    CLI   TRECTY,COPY         OVERWRITE WITH RECOVERY COPY RECORD          
         BNE   RC14B                                                            
         MVC   P1,VWRITE                                                        
         LA    RE,RECORD                                                        
         ST    RE,P2                                                            
         BASR  RE,RF               WRITE BACK COPY                              
RC14AX   B     RCDELETE                                                         
*                                                                               
RC14B    CLI   TRECTY,ADD          LOGICALLY DELETE DA FILE ADDED REC           
         BNE   RC14C                                                            
         BAS   R9,LDELETE                                                       
         MVC   P1,VWRITE                                                        
         BASR  RE,RF                                                            
         B     RCDELETE                                                         
*                                                                               
RC14C    B     RCDELETE            ERASE INVALID RCVR REC                       
                                                                                
***********************************************************************         
* INDEX SEQUENTIAL FILE                                               *         
***********************************************************************         
RC16     MVC   P1,VRKEY            READ IS FILE RECORD INTO INPUT AREA          
         L     RE,AINPUT                                                        
         ST    RE,P2                                                            
         LA    RE,RECORD           KEY IS IN RECOVERY RECORD                    
         ST    RE,P5                                                            
         ST    RE,DM3                                                           
         BASR  RE,RF                                                            
         CLI   DM3,0                                                            
         BNE   RCERR3              DISK READ ERROR IS FILE                      
         L     RE,P4                                                            
         USING ISDTF,RE                                                         
         LH    RE,ISKEYLN1         GET KEYLEN-1                                 
         DROP  RE                                                               
         L     RF,AINPUT           CAN'T GET HERE NOW                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),RECORD      COMPARE KEYS                                 
         L     RF,VDMOD000         RESET V(DMCNTL)                              
         BE    RC16A                                                            
*                                                                               
         CLI   TRECTY,ADD                                                       
         BE    RCDELETE            NOTFOUND OK FOR ADD                          
         B     RCERR3                                                           
*                                                                               
RC16A    CLI   TRECTY,COPY         OVERWRITE WITH RECOVERY COPY RECORD          
         BNE   RC16B                                                            
*                                                                               
         CLI   RFILTY,X'A1'        DON'T RECOVER LOCKET RECORDS                 
         BNE   *+12                                                             
         CLI   RECORD,C'8'                                                      
         BE    RC16AX                                                           
*                                                                               
         MVC   P1,VWKEY                                                         
         LA    RE,RECORD                                                        
         ST    RE,P2                                                            
         BASR  RE,RF                                                            
RC16AX   B     RCDELETE                                                         
*                                                                               
RC16B    CLI   TRECTY,ADD          ERASE IS FILE ADDED RECORD                   
         BNE   RC16C                                                            
         MVC   P1,VEKEY                                                         
         BASR  RE,RF                                                            
         B     RCDELETE                                                         
*                                                                               
RC16C    B     RCDELETE            ERASE INVALID RCVR REC                       
                                                                                
***********************************************************************         
* SEARCH FILE TABLE TO FIND LOGICAL DELETE FIELD AND SET TO FF'S      *         
***********************************************************************         
LDELETE  L     R3,VDMGRFLS         POINT TO DMGRFLES TABLE                      
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING DMGRFLD,R3                                                       
*                                                                               
LDEL2    CLI   0(R3),0             SEARCH FILE TABLE FOR FILE                   
         BE    LDEL3                                                            
         CLC   RFILTY,DMGFEXT      MATCH ON FILE NUMBER                         
         BE    LDEL4                                                            
LDEL3    BXLE  R3,R4,LDEL2                                                      
         B     LDEL6                                                            
*                                                                               
LDEL4    SR    R5,R5                                                            
         ICM   R5,1,DMGLDELL       R5=L'LOGICAL DELETE FIELD                    
         BZ    LDEL6                                                            
         BCTR  R5,0                                                             
         LLC   R4,DMGLDELD         LOGICAL DELETE FIELD DISPLACEMENT            
         A     R4,AINPUT           R4=A(LOGICAL DELETE FIELD)                   
         EX    R5,*+8                                                           
         B     LDELX                                                            
         MVC   0(0,R4),=8X'FF'     SET FIELD TO ALL FF'S                        
*                                                                               
LDEL6    B     RCDELETE            ERASE INVALID RCVR REC                       
*                                                                               
LDELX    BR    R9                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
* EXIT ROUTINE WITH RETURN CODE IN RF                                 *         
***********************************************************************         
RCDELETE SR    RF,RF               DELETE THIS RECOVERY RECORD                  
         B     RCXX                                                             
RCIGNORE LA    RF,4                IGNORE THIS RECOVERY RECORD                  
         B     RCXX                                                             
RCERR6   LA    RF,8                ERROR 6 - SIN HAS CHANGED                    
         B     RCXX                                                             
RCERR3   LA    RF,12               ERROR 3 - DISK READ ERROR                    
         B     RCXX                                                             
RCXX     XIT1  REGS=(RF)                                                        
         EJECT                                                                  
ADD      EQU   X'03'                                                            
CHANGE   EQU   X'02'                                                            
COPY     EQU   X'01'                                                            
*                                                                               
DMREAD   DC    C'DMREAD'                                                        
SYSFLES  DC    C'SYSFLES'                                                       
ERRMSG   DC    CL40' '                                                          
                                                                                
         USING VRCVNTRY,R3                                                      
OCVMONT  OC    VRCVNTRY(0),VRCVNTRY   IGNORE ZERO ENTRIES                       
XCVMONT  XC    VRCVNTRY(0),VRCVNTRY   CLEAR  ENTRIE                             
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
WRKD     DSECT                                                                  
*                                                                               
DMCB     DS    0CL24               DUMMY DMCB                                   
DM1      DS    F                                                                
DM2      DS    F                                                                
DM3      DS    F                                                                
DM4      DS    F                                                                
DM5      DS    F                                                                
DM6      DS    F                                                                
*                                                                               
P1       DS    F                   PARAM LIST FOR FILE I/O                      
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
Q1       DS    F                   PARAM LIST FOR RECOVERY FILE I/O             
Q2       DS    F                                                                
Q3       DS    F                                                                
Q4       DS    F                                                                
Q5       DS    F                                                                
Q6       DS    F                                                                
*                                                                               
DMCBUSSQ DS    0XL24                                                            
P1USS    DS    F                   COMMAND                                      
P2USS    DS    F                   SENUM                                        
P3USS    DS    F                   A(GIN)                                       
P4USS    DS    F                   N/D                                          
P5USS    DS    F                   N/D                                          
P6USS    DS    F                   N/D                                          
*                                                                               
DFLAG    DS    X                                                                
DFLAGVRB EQU   X'80'               VERMONT RECOVERY BUFFER                      
DFLAGGLO EQU   X'40'               GLOBAL SYSTEM FLAG                           
DFLAGACT EQU   X'20'               ACTIVE BUFFER IN IOAREA                      
DFLAGNOP EQU   X'10'               RECOVERY SYSTEM WAS NOPED                    
         DS    XL3                                                              
VERMONT  DS    C                                                                
VMONTREG EQU   C'R'                VERMONT REGULAR                              
VMONTXTN EQU   C'X'                VERMONT EXTENDED                             
*                                                                               
TASKIDS  DS    XL68                                                             
TASKSINS DS    XL256                                                            
TASKDONE DS    XL256                                                            
*                                                                               
RECNUMS  DS    256XL2                                                           
ARECBLK  DS    A                                                                
PREVDA   DS    A                                                                
VMNTRYLN DS    H                   LENGTH OF 1 VERMONT ENTRY                    
*                                                                               
ACTVID   DS    XL4                                                              
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
RCVCRCV  DS    H                                                                
RCVCBLK  DS    H                                                                
RECDA    DS    A                                                                
SVPARAM  DS    A                                                                
SVSE     DS    A                                                                
@RCVDTF  DS    A                   RECOVERY A(DTF)                              
@SYSFIL  DS    A                                                                
SVSYSFL  DS    A                                                                
ARECBUF  DS    A                                                                
LOWADR   DS    F                                                                
ERRNUM   DS    A                                                                
BLKSTRK  DS    H                                                                
NUMTASKS DS    H                                                                
SVSYSFLH DS    XL4                                                              
TRECTY   DS    XL1                                                              
RWRTPNDG DS    XL1                                                              
FACID    DS    XL1                                                              
SYSID    DS    XL1                                                              
SE#      DS    XL1                 SE NUMBER                                    
ENQINFO  DS    0CL13                                                            
ENQFLG   DS    XL1                                                              
ENQID    DS    CL4                                                              
ENQCMND  DS    CL8                                                              
AINPUT   DS    A                                                                
*                                                                               
GINS     DS    10XL8               1 GIN PER VERMONT TASK                       
*                                                                               
         DS    0D                                                               
RECVREC  DS    18432C              MAXIMUM RECOVERY FILE BLOCK IS 18K           
*                                                                               
         DS    0D                                                               
INPUT    DS    6144C               MAXIMUM LOGICAL RECORD IS 6K                 
WRKX     EQU   *                                                                
         EJECT                                                                  
RCVRREC  DSECT                                                                  
*DMRCVRHDR                                                                      
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RECORD   DS    6144C                                                            
         EJECT                                                                  
* DMRCVREXT                                                                     
RCVREXTD DSECT                                                                  
       ++INCLUDE DMRCVREXT                                                      
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
* DMDTFPH                                                                       
       ++INCLUDE DMDTFPH                                                        
* DMTABFD                                                                       
       ++INCLUDE DMTABFD                                                        
* DMSYSFD                                                                       
       ++INCLUDE DMSYSFD                                                        
* DMRCVRD                                                                       
       ++INCLUDE DMRCVRD                                                        
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* DMSPACED                                                                      
       ++INCLUDE DMSPACED                                                       
* DMDSYSHDR                                                                     
       ++INCLUDE DMDSYSHDR                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SRRCV00   09/22/19'                                      
         END                                                                    
