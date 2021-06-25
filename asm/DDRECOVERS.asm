*          DATA SET DDRECOVERS AT LEVEL 007 AS OF 05/01/02                      
*PHASE RECOVERA                                                                 
*INCLUDE FATABOFF                                                               
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'RECOVER - RECOVER ABENDED DATASPACED SYSTEMS '                  
         PRINT NOGEN                                                            
RECOVER  CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,**RECV**,=A(WORKAREA),RA,R9                          
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         L     R1,=A(IOAREA-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,AIOAREA                                                       
*                                                                               
         BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,INIT             READ CARDS ECT                               
         BAS   RE,MAIN             MAIN LOOP FOR INIT                           
         B     XBASE                                                            
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
EXITEQ   CR    RB,RB                                                            
         B     EXIT                                                             
EXITNE   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
M24SET   SLL   RE,1                SET AMODE 24                                 
         SRL   RE,1                                                             
         BSM   0,RE                                                             
*                                                                               
M31SET   ICM   RE,8,=X'80'         SET AMODE 31                                 
         BSM   0,RE                                                             
         EJECT                                                                  
*************************************************************                   
*        INITIALISE                                         *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
*                                                                               
         L     R1,=V(DDSIO)        USE DDSIOC                                   
*        MVC   0(8,R1),=C'DDSIOC  '                                             
*                                                                               
         LA    R3,CARD                                                          
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT020                                                          
         MVC   PLINE+1(80),0(R3)                                                
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BE    INIT010                                                          
         B     XBASE               NEQ MEANS INVALID KEYWORD                    
*                                                                               
INIT020  L     RE,=V(SSB)          SET DSPACE CHR IN SSB                        
         USING SSOOFF,RE                                                        
         MVC   SSODSPAC,DSPACE                                                  
         DROP  RE                                                               
*                                                                               
         XC    DMCB,DMCB           ENQUIRE ON ZERO                              
         OI    DMCB+4,X'20'                                                     
         GOTO1 =V(DATAMGR),DMCB,=C'LOCKSP' TO ALLOCATE DSPACE                   
         L     R1,8(R1)                                                         
         USING DMSPACED,R1         EXTRACT JOB NAME/NUMBER                      
         CLC   JNAME,SPACES                                                     
         BNE   *+10                                                             
         MVC   JNAME,DSPMVS                                                     
         OC    JNUM,JNUM                                                        
         BNZ   *+10                                                             
         MVC   JNUM,DSPJOB                                                      
         NC    JNUM,=X'7FFF'       TURN OFF 8000 BIT                            
         DROP  R1                                                               
*                                                                               
         L     RE,=V(SSB)          SET SSB PROTECT FLAG                         
         OI    5(RE),X'20'                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'SSBAD ' GET A(SSB)                           
         L     R1,4(R1)                                                         
         L     RE,=V(SSB)          COPY OVER MY SSB                             
         MVC   0(256,RE),0(R1)                                                  
*                                                                               
         USING SSBD,RE                                                          
         MVC   DMALET,SSBALET      EXTRACT ALET FROM SSB                        
         XC    DMOFFS,DMOFFS                                                    
         DROP  RE                                                               
         B     EXITEQ                                                           
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM                                       *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
*                                                                               
         SAC   512                 SET UP AMODE                                 
         LAM   RE,R1,=4F'0'                                                     
         LAM   R2,R2,DMALET                                                     
         L     R2,DMOFFS                                                        
*                                                                               
MAIN010  LA    R2,64(,R2)          FIRST / NEXT SYSTEM                          
         ST    R2,DINDEX                                                        
         C     R2,=X'00004000'     64*256 SYSTEM ENTRIES                        
         BE    MAINX                                                            
         USING DMSPACED,R2                                                      
*                                                                               
         MVC   SENAM,16(R2)        SAVE SYMBOLIC NAME                           
         SR    R1,R1                                                            
         ICM   R1,7,DSPECB+1       GET SYSTEM HEADER                            
         BZ    MAIN010                                                          
         LR    R2,R1                                                            
         ST    R2,DSYSHDR          SAVE A(SYSTEM HEADER)                        
         USING DMSYSHDR,R2                                                      
         MVC   SENUM,DSYSENUM+1    SAVE SE NUMBER                               
*                                                                               
         L     R2,DSYAJOBS         GET JOB TABLE                                
         USING DSJOBHDR,R2                                                      
*                                                                               
         LA    R0,32               32 ENTRIES                                   
MAIN020  CLC   DSJOBNAM,JNAME                                                   
         BNE   MAIN025                                                          
*NOP     CLC   DSJOBNUM,JNUM       HAVE WE GOT A MATCH                          
*NOP     BNE   MAIN025                                                          
*                                                                               
         MVC   ADVNUM,DSJOBADV     SAVE ADVNUM THEN CLEAR IT                    
         XC    DSJOBNAM(16),DSJOBNAM                                            
*                                                                               
         BAS   RE,INFO1            PRINT DISCONECT MSG                          
*                                                                               
         L     R2,DSYSHDR          RETURN TO SYSTEM HEADER                      
         USING DMSYSHDR,R2                                                      
         ICM   R2,15,DSYABUFF                                                   
         BZ    *+8                                                              
         BAS   RE,RECVR            RECOVER TRANSACTIONS                         
         B     MAIN030                                                          
*                                                                               
MAIN025  LA    R2,16(,R2)          NEXT JOBTAB ENTRY                            
         BCT   R0,MAIN020                                                       
*                                                                               
MAIN030  SAC   512                                                              
         L     R2,DSYSHDR          RETURN TO SYSTEM HEADER                      
         ICM   R2,15,DSYALOCK                                                   
         BZ    *+8                                                              
         BAS   RE,UNLOCK           UNLOCK LOCKTAB                               
         B     MAIN990                                                          
*                                                                               
MAIN990  SAC   512                                                              
         L     R2,DINDEX           LOOP BACK FOR NEXT                           
         B     MAIN010                                                          
*                                                                               
MAINX    B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        RECOVER TRANSACTIONS STILL IN PROGRESS             *                   
*************************************************************                   
         SPACE 1                                                                
RECVR    NTR1                                                                   
*                                                                               
         LA    R0,10               10 ACTIVE ENTRIES IN BUFFER                  
         LA    R2,16(,R2)                                                       
*                                                                               
RECV010  CLI   ADVNUM,0            WERE WE ONLINE                               
         BE    RECV020                                                          
*                                                                               
         CLC   ADVNUM,0(R2)        YES SO BACK OUT ALL SINS FOR SYS             
         BE    RECV100                                                          
         B     RECV090                                                          
*                                                                               
RECV020  CLI   0(R2),X'FF'         OFFLINE ENTRY                                
         BNE   RECV090                                                          
         CLC   JNUM,2(R2)          MATCH ON JOB NUMBER                          
         BE    RECV100                                                          
*                                                                               
RECV090  LA    R2,8(,R2)           NEXT ACTIVE ENTRY                            
         BCT   R0,RECV010                                                       
*                                                                               
RECVDX   B     EXITEQ              NOTHING TO DO FOR THIS SYSTEM                
         EJECT                                                                  
************************************************************                    
*        OPEN SYSTEM FOR RECOVERY                          *                    
************************************************************                    
         SPACE 1                                                                
RECV100  XC    RCVCRCV,RCVCRCV     CLEAR COUNTS                                 
         XC    RCVCERR,RCVCERR                                                  
*                                                                               
         SAC   0                   WE HAVE SOME RECOVERING TO DO                
         L     R4,=V(SELIST)       MUST HAVE SELIST                             
         LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
*                                                                               
         USING SELISTD,R4                                                       
RECV110  CLC   SESYS,SENUM         TEST NUM                                     
         BE    RECV120                                                          
         BXLE  R4,R0,RECV110       NEXT                                         
         DC    H'0'                ERROR SE SYS NOT FOUND                       
*                                                                               
RECV120  MVC   SENAM,SENAME        SAVE NAME                                    
         DROP  R4                                                               
*                                                                               
         MVC   UTL+4(1),SENUM      SET UTL                                      
*                                                                               
         XC    DMCB,DMCB           DO SYSFLES CALL FOR FILES                    
         MVC   DMCB+11(1),SENUM                                                 
         GOTO1 =V(DATAMGR),DMCB,DMREAD,=C'SYSFLES'                              
         ICM   R1,15,12(R1)                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,0(R1)            R1=SE FILES                                  
         ST    R1,ASYSFLES                                                      
*                                                                               
         CLC   SENUM,0(R1)         CONFIRM FILE LIST                            
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    SEFILN,SEFILN       CLEAR DOWN                                   
         LA    R4,SEFILN                                                        
*                                                                               
         SR    RE,RE               RE=NUMBER OF FILES                           
         ICM   RE,3,2(R1)                                                       
         LA    R1,4(R1)            POINT R1 TO FILES                            
*                                                                               
RECV130  SR    RF,RF                                                            
         ICM   RF,7,5(R1)          RF=DTF ADDR                                  
         TM    0(R1),X'40'         TEST FOR RECOVERY FILE                       
         BZ    *+8                                                              
         ST    RF,ARCVDTF          SAVE DTFADDR                                 
         MVI   0(R4),C'U'                                                       
         MVC   1(7,R4),22(RF)      GET NAME                                     
*                                                                               
         LA    R4,8(R4)            NEXT FILE                                    
         LA    R1,8(R1)                                                         
         BCT   RE,RECV130                                                       
         MVI   0(R4),C'X'          END OF                                       
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,OPEN,SENAM,SEFILN,AIOAREA,0                     
*                                                                               
         EJECT                                                                  
************************************************************                    
*        RECOVER TRANSACTIONS FOR THIS SYSTEM              *                    
************************************************************                    
         SPACE 1                                                                
RECV200  XC    DMCB,DMCB           DSPACE RESOURCE NUMBER                       
         MVC   DMCB+7(1),SENUM                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'LOCKSP'                                      
*                                                                               
         L     R2,8(R1)            R2 = SYSTEM HEADER                           
         USING DMSPACED,R2                                                      
*                                                                               
         MVC   ARECBUF,DSPECB                                                   
*                                                                               
         SAC   512                 COPY BUFFER FROM DSPACE                      
         LAM   R2,R2,DMALET                                                     
         L     R2,ARECBUF          R2 = SYS HEADER                              
         USING DMSYSHDR,R2                                                      
         L     R2,DSYABUFF         R2 = A(BUFFER)                               
         ST    R2,ARECBUF                                                       
         L     RE,AIOAREA          COPY IT TO IOAREA                            
         LH    RF,4(,R2)                                                        
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
         MVI   DFLAG,C'Y'          FLAG ACTIVE DSPACE BUFFER                    
*                                                                               
         XC    VERMONTS,VERMONTS   CLEAR VERMONTS TABLE                         
         LA    R1,VERMONTS                                                      
         LA    R0,10               10 ACTIVE ENTRIES IN BUFFER                  
         L     R2,ARECBUF                                                       
         LA    R2,16(,R2)                                                       
*                                                                               
RECV210  CLI   ADVNUM,0            WERE WE OFFLINE                              
         BE    RECV220                                                          
*                                                                               
         CLC   ADVNUM,0(R2)        NO SO BACK OUT ALL SINS FOR SYS              
         BE    RECV225                                                          
         B     RECV230                                                          
*                                                                               
RECV220  CLI   0(R2),X'FF'         OFFLINE ENTRY                                
         BNE   RECV230                                                          
         CLC   JNUM,2(R2)          MATCH ON JOB NUMBER                          
         BNE   RECV230                                                          
*                                                                               
RECV225  MVC   0(4,R1),0(R2)       COPY VERMONT ENTRY                           
         LA    R1,4(R1)                                                         
*                                                                               
RECV230  LA    R2,8(,R2)           NEXT ACTIVE ENTRY                            
         BCT   R0,RECV210                                                       
         SAC   0                   TURN OFF ACCESS MODE                         
*                                                                               
         B     RECV300                                                          
         EJECT                                                                  
************************************************************                    
*        READ A RECOVERY BLOCK DA=PREVDA INTO IOAREA       *                    
************************************************************                    
         SPACE 1                                                                
RECV250  L     RE,AIOAREA                                                       
         LA    R2,DMCB             READ RECOVERY FILE BLOCK                     
         MVC   RECDA,PREVDA                                                     
         MVI   RECDA+3,0                                                        
         GOTO1 =V(DMOD000),Q1,A(DMODREAD),AIOAREA,,ARCVDTF,RECDA                
         OC    Q3(2),Q3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         XC    PREVDA,PREVDA                                                    
         MVI   DFLAG,C'N'          NOT ACTIVE DSPACE BUFFER                     
         EJECT                                                                  
************************************************************                    
*        RECOVER RECORDS IN THIS BUFFER                    *                    
************************************************************                    
         SPACE 1                                                                
RECV300  L     RE,AIOAREA          SAVE A(ACTIVE) BLOCK                         
*                                                                               
         XC    RECNUMS+000(256),RECNUMS+000                                     
         XC    RECNUMS+256(256),RECNUMS+256                                     
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,RECNUMS          BUILD LIST OF OFFSETS                        
         LA    R2,96(,R2)                                                       
RECV310  LR    R1,R2                                                            
         S     R1,AIOAREA          CALC OFFSET                                  
         STCM  R1,3,0(R3)                                                       
         LA    R3,2(R3)            NEXT TABLE ENTRY                             
         SR    R1,R1                                                            
         ICM   R1,3,0(R2)          GET LENGTH                                   
         BZ    RECV320                                                          
         AR    R2,R1               NEXT RECORD                                  
         B     RECV310                                                          
*                                                                               
RECV320  LA    R0,10                                                            
         L     R3,AIOAREA          FIND 1ST ACTIVE ENTRY                        
         LA    R3,16(R3)                                                        
*                                                                               
RECV330  OC    0(8,R3),0(R3)       IGNORE ZERO ENTRIES                          
         BZ    RECV340                                                          
*                                                                               
         LA    R1,VERMONTS         SCAN VERMONTS FOR A MATCH                    
RECV331  OC    0(4,R1),0(R1)                                                    
         BZ    RECV340                                                          
         CLC   0(4,R1),0(R3)       GOT ONE - GO PROCESS IT                      
         BE    RECV400                                                          
         LA    R1,4(R1)                                                         
         B     RECV331             NEXT VERMONT                                 
*                                                                               
RECV340  LA    R3,8(R3)            NEXT HEADER ENTRY                            
         BCT   R0,RECV330                                                       
         EJECT                                                                  
************************************************************                    
*        THIS BUFFER HAS BEEN PROCESSED                    *                    
************************************************************                    
         SPACE 1                                                                
         CLI   DFLAG,C'Y'          COPY ACTIVE BUFFER BACK                      
         BNE   RECV350                                                          
*                                                                               
         SAC   512                 POINT TO DSPACE BUFFER                       
         LAM   R2,R2,DMALET                                                     
         L     R2,ARECBUF                                                       
         L     RE,AIOAREA          COPY RECORD TO DSPACE                        
         LH    RF,4(,R2)                                                        
         LR    R3,RF                                                            
         MVCL  R2,RE                                                            
         SAC   0                   TURN OFF ACCESS MODE                         
*                                                                               
RECV350  L     RE,ARCVDTF          SET BLOCK SIZE                               
         ST    RE,Q4                                                            
         ICM   R0,3,DBLKSZ-DTFPHD(RE)                                           
         ST    R0,Q3                                                            
         LA    R2,DMCB                                                          
         L     RE,AIOAREA          WRITE BUFFER TO DISK                         
         ST    RE,Q2                                                            
         MVC   RECDA,0(RE)                                                      
         MVI   RECDA+3,0                                                        
         GOTO1 =V(DMOD000),Q1,A(DMODWRI),,,,RECDA                               
         OC    Q3(2),Q3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    PREVDA,PREVDA       ANY PREVIOUS RECORD TO DO                    
         BNZ   RECV250                                                          
*                                                                               
RECVSX   BAS   RE,INFO2                                                         
*                                                                               
         XC    DMCB,DMCB           UNLOCK DSPACE RESOURCE                       
         MVC   DMCB+7(1),SENUM                                                  
         OI    DMCB+4,X'10'                                                     
         GOTO1 =V(DATAMGR),DMCB,=C'LOCKSP'   UNLOCK RESOURCE                    
         B     EXIT                                                             
         EJECT                                                                  
************************************************************                    
*        IS RECOVERY RECORD IN THIS RECOVERY BLOCK         *                    
************************************************************                    
         SPACE 1                                                                
RECV400  EQU   *                                                                
         L     R1,AIOAREA                                                       
         CLC   4(3,R3),0(R1)       IS RECORD IN BLOCK                           
         BE    RECV500                                                          
*                                                                               
         CLC   4(3,R3),PREVDA      SAVE THE HIGHEST PREVIOUS DA                 
         BL    *+10                                                             
         MVC   PREVDA(3),4(R3)     THIS IS HIGHER SO SAVE IT                    
*                                                                               
         CLI   DFLAG,C'Y'          ARE WE STILL ON ACTIVE BLOCK                 
         BNE   RECV340                                                          
         XC    0(8,R3),0(R3)       YES SO CLEAR VERMONT HEADER                  
         B     RECV340                                                          
         EJECT                                                                  
************************************************************                    
*        PROCESS INDIVIDUAL RECOVERY RECORD                *                    
************************************************************                    
         SPACE 1                                                                
RECV500  SR    R4,R4               INDEX INTO RECNUMS TO FIND RECORD            
         IC    R4,7(R3)                                                         
         BCTR  R4,0                                                             
         SLL   R4,1                                                             
         LA    R4,RECNUMS(R4)      R4 INDEX                                     
         LH    R1,0(R4)                                                         
*                                                                               
         L     R6,AIOAREA          POINT R6 TO RECORD                           
         AR    R6,R1                                                            
         SR    R1,R1                                                            
         ICM   R1,3,0(R6)          GET RECLEN IN R1                             
         ST    R1,P3                                                            
         LA    R6,6(R6)                                                         
         USING RCVRREC,R6                                                       
*                                                                               
         CLI   RTASKID,X'FF'       IGNORE LOGICALLY DELETED RECORDS             
         BE    RECV510                                                          
*                                                                               
         GOTO1 PROCESS             GO PROCESS RECOVERY RECORD                   
*                                                                               
RECV510  MVI   RTASKID,X'FF'       LOGICALLY DELETE RECORD                      
         LH    R1,RCVCRCV                                                       
         LA    R1,1(R1)            BUMP RECOVERED COUNT                         
         STH   R1,RCVCRCV                                                       
*                                                                               
         LTR   RF,RF               TEST ERROR RETURN                            
         BZ    RECV520                                                          
         LH    R1,RCVCERR                                                       
         LA    R1,1(R1)            BUMP ERROR COUNT                             
         STH   R1,RCVCERR                                                       
*                                                                               
RECV520  SH    R6,=H'6'            LINK BACK TO PREVIOUS                        
         OC    2(4,R6),2(R6)                                                    
         BNZ   *+14                                                             
         XC    0(8,R3),0(R3)       CLEAR THIS ENTRY                             
         B     RECV340                                                          
         MVC   4(4,R3),2(R6)       BACK UP TO PREVIOUS                          
         B     RECV400                                                          
         EJECT                                                                  
***********************************************************************         
* PROCESS RECOVERY FILE RECORD                                        *         
* R6=A(RECORD) AND P3 CONTAINS LENGTH                                 *         
***********************************************************************         
         SPACE 1                                                                
PROCESS  NTR1                                                                   
         USING RCVRREC,R6                                                       
*                                                                               
RCDSPACE MVC   TRECTY,RRECTY       DON'T CHANGE IN RECORD                       
         NI    TRECTY,X'7F'        TURN OFF 'POINTER' BIT                       
         CLI   TRECTY,CHANGE                                                    
         BE    RCDELETE            DELETE CHANGES FROM RECOVERY FILE            
*                                                                               
         L     R7,ASYSFLES         SEARCH FOR FILE IN SYSFLES LIST              
         SR    R8,R8                                                            
         IC    R8,3(R7)                                                         
         LA    R7,4(R7)                                                         
RC12A    CLC   RFILTY,3(R7)        MATCH FILE NUMBER                            
         BE    RC12B                                                            
         LA    R7,8(R7)                                                         
         BCT   R8,RC12A                                                         
         B     RCDELETE            ERASE INVALID RCVR REC                       
*                                                                               
RC12B    L     RE,4(R7)            GET DTF ADDRESS                              
         ST    RE,P4                                                            
         L     RF,=V(DMOD000)      SET V(DMCNTL)                                
         LA    R1,P1               SET A(PARAMS)                                
         TM    0(R7),X'01'         TEST FILE TYPE                               
         BO    RC16                IS FILE                                      
         B     RC14                DA FILE                                      
         EJECT                                                                  
***********************************************************************         
* DIRECT ACCESS FILE                                                  *         
***********************************************************************         
         SPACE 1                                                                
RC14     LA    RE,RVCHR            READ DA FILE RECORD INTO INPUT AREA          
         ST    RE,P5                                                            
         MVC   P1,=A(DMODREAD)                                                  
         LA    RE,INPUT                                                         
         ST    RE,P2                                                            
         BASR  RE,RF                                                            
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
         MVC   P1,=A(DMODWRI)                                                   
         LA    RE,RECORD                                                        
         ST    RE,P2                                                            
         BASR  RE,RF                                                            
         B     RCDELETE                                                         
*                                                                               
RC14B    CLI   TRECTY,ADD          LOGICALLY DELETE DA FILE ADDED REC           
         BNE   RC14C                                                            
         BAS   R9,LDELETE                                                       
         MVC   P1,=A(DMODWRI)                                                   
         BASR  RE,RF                                                            
         B     RCDELETE                                                         
*                                                                               
RC14C    B     RCDELETE            ERASE INVALID RCVR REC                       
         EJECT                                                                  
***********************************************************************         
* INDEX SEQUENTIAL FILE                                               *         
***********************************************************************         
         SPACE 1                                                                
RC16     MVC   P1,=A(DMODRKEY)     READ IS FILE RECORD INTO INPUT AREA          
         LA    RE,INPUT                                                         
         ST    RE,P2                                                            
         LA    RE,RECORD           KEY IS IN RECOVERY RECORD                    
         ST    RE,P5                                                            
         ST    RE,DM3                                                           
         BASR  RE,RF                                                            
         CLI   DM3,0                                                            
         BNE   RCERR3              DISK READ ERROR IS FILE                      
         L     RE,P4                                                            
         LH    RE,46(RE)           GET KEYLEN-1                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   INPUT(0),RECORD     COMPARE KEYS                                 
         BE    RC16A                                                            
         CLI   TRECTY,ADD                                                       
         BE    RCDELETE            NOTFOUND OK FOR ADD                          
         B     RCERR3                                                           
*                                                                               
RC16A    CLI   TRECTY,COPY         OVERWRITE WITH RECOVERY COPY RECORD          
         BNE   RC16B                                                            
         MVC   P1,=A(DMODWKEY)                                                  
         LA    RE,RECORD                                                        
         ST    RE,P2                                                            
         BASR  RE,RF                                                            
         B     RCDELETE                                                         
*                                                                               
RC16B    CLI   TRECTY,ADD          ERASE IS FILE ADDED RECORD                   
         BNE   RC16C                                                            
         MVC   P1,=A(DMODEKEY)                                                  
         BASR  RE,RF                                                            
         B     RCDELETE                                                         
*                                                                               
RC16C    B     RCDELETE            ERASE INVALID RCVR REC                       
         EJECT                                                                  
***********************************************************************         
* SEARCH FILE TABLE TO FIND LOGICAL DELETE FIELD AND SET TO FF'S      *         
***********************************************************************         
         SPACE 1                                                                
LDELETE  L     R3,ASYSFLES         POINT TO DMGRFLES TABLE                      
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
*                                                                               
LDEL2    CLI   0(R3),0             SEARCH FILE TABLE FOR FILE                   
         BE    *+14                                                             
         CLC   RFILTY,8(R3)                                                     
         BE    LDEL4                                                            
         BXLE  R3,R4,LDEL2                                                      
         B     LDEL6                                                            
*                                                                               
LDEL4    SR    R5,R5                                                            
         IC    R5,9(R3)            R5=L'LOGICAL DELETE FIELD                    
         LTR   R5,R5                                                            
         BZ    LDEL6                                                            
         BCTR  R5,0                                                             
         SR    R4,R4                                                            
         IC    R4,10(R3)                                                        
         LA    R4,INPUT(R4)        R4=A(LOGICAL DELETE FIELD)                   
         EX    R5,*+8                                                           
         B     LDELX                                                            
         MVC   0(0,R4),=8X'FF'     SET FIELD TO ALL FF'S                        
*                                                                               
LDEL6    B     RCDELETE            ERASE INVALID RCVR REC                       
*                                                                               
LDELX    BR    R9                                                               
         SPACE 2                                                                
***********************************************************************         
* EXIT ROUTINE WITH RETURN CODE IN RF                                 *         
***********************************************************************         
         SPACE 1                                                                
RCDELETE SR    RF,RF               DELETE THIS RECOVERY RECORD                  
         B     RCXX                                                             
RCERR3   LA    RF,12               ERROR 3 - DISK READ ERROR                    
         B     RCXX                                                             
RCXX     XIT1  REGS=(RF)                                                        
         EJECT                                                                  
ADD      EQU   X'03'                                                            
CHANGE   EQU   X'02'                                                            
COPY     EQU   X'01'                                                            
*                                                                               
SYSFLES  DC    C'SYSFLES'                                                       
         EJECT                                                                  
*************************************************************                   
*        UNLOCK ALL LOCKTAB ENTRIES HELD                    *                   
*************************************************************                   
         SPACE 1                                                                
UNLOCK   NTR1                                                                   
*                                                                               
UNLK010  XC    LOCKCNT,LOCKCNT     CLEAR COUNTER                                
         SAC   0                                                                
*                                                                               
UNLK020  XC    DMCB,DMCB           DSPACE RESOURCE NUMBER                       
         MVC   DMCB+7(1),SENUM                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'LOCKSP'                                      
*                                                                               
         L     R2,8(R1)                                                         
         USING DMSPACED,R2                                                      
         MVC   ALOCKTAB,DSPECB     SAVE RESOURCE ADDRESS                        
*                                                                               
         SAC   512                                                              
         LAM   R2,R2,DMALET                                                     
         L     R2,ALOCKTAB         R2=A(RESOURCE)                               
         L     R2,DSYALOCK-DMSYSHDR(,R2)                                        
         ST    R2,ALOCKTAB         R2=A(LOCKTAB)                                
         USING LKTABD,R2                                                        
*                                                                               
         LH    R7,2(,R2)           R7=NUM OF ACTIVE ENTRIES                     
         LA    R2,16(,R2)          POINT TO FIRST ACTIVE ENTRY                  
         LTR   R0,R7               R0=NUM OF ENTRIES AFTER UNLOCK               
         BZ    UNLK090             EXIT IF NO ENTRIES AT ALL                    
*                                                                               
UNLK030  CLI   LKFILE,0            IGNORE BLANKS                                
         BNE   *+12                                                             
         LA    R2,L'LKDATA(,R2)                                                 
         B     UNLK030                                                          
*                                                                               
         CLI   ADVNUM,0                                                         
         BNE   UNLK040                                                          
         CLC   LKJOB,JNUM          TEST SAME JOB ID                             
         BNE   UNLK050                                                          
         B     UNLK045                                                          
*                                                                               
UNLK040  CLC   LKJOB(1),ADVNUM     TEST SAME ADV SYSTEM                         
         BNE   UNLK050                                                          
*                                                                               
UNLK045  LH    R1,LOCKCNT          BUMP COUNT OF UNLOCKED RECORDS               
         LA    R1,1(R1)                                                         
         STH   R1,LOCKCNT                                                       
         XC    LKDATA,LKDATA       CLEAR ENTRY FOR THIS RECORD                  
         BCTR  R0,0                AND UPDATE REMAINING COUNT                   
*                                                                               
UNLK050  LA    R2,L'LKDATA(,R2)                                                 
         BCT   R7,UNLK030          NEXT ENTRY                                   
*                                                                               
UNLK060  L     R2,ALOCKTAB                                                      
         STH   R0,2(,R2)           SET NEW COUNT IN LOCK TABLE                  
*                                                                               
UNLK090  BAS   RE,FREESYS          FREE UP SYSTEM                               
*                                                                               
         BAS   RE,INFO3            REPORT ON UNLOCKED                           
*                                                                               
         SAC   512                                                              
         B     UNLKFREE                                                         
*                                                                               
FREESYS  SAC   0                                                                
         ST    RE,SAVERE           FREE RESOURCE                                
         XC    DMCB,DMCB           DSPACE RESOURCE NUMBER                       
         MVC   DMCB+7(1),SENUM                                                  
         OI    DMCB+4,X'10'                                                     
         GOTO1 =V(DATAMGR),DMCB,=C'LOCKSP'                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
************************************************************                    
*        FREE ECBS WAITING ON THIS TASK                    *                    
************************************************************                    
         SPACE 1                                                                
UNLKFREE SAC   512                                                              
         L     R2,DMOFFS           FREE ECBS FOR THIS LOCKID                    
         L     R2,0(,R2)           GET A(ECBS)                                  
*                                                                               
         ICM   RF,15,0(R2)         RF=NUMBER OF ACTIVE ECBS                     
         BZ    UNLKXXX                                                          
*                                                                               
UNLK100  LA    R2,8(,R2)           POINT TO FIRST / NEXT ENTRY                  
         OC    0(2,R2),0(R2)                                                    
         BZ    UNLK100             IGNORE ZERO ENTRIES                          
         CLI   ADVNUM,0                                                         
         BE    UNLK110                                                          
         CLC   0(2,R2),JNUM        TEST SAME JOB ID                             
         BNE   UNLK160                                                          
         B     UNLK150                                                          
*                                                                               
UNLK110  CLC   0(1,R2),ADVNUM      TEST SAME ADV SYSTEM                         
         BNE   UNLK160                                                          
*                                                                               
UNLK150  OI    4(R2),X'40'         POST COMPLETE                                
*                                                                               
UNLK160  LA    R2,8(,R2)           NEXT ECB                                     
         BCT   RF,UNLK100                                                       
*                                                                               
UNLKXXX  SAC   0                   SET AMODE BACK                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
         SPACE 1                                                                
INFO1    NTR1                                                                   
         L     R2,DINDEX           PRINT DISCON MESSAGE                         
         MVC   PLINE,SPACES                                                     
         MVC   PLINE+01(07),SENAM                                               
         MVC   PLINE+10(17),=C'DISCONNECTED FROM'                               
         MVC   PLINE+29(08),JNAME                                               
         SAC   0                                                                
         BAS   RE,SQUASH                                                        
         BAS   RE,PRINTL                                                        
         SAC   512                                                              
         B     EXIT                                                             
*                                                                               
INFO2    NTR1                                                                   
         MVC   PLINE,SPACES                                                     
         MVC   PLINE+01(07),SENAM                                               
         MVC   PLINE+10(24),=C'NO     RECORDS RECOVERED'                        
         OC    RCVCRCV,RCVCRCV                                                  
         BZ    INFO2A                                                           
         EDIT  (B2,RCVCRCV),(6,PLINE+9)                                         
         OC    RCVCERR,RCVCERR                                                  
         BZ    INFO2A                                                           
         MVC   PLINE+35(13),=C'...... ERRORS'                                   
         EDIT  (B2,RCVCERR),(6,PLINE+34)                                        
INFO2A   BAS   RE,SQUASH                                                        
         BAS   RE,PRINTL                                                        
         B     EXIT                                                             
*                                                                               
INFO3    NTR1                                                                   
         MVC   PLINE,SPACES                                                     
         MVC   PLINE+01(07),SENAM                                               
         MVC   PLINE+10(21),=C'NO     LOCKS REMOVED.'                           
         OC    LOCKCNT,LOCKCNT                                                  
         BZ    INFO3A                                                           
         EDIT  (B2,LOCKCNT),(6,PLINE+9)                                         
INFO3A   BAS   RE,SQUASH                                                        
         BAS   RE,PRINTL                                                        
         B     EXIT                                                             
*                                                                               
SQUASH   ST    RE,SAVERE                                                        
         MVC   BYTE,PLINE                                                       
         MVI   PLINE,C'X'          REPLACE PLINE+1 WITH SOMETHING               
         GOTO1 =V(SQUASHER),DMCB,PLINE,80                                       
         MVC   PLINE(1),BYTE                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
         SPACE 1                                                                
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
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
         PUT   SYSPRINT,T1         PRINT TITLE                                  
         PUT   SYSPRINT,T2                                                      
         PUT   SYSPRINT,T3                                                      
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN),X'FLAGS',AL3(OUTPUT)             
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*        X'0100'                   DATE VALUE                                   
*                                                                               
         SPACE 1                                                                
CARDTAB  DS    0F                                                               
         DC    C'DSPACE ',AL1(5,4),X'0000',AL3(DSPACE)                          
         DC    C'MODE   ',AL1(3,10),X'0000',AL3(MODE)                           
         DC    C'JOBNAME',AL1(6,08),X'0000',AL3(JNAME)                          
         DC    C'JOBNUM ',AL1(5,02),X'0800',AL3(JNUM)                           
         DC    X'0000'                                                          
*                                                                               
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
MODE     DC    CL10'INIT'          DEFAULT TO INIT                              
JNAME    DC    CL8' '                                                           
JNUM     DC    XL2'0000'                                                        
DSPACE   DC    C'TEST'             P OR T DEFAULT TO TEST                       
         EJECT                                                                  
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITEQU                                                          
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC500                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         CLC   0(2,RF),=C'  '      EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC500                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC500                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BAS   RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC500                                                          
*                                                                               
VALC080  TM    9(R4),X'01'         DATE INPUT                                   
         BZ    VALC400                                                          
         LA    R0,1(R1)            SET R0 INPUT LEN                             
         ST    RF,FULL                                                          
         GOTO1 =V(PERVAL),DMCB,((R0),(R2)),(X'60',WORK)                         
         L     RF,FULL                                                          
         CLI   4(R1),X'04'                                                      
         BNE   CERRDAT                                                          
         MVC   0(2,RF),WORK+PVALCSTA-PERVALD                                    
         B     VALC500                                                          
*                                                                               
VALC400  CLI   8(R4),0             DONT CARE                                    
         BE    VALC410                                                          
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         SR    RE,RE                                                            
         IC    RE,8(R4)            PAD OUT TO SPACES                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SPACES                                                   
VALC410  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC500  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC500                                                          
*                                                                               
EXITEQU  CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDAT  LA    R1,=C'INVALID DATE    '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
EXITNEQ  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU  *                   
*************************************************************                   
         SPACE 1                                                                
VALTIME  NTR1                                                                   
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         BNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         B     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALF                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BAS   RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BAS   RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BAS   RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BAS   RE,VALTADD                                                       
         B     EXITEQU                                                          
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LTORG                                  *                   
*************************************************************                   
         SPACE 1                                                                
DMREAD   DC    CL8'DMREAD '                                                     
OPEN     DC    CL8'DMOPEN'                                                      
SPACES   DC    CL166' '                                                         
STARS    DC    16C'*'                                                           
FFS      DC    16X'FF'                                                          
MAXLINE  DC    PL3'60'                                                          
         SPACE 2                                                                
T1       DC    166C' '                                                          
T2       DC    166C' '                                                          
T3       DC    166C' '                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        DCBS & ADCONS                                      *                   
*************************************************************                   
         SPACE 2                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
UTL      DC    F'0',AL1(00)                                                     
         DS    0D                                                               
SSB      DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'02' NO RECOVERY                                  
         ORG                                                                    
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE DC                                 *                   
*************************************************************                   
         SPACE 1                                                                
         DS    0D                                                               
WORKAREA DC    60000X'00'                                                       
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 2                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
         DS    0F                                                               
DMCB     DS    0CL24                                                            
DM1      DS    F                                                                
DM2      DS    F                                                                
DM3      DS    F                                                                
DM4      DS    F                                                                
DM5      DS    F                                                                
DM6      DS    F                                                                
CARDEND  DS    A                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
AHEADER  DS    A                   ADDRESS OF CURRENT HEADER                    
ARCVDTF  DS    A                   ADDRESS OF DTF FOR RECOVERY                  
ALOCKTAB DS    A                   ADDRESS OF LOCKTAB                           
*                                                                               
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
WAITER   DS    A                                                                
*                                                                               
DMOFFS   DS    A                   DATASPACE OFFSET                             
DMALET   DS    A                   ALET                                         
*                                                                               
DINDEX   DS    A                   INDEX INTO DSOACE                            
DSYSHDR  DS    A                   CURRENT SYS HDR                              
*                                                                               
ASYSFLES DS    A                                                                
TRECTY   DS    XL1                 RECORD TYPE                                  
ADVNUM   DS    XL1                 RESOURCE NUMBER                              
RESNUM   DS    XL1                 RESOURCE NUMBER                              
SENUM    DS    CL1                 SE NUMBER                                    
SENAM    DS    CL8                 SE NAME                                      
SEFILN   DS    CL256               FILENAMES FOR OPEN                           
*                                                                               
RECNUMS  DS    256XL2                                                           
VERMONTS DS    16XL4               VERMONTS TO BE RECOVERED                     
ARECBUF  DS    A                   A(DSPACE BUFFER)                             
AIOAREA  DS    A                   A(IOAREA)                                    
PREVDA   DS    F                                                                
RECDA    DS    F                                                                
RCVCRCV  DS    H                   RECOVERY COUNT                               
RCVCERR  DS    H                   ERROR COUNT                                  
LOCKCNT  DS    H                   LOCK COUNT                                   
DFLAG    DS    X                                                                
*                                                                               
Q1       DS    F                                                                
Q2       DS    F                                                                
Q3       DS    F                                                                
Q4       DS    F                                                                
Q5       DS    F                                                                
Q6       DS    F                                                                
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
INPUT    DS    2524C                                                            
*                                                                               
IOAREA   DS    10000C                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        RECOVERY RECORD DSECT                              *                   
*************************************************************                   
         SPACE 2                                                                
RCVRREC  DSECT                                                                  
*DMRCVRHDR                                                                      
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RECORD   DS    2000C                                                            
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 2                                                                
* FASSBOFF                                                                      
* DMLKTABA                                                                      
* DMDSYSHDR                                                                     
* DMSPACED                                                                      
* DMDFTPH                                                                       
* DMDTFIS                                                                       
* DDPERVALD                                                                     
* DMGREQUS                                                                      
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE DMLKTABA                                                       
       ++INCLUDE DMDSYSHDR                                                      
       ++INCLUDE DMSPACED                                                       
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DMGREQUS                                                       
       ++INCLUDE FAD                                                            
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DDRECOVERS05/01/02'                                      
         END                                                                    
