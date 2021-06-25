*          DATA SET SRMQT00    AT LEVEL 010 AS OF 02/12/08                      
*PHASE T16A00A                                                                  
         PRINT NOGEN                                                            
         TITLE '$MQT - MQ SERIES FACPAK INTERFACE TEST PROGRAM'                 
MQT      CSECT                                                                  
         NMOD1 WORKL,**$MQT**,R9,CLEAR=YES,RR=RE                                
         USING WORKD,RC             RC=A(W/S)                                   
         ST    RE,RELO                                                          
         ST    R1,APARMS                                                        
         ST    RD,SAVERD                                                        
         USING SRPARMD,R1                                                       
         L     RA,SRQATWA                                                       
         USING SRMQTFFD,RA         RA=A(TWA)                                    
         L     R7,SRQASYSF                                                      
         USING SYSFACD,R7          R7=A(SYSFACS)                                
         L     R8,SRQAUTL                                                       
         USING UTLD,R8             R8=A(UTL)                                    
         BRAS  RE,INIT                                                          
         BRAS  RE,VALACT                                                        
         BRAS  RE,VALOPT                                                        
         BRAS  RE,VALDATA                                                       
*                                                                               
         ICM   RF,15,ROUTINE       GET ACTION ROUTINE                           
         BZ    XMOD                                                             
         BASR  RE,RF               AND GOTO IT                                  
         B     XMOD                                                             
         EJECT                                                                  
**********************************************************************          
* VALIDATE ACTION FIELD                                              *          
**********************************************************************          
         SPACE 1                                                                
VALACT   NTR1  ,                                                                
         LA    R4,SRVACTH                                                       
         ST    R4,CURSOR           DEFAULT CURSOR POSITION HERE                 
         USING FHD,R4                                                           
         CLI   FHDA,C'?'                                                        
         BNE   *+8                                                              
         BRAS  RE,DISHELP                                                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,1,FHIL                                                        
         BZ    VACT06              NO INPUT (DEFAULT ACTION)                    
         BCTR  R1,0                                                             
         LA    RF,ACTTAB           FIND ACTION ROUTINE IN TABLE                 
         USING ACTTABD,RF                                                       
*                                                                               
VACT02   CLI   ACTNAME,0           EOT                                          
         BE    ERR2                                                             
         EX    R1,*+8              COMPARE KEYWORD                              
         BE    VACT04                                                           
         CLC   FHDA(0),ACTNAME                                                  
         AHI   RF,ACTTABL                                                       
         B     VACT02                                                           
*                                                                               
VACT04   MVC   ACTION,ACTNUM       GET ACTION INFO                              
         ICM   R1,15,ACTRTN                                                     
         A     R1,RELO                                                          
         ST    R1,ROUTINE          ADDRESS OF PROCESSING ROUTINE                
         B     VACTX                                                            
*                                                                               
VACT06   MVI   ACTION,0            DEFAULT ACT IS NOTHING                       
         XC    ROUTINE,ROUTINE                                                  
         XR    RF,RF                                                            
*                                                                               
VACTX    MVC   FHDA(L'ACTNAME),SPACES                                           
         OI    FHOI,FHOITR                                                      
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         MVC   FHDA(L'ACTNAME),ACTNAME                                          
         B     EXITOK                                                           
         DROP  R4,RF                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE OPTION FIELD                                              *          
**********************************************************************          
         SPACE 1                                                                
VALOPT   NTR1  ,                                                                
         CLI   ACTION,2            PUTQ REQUESTED                               
         BNE   EXITOK                                                           
*                                                                               
         LA    R4,SRVOPTH                                                       
         USING FHD,R4                                                           
         CLI   FHIL,0                                                           
         BE    ERR1                                                             
         CLI   FHIL,8                                                           
         BH    VOPT06                                                           
*                                                                               
         LA    RF,PUTQQS                                                        
         USING PUTQQSD,RF                                                       
         XR    R1,R1                                                            
         IC    R1,FHIL                                                          
         BCTR  R1,0                                                             
*                                                                               
VOPT02   CLI   PQNAME,0            ASSUME REAL QUEUE NAME INPUT                 
         BE    VOPT06                                                           
         EX    R1,*+8              COMPARE KEYWORD                              
         BE    VOPT04                                                           
         CLC   FHDA(0),PQNAME                                                   
         AHI   RF,PUTQQSL                                                       
         B     VOPT02                                                           
*                                                                               
VOPT04   ICM   RF,15,PQQADDR                                                    
         A     RF,RELO                                                          
         MVC   MQQNAME,0(RF)                                                    
         MVC   FHDA(L'MQQNAME),MQQNAME                                          
         OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
*                                                                               
VOPT06   XR    R1,R1                                                            
         IC    R1,FHIL                                                          
         BCTR  R1,0                                                             
         MVC   MQQNAME,SPACES                                                   
         EX    R1,*+8                                                           
         B     EXITOK                                                           
         MVC   MQQNAME(0),FHDA                                                  
         DROP  R4,RF                                                            
         EJECT                                                                  
**********************************************************************          
* DISPLAY ACTION HELP                                                *          
**********************************************************************          
         SPACE 1                                                                
DISHELP  NTR1  ,                                                                
         LA    R2,SRVSACTH         POINT TO FIRST LINE                          
         USING FHD,R2                                                           
         XR    RF,RF                                                            
HELP02   ICM   RF,1,FHLN                                                        
         BZ    HELP04                                                           
         LR    RE,RF                                                            
         AHI   RE,-(FHDAD+1)                                                    
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         AHI   RE,-(FHDAD)                                                      
         EX    RE,HELPXC                                                        
         BXH   R2,RF,HELP02                                                     
*                                                                               
HELPXC   XC    FHDA(0),FHDA                                                     
         DROP  R2                                                               
*                                                                               
HELP04   LA    R2,SRVSACTH         POINT TO FIRST LINE                          
         USING LINED,R2                                                         
         LA    R3,ACTTAB                                                        
         USING ACTTABD,R3                                                       
*                                                                               
HELP06   CLI   ACTNAME,0                                                        
         BE    XMOD                                                             
         MVC   RLDATA,SPACES                                                    
         MVC   RLDATA+00(L'ACTNAME),ACTNAME                                     
         MVC   RLDATA+15(L'ACTHLP),ACTHLP                                       
         AHI   R2,L'RLINE                                                       
         AHI   R3,ACTTABL                                                       
         B     HELP06                                                           
         EJECT                                                                  
**********************************************************************          
* VALIDATE MESSAGE DATA FIELD                                        *          
**********************************************************************          
         SPACE 1                                                                
VALDATA  NTR1  ,                                                                
         MVC   MSGDATA,SPACES                                                   
         XR    R1,R1                                                            
         ICM   R1,1,SRVDATAH+5                                                  
         BZ    EXITOK              NO INPUT                                     
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MSGDATA(0),SRVDATA                                               
*                                                                               
         XC    SRVDATA,SRVDATA                                                  
         MVC   SRVDATA,MSGDATA                                                  
         OI    SRVDATAH+6,X'80'                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PUT MESSAGE TO FACPAK OUTPUT QUEUE                                  *         
***********************************************************************         
         SPACE 1                                                                
PUTMSG   NTR1  ,                                                                
         LA    RF,L'MSGDATA                                                     
         GOTO1 VMQIO,DMCB,=CL8'PUT',MSGDATA,(RF)                                
*                                                                               
         MVC   SRVDATA,MSGDATA                                                  
         OI    SRVDATAH+6,X'80'                                                 
PMSGX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PUT MESSAGE TO NAMED FACPAK QUEUE                                   *         
***********************************************************************         
         SPACE 1                                                                
PUTMSG1  NTR1  ,                                                                
         LA    RF,L'MSGDATA                                                     
         GOTO1 VMQIO,DMCB,=CL8'PUTQ',MSGDATA,(RF),MQQNAME                       
*                                                                               
         MVC   SRVDATA,MSGDATA                                                  
         OI    SRVDATAH+6,X'80'                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* OPEN SYSTEM DUMMY CALL                                              *         
***********************************************************************         
         SPACE 1                                                                
OPENSYS  NTR1  ,                                                                
         GOTO1 VMQIO,DMCB,=CL8'OPENSYS',0,0,0                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* POST MQIOECB TO ALERT FATASKER                                      *         
***********************************************************************         
         SPACE 1                                                                
POST     NTR1  ,                                                                
         L     R2,VSSB             DETACH ANY ATTACHED SUB-TASKS                
         USING SSBD,R2                                                          
         OC    SSBMQION,SSBMQION                                                
         BZ    EXITOK                                                           
*                                                                               
         L     R3,SSBAATC                                                       
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING ATCD,R3             R5=A(ATC)                                    
POST010  CLI   ATCTYPE,ATCTMQIO    TEST FAMQIO                                  
         BNE   POST030                                                          
         TM    ATCSTAT1,ATCSATCH   ATTACHED?                                    
         BZ    POST030                                                          
*                                                                               
         ICM   R1,15,ATCARECB      POST RETURN ECB (MQIOECB, TASKER)            
         POST  (1)                                                              
*                                                                               
POST030  BXLE  R3,R4,POST010                                                    
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP INPUT TRANSACTION MONITOR                                    *         
***********************************************************************         
         SPACE 1                                                                
SETINP   NTR1  ,                                                                
         GOTO1 VMQIO,DMCB,=CL8'RESET'                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RESET FAMQIO                                                        *         
***********************************************************************         
         SPACE 1                                                                
RESET    NTR1  ,                                                                
         L     R2,VSSB             DETACH ANY ATTACHED SUB-TASKS                
         USING SSBD,R2                                                          
         OC    SSBMQION,SSBMQION                                                
         BZ    EXITOK                                                           
*                                                                               
         L     R3,SSBAATC                                                       
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING ATCD,R3             R5=A(ATC)                                    
RSET010  CLI   ATCTYPE,ATCTMQIO    TEST FAMQIO                                  
         BNE   RSET030                                                          
         TM    ATCSTAT1,ATCSATCH                                                
         BZ    RSET020                                                          
         MVI   ATCSTAT1,ATCSRSET                                                
         LA    R2,ATCOSTCB                                                      
         DETACH (2),STAE=NO                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  PROCESS FAMQIO ENDED                         
         GOTO1 VMQIO,DMCB,=CL8'END'                                             
         B     RSET030                                                          
*                                  PROCESS FAMQIO ENDED                         
RSET020  MVI   ATCSTAT1,ATCSRSET                                                
         LHI   R2,999                                                           
         LA    R1,ATCOSECB                                                      
         POST  (1),(2)             POST OS ECB TO RE-ATTACH FAMQIO              
*                                                                               
         GOTO1 VMQIO,DMCB,=CL8'END'                                             
*                                                                               
RSET030  BXLE  R3,R4,RSET010                                                    
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT ON FAMQIO STATUS                                             *         
***********************************************************************         
         SPACE 1                                                                
STATUS   NTR1  ,                                                                
         LA    R2,SRVSACTH         POINT TO FIRST LINE                          
         USING FHD,R2                                                           
         XR    RF,RF                                                            
STAT02   ICM   RF,1,FHLN                                                        
         BZ    STAT04                                                           
         LR    RE,RF                                                            
         AHI   RE,-(FHDAD+1)                                                    
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         AHI   RE,-(FHDAD)                                                      
         EX    RE,STATXC                                                        
         BXH   R2,RF,STAT02                                                     
*                                                                               
STATXC   XC    FHDA(0),FHDA                                                     
         DROP  R2                                                               
*                                                                               
STAT04   LA    R2,SRVSACTH         POINT TO FIRST LINE                          
         USING LINED,R2                                                         
*                                                                               
         MVC   RLDATA,SPACES                                                    
         MVC   RLDATA+00(4),=CL4'MQIO'                                          
         MVC   RLDATA+30(15),=CL10'Enabled'                                     
*                                                                               
         L     R3,VSSB                                                          
         USING SSBD,R3                                                          
         ICM   R0,15,SSBMQION                                                   
         BZ    STAT16                                                           
         MVC   RLDATA+30(15),=CL15'Enabled'                                     
         CHI   R0,2                                                             
         BNE   *+10                                                             
         MVC   RLDATA+30(25),=CL25'Enabled - Debug Mode'                        
*                                                                               
         AHI   R2,L'RLINE          NEXT LINE                                    
*                                                                               
         L     R4,SSBAATC                                                       
         LH    RE,0(R4)                                                         
         L     RF,2(R4)                                                         
         LA    R4,6(R4)                                                         
         USING ATCD,R4             R4=A(ATC)                                    
         CLI   ATCTYPE,ATCTMQIO    TEST FAMQIO                                  
         BE    *+12                                                             
         BXLE  R4,RE,*-8                                                        
         B     STAT16                                                           
*                                                                               
         MVC   RLDATA,SPACES                                                    
         MVC   RLDATA(25),=CL25'FAMQIO Subtask Status: '                        
         MVC   RLDATA+30(20),=CL20'Not Attached'                                
         TM    ATCSTAT1,ATCSATCH                                                
         BZ    *+14                                                             
         MVC   RLDATA+30(20),=CL20'Attached'                                    
         B     STAT08                                                           
         TM    ATCSTAT1,ATCSERRS                                                
         BZ    *+14                                                             
         MVC   RLDATA+30(20),=CL20'Error'                                       
         B     STAT08                                                           
         TM    ATCSTAT1,ATCSBUSY                                                
         BZ    STAT08                                                           
         MVC   RLDATA+30(20),=CL20'Busy'                                        
*                                                                               
STAT08   AHI   R2,L'RLINE          NEXT LINE                                    
         MVC   RLDATA,SPACES                                                    
*                                                                               
         MVC   RLDATA(25),=CL25'MQ Connect Status: '                            
         CLI   ATCSTATI,ATCCSNOQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(25),=CL25'Not Connected'                               
         B     STAT10                                                           
         CLI   ATCSTATI,ATCCSEXQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(25),=CL25'MQ Exception Condition'                      
         B     STAT10                                                           
         CLI   ATCSTATI,ATCCSCOQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(25),=CL25'Connected to Qmgr'                           
         B     STAT10                                                           
         CLI   ATCSTATI,ATCCSOPQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(25),=CL25'Opened Queues'                               
         B     STAT10                                                           
         CLI   ATCSTATI,ATCCSCLQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+25(40),=CL25'Closed Queues'                               
         B     STAT10                                                           
         CLI   ATCSTATI,ATCCSDIQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(25),=CL25'Disconnected from Qmgr'                      
         B     STAT10                                                           
         MVC   RLDATA+30(25),=CL25'MQ Error Status'                             
         B     STAT10                                                           
*                                                                               
STAT10   AHI   R2,L'RLINE          NEXT LINE                                    
         MVC   RLDATA,SPACES                                                    
*                                                                               
         MVC   RLDATA(25),=CL25'MQ Error Status: '                              
         CLI   ATCERRCD,ATCESNOQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(20),=CL20'No Error'                                    
         B     STAT14                                                           
         CLI   ATCERRCD,ATCESCOQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(20),=CL20'Connect to Qmgr'                             
         B     STAT12                                                           
         CLI   ATCERRCD,ATCESOPQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(20),=CL20'Open Queue'                                  
         B     STAT12                                                           
         CLI   ATCERRCD,ATCESGEQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(20),=CL20'Get Message'                                 
         B     STAT12                                                           
         CLI   ATCERRCD,ATCESPUQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(20),=CL20'Put Message'                                 
         B     STAT12                                                           
         CLI   ATCERRCD,ATCESCLQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(20),=CL20'Close Queue'                                 
         B     STAT12                                                           
         CLI   ATCERRCD,ATCESDIQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(30),=CL30'Disconnect from Qmgr'                        
         B     STAT12                                                           
         CLI   ATCERRCD,ATCESSEQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(20),=CL20'Ser get Message'                             
         B     STAT12                                                           
         CLI   ATCERRCD,ATCESRPQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(20),=CL20'Reply Put Message'                           
         B     STAT12                                                           
         CLI   ATCERRCD,ATCESCMQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(20),=CL20'Commit'                                      
         B     STAT12                                                           
         CLI   ATCERRCD,ATCESIQQ                                                
         BNE   *+14                                                             
         MVC   RLDATA+30(20),=CL20'Qmgr Inquiry'                                
         B     STAT12                                                           
         MVC   RLDATA+30(20),=CL20'Undefined Error'                             
         B     STAT14                                                           
*                                                                               
STAT12   AHI   R2,L'RLINE          NEXT LINE                                    
         MVC   RLDATA,SPACES                                                    
*                                                                               
         MVC   RLDATA(30),=CL30'Last MQ Routine Called: '                       
         MVC   RLDATA+30(3),ATCERRCD+1                                          
*                                                                               
STAT14   AHI   R2,L'RLINE          NEXT LINE                                    
         MVC   RLDATA,SPACES                                                    
         MVC   RLDATA(25),=CL25'MQ Completion Code: '                           
         GOTO1 VHEXOUT,DMCB,ATCMQCC,RLDATA+30,4                                 
*                                                                               
         AHI   R2,L'RLINE          NEXT LINE                                    
         MVC   RLDATA,SPACES                                                    
         MVC   RLDATA(20),=CL20'MQ Reason Code: '                               
         GOTO1 VHEXOUT,DMCB,ATCMQRC,RLDATA+30,4                                 
         AHI   R2,L'RLINE          NEXT LINE                                    
         DROP  R4                                                               
*                                                                               
STAT16   AHI   R2,L'RLINE                                                       
         MVC   RLDATA,SPACES                                                    
         MVC   RLDATA+00(15),=CL15'Queue Manager'                               
         MVC   RLDATA+20(06),=CL06'MQ1P'                                        
         ICM   RF,15,SSBAMQM                                                    
         BZ    *+14                                                             
         MVC   RLDATA+20(48),0(RF)                                              
         AHI   R2,L'RLINE                                                       
*                                                                               
         MVC   RLDATA,SPACES                                                    
         MVC   RLDATA+00(15),=CL15'Input Queue'                                 
         ICM   RF,15,SSBAMQIN                                                   
         BZ    *+14                                                             
         MVC   RLDATA+20(48),0(RF)                                              
         AHI   R2,L'RLINE                                                       
*                                                                               
         MVC   RLDATA,SPACES                                                    
         MVC   RLDATA+00(15),=CL15'Output Queue'                                
         ICM   RF,15,SSBAMQOU                                                   
         BZ    *+14                                                             
         MVC   RLDATA+20(48),0(RF)                                              
         AHI   R2,L'RLINE                                                       
*                                                                               
         MVC   RLDATA,SPACES                                                    
         MVC   RLDATA+00(15),=CL15'Work Queue'                                  
         ICM   RF,15,SSBAMQWK                                                   
         BZ    *+14                                                             
         MVC   RLDATA+20(48),0(RF)                                              
         AHI   R2,L'RLINE          NEXT LINE                                    
*                                                                               
         MVC   RLDATA,SPACES                                                    
         MVC   RLDATA+00(15),=CL15'Control Queue'                               
         ICM   RF,15,SSBAMQCT                                                   
         BZ    *+14                                                             
         MVC   RLDATA+20(48),0(RF)                                              
         AHI   R2,L'RLINE          NEXT LINE                                    
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
*************************************************************                   
*     DISPAY MESSAGES ON FACPAK INPUT QUEUE                 *                   
*************************************************************                   
         SPACE 1                                                                
DISPMSG  NTR1                                                                   
         USING LINED,R3                                                         
         LA    R2,LINENUM          # DISPLAY LISTING LINES                      
         LA    R3,SRVSACTH         POINT TO FIRST LINE                          
*                                                                               
DREP10   EQU   *                                                                
*                                                                               
         MVC   MSGDATA,SPACES                                                   
*                                                                               
DREP20   EQU   *                                                                
         MVC   RLDATA(L'MSGDATA),MSGDATA                                        
         BCT   R2,DREP40           TEST LAST LINE DONE                          
         B     DREPX               IF SO END DISPLAY                            
*                                                                               
DREP40   LA    R3,L'RLINE(R3)      NEXT LINE                                    
         B     DREP10                                                           
*                                                                               
DREP30   EQU   *                                                                
         TWAXC 0(R3),PROT=Y        CLEAR REST OF SCREEN                         
         B     DREP20              CONTINUE TO CLEAR LINE SAVE TABLE            
*                                                                               
DREPX    B     XMOD                RETURN TO MAIN CONTROL EXIT                  
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* INITIALISATION                                                     *          
**********************************************************************          
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         USING SRPARMD,R1                                                       
         L     RF,SRQATIA                                                       
         ST    RF,ATIA                                                          
         L     RF,SRQATIOB                                                      
         ST    RF,ATIOB                                                         
*                                                                               
         MVC   TRMUSER,TUSER       SAVE UTL INFO                                
         MVC   TRM,TNUM                                                         
         MVC   TRMTYP,TSTAT                                                     
         MVC   TRMTYP1,TTYPE                                                    
*                                                                               
         MVI   DDSFLAG,X'00'       INITIALISE TERMINAL FLAG                     
         TM    TSTAT1,TSTATDDS                                                  
         BZ    *+8                                                              
         OI    DDSFLAG,DDSTRM                                                   
*                                                                               
         MVC   DATAMGR,VDATAMGR                                                 
         L     RE,SRQACOMF         SAVE COMFACS ENTRYS                          
         ST    RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGLOBBER,CGLOBBER                                                
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VDICTATE,CDICTATE                                                
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VSCANNER,CSCANNER                                                
         MVC   VDATCON,CDATCON                                                  
         MVC   VMQIO,CMQIO                                                      
         DROP  RE,R1                                                            
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9A00A0D'                                       
         L     R1,0(R1)            GET A(SQUASHER)                              
         ST    R1,VSQUASH                                                       
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LU  ',DDDCLST,DDDSLST                            
*                                                                               
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BNO   *+12                                                             
         OI    DDSFLAG,DDSNEW      FORCE RELOAD                                 
         OI    TSVCREQ,X'02'       SET CURSOR FLAG                              
*                                                                               
         MVC   USERID,TUSER        SET USER ID TO LOGON VALUE                   
         MVC   LOGONID,TUSER       AND SAVE LOGON VALUE                         
         TM    DDSFLAG,DDSPUB                                                   
         BZ    *+10                                                             
         MVC   USERID,PUBLICID     SET USER ID TO PUBLIC VALUE                  
         OC    USERID,USERID       NON DDS TERMINALS MUST BE LOGGED ON          
         BNZ   *+12                                                             
         TM    DDSFLAG,DDSTRM                                                   
         BZ    ERR0                                                             
*                                                                               
         SR    R0,R0               GET DATE AND TIME                            
         SR    R1,R1                                                            
         TIME  BIN                 R0=DATE,R1=TIME                              
         STM   R0,R1,MVSTIME                                                    
         OI    MVSDATE+3,X'0F'                                                  
*                                                                               
         L     R2,VSSB                                                          
         USING SSBD,R2                                                          
         ICM   RF,15,SSBAMQIN                                                   
         BZ    *+10                                                             
         MVC   MQQIN,0(RF)                                                      
         ICM   RF,15,SSBAMQOU                                                   
         BZ    *+10                                                             
         MVC   MQQOU,0(RF)                                                      
         ICM   RF,15,SSBAMQWK                                                   
         BZ    *+10                                                             
         MVC   MQQWK,0(RF)                                                      
         ICM   RF,15,SSBAMQCT                                                   
         BZ    *+10                                                             
         MVC   MQQCT,0(RF)                                                      
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* ERROR MESSAGES                                                     *          
**********************************************************************          
         SPACE 1                                                                
ERR0     LA    R0,SREMBC           MUST BE CONNECTED                            
         B     ERRX                                                             
ERR1     LA    R0,SREMIF           MISSING INPUT FIELD                          
         B     ERRX                                                             
ERR2     LA    R0,SREIIF           INVALID INPUT FIELD                          
         B     ERRX                                                             
ERR10    LA    R0,57               INVALID SUB ACTION                           
         B     ERRX                                                             
*                                                                               
ERRX     L     R1,CURSOR                                                        
         OI    6(R1),X'40'         CURSOR POS                                   
         GOTO1 VGETTXT,DMCB,(R0),0,(C'E',0),0,0,0                               
         L     RD,SAVERD           EXIT MODULE                                  
         XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* USEFUL ROUTINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XMOD     L     R1,CURSOR                                                        
         OI    6(R1),X'40'         SET CURSOR                                   
         L     RD,SAVERD                                                        
         XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* LITERALS AND CONSTANTS                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
*                                                                               
*  MQ API CONSTANTS                                                             
*                                                                               
         CMQA LIST=NO                                                           
*                                                                               
OBJDESC    CMQODA  DSECT=NO,LIST=YES        OBJECT DESCRIPTOR                   
MSGDESC    CMQMDA  DSECT=NO,LIST=YES        MESSAGE DESCRIPTOR                  
PUTMSGOPTS CMQPMOA DSECT=NO,LIST=YES        PUT MESSAGE OPTIONS                 
GETMSGOPTS CMQGMOA DSECT=NO,LIST=YES        GET MESSAGE OPTIONS                 
*                                                                               
* SCREEN DISPLAY GLOBAL DATA LISTING LINE COUNT                                 
*                                                                               
LINENUM  EQU   (SRVXXXH-SRVSACTH)/(SRVLIN2-SRVLIN1)                             
         EJECT                                                                  
**********************************************************************          
*        ACTION TABLE                                                           
**********************************************************************          
         SPACE 1                                                                
         DS    0D                                                               
ACTTAB   DC    CL8'PUT     ',AL1(1),X'000000',AL4(PUTMSG)                       
         DC    CL32'Put message to output queue     '                           
         DC    CL8'PUTQ    ',AL1(2),X'000000',AL4(PUTMSG1)                      
         DC    CL32'Put to named output queue       '                           
         DC    CL8'DISPLAY ',AL1(3),X'000000',AL4(DISPMSG)                      
         DC    CL32'Display messages on input queue '                           
         DC    CL8'SET     ',AL1(4),X'000000',AL4(SETINP)                       
         DC    CL32'Activate MQIO if required       '                           
         DC    CL8'RESET   ',AL1(5),X'000000',AL4(RESET)                        
         DC    CL32'Reset FAMQIO status             '                           
         DC    CL8'STATUS  ',AL1(6),X'000000',AL4(STATUS)                       
         DC    CL32'Display MQ status               '                           
         DC    CL8'OPENSYS ',AL1(7),X'000000',AL4(OPENSYS)                      
         DC    CL32'Send dummy "OPENSYS" to $MQP    '                           
         DC    CL8'POST    ',AL1(8),X'000000',AL4(POST)                         
         DC    CL32'Post MQIOECB to alert FATASKER  '                           
         DC    X'0000'                                                          
*                                                                               
ACTTABD  DSECT                                                                  
ACTNAME  DS    CL8                                                              
ACTNUM   DS    X                                                                
ACTFLAG  DS    X                                                                
         DS    XL2                                                              
ACTRTN   DS    AL4                                                              
ACTHLP   DS    CL32                                                             
ACTTABL  EQU   *-ACTTABD                                                        
*                                                                               
MQT      CSECT                                                                  
**********************************************************************          
         EJECT                                                                  
*        SUB-ACTION TABLE                                                       
*        LA    RF,S(TEXT),SUBNUM,MIN LEN-1                                      
*                                                                               
SUBTAB   DC    X'41F0',S(SR@UPD),AL1(1),X'00'                                   
         DC    X'41F0',S(SR@PURGE),AL1(2),X'00'                                 
         DC    X'0000'                                                          
*                                                                               
**********************************************************************          
* PUTQ QUEUE SHORTCUTS                                               *          
**********************************************************************          
         SPACE 1                                                                
         DS    0F                                                               
PUTQQS   DC    CL8'OUTPUT  ',AL4(MQQOU)                                         
         DC    CL8'INPUT   ',AL4(MQQIN)                                         
         DC    CL8'CONTROL ',AL4(MQQCT)                                         
         DC    CL8'WORK    ',AL4(MQQWK)                                         
         DC    CL8'BROKER  ',AL4(MQQBK)                                         
         DC    CL8'TBROKER ',AL4(MQQBKT)                                        
         DC    X'00'                                                            
*                                                                               
PUTQQSD  DSECT                                                                  
PQNAME   DS    CL8                                                              
PQQADDR  DS    AL4                 IF ZERO USE NAME BELOW                       
PUTQQSL  EQU   *-PUTQQSD                                                        
*                                                                               
MQT      CSECT                                                                  
*                                                                               
MQQIN    DC    CL48' '                                                          
MQQOU    DC    CL48' '                                                          
MQQWK    DC    CL48' '                                                          
MQQCT    DC    CL48' '                                                          
MQQBK    DC    CL48'DDS.BROKER.LOCALQ'                                          
MQQBKT   DC    CL48'DDS.BROKER.TEST.LOCALQ'                                     
*                                                                               
DDDCLST  DS    0C                                                               
*                                                                               
         DCDDL SR#UPD,9,L                                                       
         DCDDL SR#DSP,9,L                                                       
         DCDDL SR#RLEAS,9,L                                                     
         DCDDL SR#UNKNW,9,L                                                     
         DCDDL SR#PURGE,9,L                                                     
         DCDDL SR#LAST,9,L                                                      
         DCDDL SR#NEXT,9,L                                                      
         DCDDL SR#CLEAR,9,L                                                     
*                                                                               
DC@CODE  DC    CL8'CODE'                                                        
DC@DDS   DC    CL8'DDS'                                                         
DC@USR   DC    CL8'U'                                                           
DC@SWIT  DC    CL8'SWITCH'                                                      
*                                                                               
SPACES   DC    CL132' '                                                         
FAMQIO   DC    CL8'FAMQIO  '                                                    
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
SAVERE   DS    F                                                                
SAVERD   DS    F                                                                
DMCB     DS    6F                                                               
ACTION   DS    X                   ACTION CODE                                  
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
MVSTIME  DS    F                   IBM TIME BINARY 100THS SECS.                 
MVSDATE  DS    F                   IBM DATE JULIAN                              
RELO     DS    A                   RELOCATION FACTOR                            
ROUTINE  DS    A                   ADDRESS ACTION ROUTINE                       
ATIA     DS    A                   ATIA                                         
APARMS   DS    A                   ADDRESS INPUT PARAMETERS                     
ATIOB    DS    A                   ATIOB                                        
ATSKGLOB DS    A                   ADDRESS                                      
*                                                                               
VGETFACT DS    A                   ADDRESSES OF EXTERNAL ROUTINES               
VGLOBBER DS    A                                                                
VHEXOUT  DS    A                                                                
VDICTATE DS    A                                                                
VGETTXT  DS    A                                                                
VSCANNER DS    A                                                                
VSQUASH  DS    A                                                                
VDATCON  DS    A                                                                
DATAMGR  DS    A                                                                
VMQIO    DS    A                                                                
ACOMFACS DS    A                                                                
*                                                                               
CURSOR   DS    A                   ADDRESS TWA FIELD FOR CURSOR                 
*                                                                               
DDSFLAG  DS    X                   TERMINAL STATUS FLAG                         
DDSTRM   EQU   X'20'                                                            
DDSACT   EQU   X'20'                                                            
DDSPUB   EQU   X'08'                                                            
DDSNEW   EQU   X'04'                                                            
*                                                                               
QHDR     DS    A                   HELP ROUTINE WORK AREAS                      
FLAG     DS    C                                                                
*                                                                               
PUBLICID DS    AL2                                                              
         DS    AL2                                                              
LOGONID  DS    XL2                                                              
USERID   DS    CL8                                                              
TRMTYP   DS    XL2                 UTL INFO SAVE                                
TRMTYP1  DS    X                                                                
TRMUSER  DS    XL2                                                              
TRM      DS    XL2                                                              
*                                                                               
WORK     DS    CL32                                                             
MQQNAME  DS    CL48                                                             
MSG      DS    CL80                                                             
EXT      DS    CL132               EXTRA TEXT FOR GETTXT CALLS                  
SAVE     DS    360C                                                             
*                                                                               
SCANBLK  DS    256C                                                             
MSGDATA  DS    CL(L'SRVDATA)                                                    
*                                                                               
*                                  $GLOBAL SAVE DATA AREA                       
*                                                                               
SAVEDSTR DS    0F                  SAVED STORAGE VARIABLES (COPY)               
IDENT    DS    CL4                 $GLO INDENTIFIER                             
SVELCODE DS    X                   FIRST ELEMENT CODE SAVE                      
SWITCH1  DS    X                   OPTION SWITCH SAVE                           
SWINAS   EQU   X'02'               NOT ASSIGNED ELEMENT DISPLAY SWITCH          
SWINUL   EQU   X'04'               EMPTY ELEMNT DISPLAY SWITCH                  
*                                                                               
DDDSLST  DS    0C                                                               
         DSDDL                                                                  
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
         SPACE 1                                                                
LINED    DSECT                     DISPLAY GLOBAL REPORT LINE DSECT             
RLINE    DS    0CL(SRVLIN2-SRVLIN1)                                             
RLSAHDR  DS    CL8                                                              
RLSACT   DS    CL3                 SUB-ACTION                                   
RLLNHDR  DS    CL8                                                              
RLDATA   DS    CL74                MESSAGE DATA                                 
         SPACE 1                                                                
*FADSECTS                                                                       
SRMQTFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRMQTFFD                                                       
         EJECT                                                                  
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* SRDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SRDDEQUS                                                       
         PRINT ON                                                               
* SRERREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE SRERREQUS                                                      
         PRINT ON                                                               
* DDMQIOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMQIOD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SRMQT00   02/12/08'                                      
         END                                                                    
