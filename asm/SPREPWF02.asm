*          DATA SET SPREPWF02  AT LEVEL 003 AS OF 06/03/03                      
*PHASE SPWF02A                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRTREC                                                                 
         PRINT NOGEN                                                            
         TITLE 'SPWF02 - SPOT WRITER FORMAT UPDATE'                             
         SPACE 1                                                                
SPWF02   CSECT                                                                  
         NMOD1 0,SPWF02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    WF0                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
WF0      OPEN  (PROGFILE,INPUT)    OPEN FILE W/PROGRAM RECS                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WF2      LA    R6,MYRECLEN                                                      
         GET   PROGFILE,(R6)       GET A PROGRAM FILE RECORD                    
*                                                                               
         LA    R6,MYREC                                                         
         LA    R4,=CL40'PROGRAM REC IN EJOR.TOUT'                               
         BAS   RE,PRNTREC                                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMRDHI'),=C'CTFILE',MYREC,PROGREC         
         CLC   MYREC(CTLEN-CTLKID),PROGREC                                      
         BNE   WF06                REC NOT FOUND...ADD IT                       
*                                                                               
         LA    R6,PROGREC          REC IN CTFILE                                
         LA    R4,=CL40'PROGRAM REC IN TEST CONTROL FILE'                       
         BAS   RE,PRNTREC                                                       
         USING CTLKEYD,R6                                                       
         SR    R7,R7                                                            
         ICM   R7,3,CTLEN          CTFILE REC LEN                               
         DROP  R6                                                               
         USING CTLKEYD,R2                                                       
         LA    R2,MYREC            REC IN EJOR.TOUT                             
         SR    R3,R3                                                            
         ICM   R3,3,CTLEN          EJOR.TOUT REC LEN                            
         DROP  R2                                                               
         MVC   P(28),=C'PROGRAM RECORDS ARE THE SAME'                           
         CLCL  R2,R6                                                            
         BE    WF08                PROG RECS THE SAME, SKIP I/O                 
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   WF05                NO, SKIP I/O                                 
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMWRT'),=C'CTFILE',MYREC,MYREC            
*                                                                               
WF05     MVC   P+20(9),=C'DIFFERENT'                                            
         B     WF08                                                             
*                                                                               
WF06     CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   WF07                NO, SKIP I/O                                 
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMADD'),=C'CTFILE',MYREC,MYREC            
*                                                                               
WF07     MVC   P(28),=CL28'PROGRAM REC WAS ADDED'                               
*                                                                               
WF08     CLI   QOPT1,C'Y'          WANT TO PRINT WHAT WE ARE DOING?             
         BNE   WF2                 NO                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     WF2                                                              
*                                                                               
WF10     CLOSE PROGFILE            CLOSE PROGRAM FILE                           
*                                                                               
WF100    GOTO1 AENDREQ                                                          
         EJECT                                                                  
***********************************************************************         
* PRINT THE RECORD INFO                                               *         
***********************************************************************         
PRNTREC  NTR1                                                                   
         CLI   QOPT1,C'Y'              WANT TO PRINT WHAT WE ARE DOING?         
         BNE   PRNTX                   NO                                       
*                                                                               
         MVC   P(20),=CL20'RECORD LENGTH = '                                    
         USING CTLKEYD,R6                                                       
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,CTLEN                                                       
         EDIT  (R5),(4,P+20)                                                    
         MVC   P+30(3),=C'(X"'                                                  
         GOTO1 HEXOUT,DMCB,CTLEN,P+33,2,=C'TOG'                                 
         MVC   P+37(2),=C'")'                                                   
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,(40,(R4)),(R6),C'DUMP',(R5),=C'2D'               
         GOTO1 REPORT                                                           
PRNTX    B     EXIT                                                             
         DROP  R6                                                               
               EJECT                                                            
         GETEL R6,24,ELCODE                                                     
*                                                                               
PROGFILE DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=WF10                                              
         EJECT                                                                  
* CONSTANTS                                                                     
ELCODE   DS    CL1                                                              
PROGREC  DS    CL1000                                                           
*                                                                               
         LTORG                                                                  
MYRECLEN DS    F                                                                
MYREC    DS    1000C                                                            
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
CTLKEYD  DSECT                                                                  
CTLKID   DS    XL1                                                              
CTLKIDQ  EQU   X'01'                                                            
         DS    CL11                                                             
CTLAGID  DS    CL2                                                              
CTLSYS   DS    XL1                                                              
CTLPRG   DS    XL1                                                              
CTLPHAS  DS    XL1                                                              
CTLNAME  DS    CL8                                                              
CTLEN    DS    XL2    <--- CHANGED NAME...WAS DEFINED IN LIBRARY RECS           
CTLSTATS DS    XL1    <--- CHANGED NAME...WAS DEFINED IN LIBRARY RECS           
CTLDATA2 DS    0X     <--- CHANGED NAME...WAS DEFINED IN LIBRARY RECS           
*                                                                               
CTLDESCD DSECT                                                                  
CTLDCOD  DS    XL1                                                              
CTLDCODQ EQU   X'01'                                                            
CTLDLEN  DS    XL1                                                              
CTLDDATA DS    0X                                                               
*                                                                               
CTLFIELD DSECT                                                                  
CTLFCOD  DS    XL1                                                              
CTLFCODQ EQU   X'02'                                                            
CTLFLEN  DS    XL1                                                              
CTLFID   DS    XL1                                                              
CTLFSEQ  DS    XL1                                                              
CTLFDATA DS    0X                                                               
*                                                                               
CTLFILTD DSECT                                                                  
CTLLCOD  DS    XL1                                                              
CTLLCODQ EQU   X'10'                                                            
CTLLLEN  DS    XL1                                                              
CTLLFILT DS    CL4                                                              
CTLLLENQ EQU   *-CTLFILTD                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPWF02 06/03/03'                                      
         END                                                                    
