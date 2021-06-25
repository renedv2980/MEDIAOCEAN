*          DATA SET DERENDRIV  AT LEVEL 002 AS OF 08/03/12                      
*PHASE DERENDRA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'RENTRAK DRIVER/PREPROCESSOR'                                    
***********************************************************************         
*                                                                               
* THIS IS THE RENTRAK PRE-PROCESSOR DRIVER MODULE.                              
*                                                                               
***********************************************************************         
RENDRIVE CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,RENDRIVE,=V(REGSAVE),R9                                        
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'DRVTRACE'  DDNAME=DRVTRACE              
*                                                                               
         BAS   RE,READCRDS         READ PARAMETER CARDS                         
         BE    *+6                 ALL CARDS ARE VALID?                         
         DC    H'0'                NO: INVALID PARAMETER CARD                   
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'A',=C'FILEIN  ')                            
         CLI   DMCB+4,0            IS FILEIN ALLOCATED VIA DD STMT?             
         BE    DYNALINP            NO                                           
         MVI   HAVEDATA,C'Y'       YES: WE HAVE SOME DATA TO READ               
         MVI   DYNALFLG,C'N'       NO NEED FOR DYNAMIC ALLOCATION               
         B     PROCESS                                                          
*                                                                               
DYNALINP DS    0H                  THERE'S NO DD STATEMENT FOR FILEIN           
         MVI   HAVEDATA,C'N'       ASSUME MOUNTED FOLDER IS EMPTY               
         OPEN  NFSFILES            CONTAINS FILENAMES IN MOUNTED FOLDER         
*                                                                               
FILELOOP DS    0H                                                               
         GET   NFSFILES,RDW        GET THE FIRST FILENAME                       
         SR    R2,R2                                                            
         ICM   R2,3,RDW            RECLEN (FROM RDW)                            
         SHI   R2,4                L'RDW                                        
*                                                                               
         MVC   P(17),=C'PROCESSING FILE: '                                      
         LR    RF,R2                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P+17(0),RECORD      PRINT PATHNAME IN TRACE                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'H',=C'FILEIN  '),((R2),RECORD),    +        
               (X'50',DUMMYDCB)                                                 
         TM    DMCB+8,X'20'        DYNAMIC ALLOCATION FAILED?                   
         BZ    *+6                                                              
         DC    H'0'                YES                                          
         MVI   HAVEDATA,C'Y'       WE HAVE AT LEAST ONE FILE TO CONVERT         
*                                                                               
PROCESS  DS    0H                                                               
         CLI   HAVEDATA,C'Y'       IS THERE ANY DATA TO CONVERT?                
         BNE   CHKWARNS            NO                                           
*                                                                               
         BAS   RE,CALLICE          PROCESS ONE RENTRAK FILE AT A TIME           
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'U',=C'FILEIN  ')                            
         OC    DMCB+4(4),DMCB+4    SUCCESSFUL DYNAMIC UNALLOCATION?             
         BZ    *+6                                                              
         DC    H'0'                NO                                           
*                                                                               
         CLI   DYNALFLG,C'Y'       DO WE NEED TO CHECK FOR MORE FILES?          
         BE    FILELOOP            YES                                          
*                                                                               
DONE     DS    0H                                                               
         CLI   DYNALFLG,C'Y'       WAS NFSFILES EVER OPENED?                    
         BNE   CHKWARNS            NO                                           
         CLOSE NFSFILES                                                         
*                                                                               
CHKWARNS DS    0H                                                               
         MVC   P(20),=C'PROCESSING COMPLETED'                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
*                                                                               
READCRDS NTR1                                                                   
*                                                                               
READCARD DS    0H                                                               
         MVC   TITLE(27),=C'RENTRAK PREPROCESSOR DRIVER'                        
         MVC   P(16),=C'PARAMETER CARDS:'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
NEXTCARD DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    RDCARD90            ALL PARAMETER CARDS VALID                    
*                                                                               
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    NEXTCARD            YES                                          
*                                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   *+18                                                             
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6      DDSIO= OVERRIDE                              
         B     NEXTCARD                                                         
*                                                                               
RDCARD90 DS    0H                                                               
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     YES                 ALL CARDS ARE VALID                          
*                                                                               
         ANSR                                                                   
         EJECT                                                                  
*                                                                               
* CALL ICETOOL TO PREPROCESS/SORT THE RENTRAK INPUT FILE.                       
*                                                                               
CALLICE  NTR1                                                                   
*                                                                               
         MVC   P(42),=C'ABOUT TO CALL ICETOOL VIA TOOLIN INTERFACE'             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SR    R1,R1               USE TOOLIN INTERFACE                         
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               WERE ALL OPERATORS SUCCESSFUL?               
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(30),=C'SUCCESSFUL RETURN FROM ICETOOL'                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CALLICEX DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
NFSFILES DCB   DDNAME=NFSFILES,DSORG=PS,MACRF=GM,LRECL=256,RECFM=VB,   +        
               EODAD=DONE                                                       
*                                                                               
* THE DUMMYDCB IS USED AS TO HOLD DCB PARAMETERS FOR DDDYNALLOC.                
* THIS DCB ITSELF IS NEVER OPENED.                                              
DUMMYDCB DCB   DSORG=PS,RECFM=VB,MACRF=GM,LRECL=304,BLKSIZE=308                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
CARD     DS    CL80                                                             
*                                                                               
HAVEDATA DS    C                                                                
DYNALFLG DC    C'Y'                ASSUME DATA WILL BE READ VIA NFS             
*                                                                               
RDW      DS    F                                                                
RECORD   DS    CL256                                                            
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         SPACE 2                                                                
         IEFZB4D0                                                               
         IEFZB4D2                                                               
         SPACE 2                                                                
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DERENDRIV 08/03/12'                                      
         END                                                                    
