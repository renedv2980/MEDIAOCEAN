*          DATA SET DMFAST     AT LEVEL 002 AS OF 12/01/10                      
*PHASE DMFASTA                                                                  
*INCLUDE DMDMGRL                                                                
         TITLE 'DMFAST - SAMPLE CODING FOR FAST INTERFACES'                     
         PRINT NOGEN                                                            
DMFAST   CSECT                                                                  
         NBASE 0,DMFAST,WORK=A(WORK)                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
                                                                                
         L     RF,=A(UTL)          OPEN SYSTEM XXX                              
         MVC   4(1,RF),XXXSEN                                                   
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',XXXSYS,XXXLST,A(DAREC)               
                                                                                
INITIS   GOTO1 =V(DATAMGR),DMCB,=C'DMFAST',XXXDIR                               
         MVC   VISDDS,DMCB3                                                     
         MVC   VISDTF,DMCB4                                                     
         XC    ISPL,ISPL           BUILD ISDDS PARAM LIST                       
         LA    RF,ISREC                                                         
         ST    RF,ISPL2                                                         
         MVC   ISPL4,VISDTF                                                     
         LA    RF,ISKEY                                                         
         ST    RF,ISPL5                                                         
                                                                                
INITDA   GOTO1 =V(DATAMGR),DMCB,=C'DMFAST',XXXFIL                               
         MVC   VGETREC,DMCB3                                                    
         MVC   VDADTF,DMCB4                                                     
         XC    GRPL,GRPL           BUILD GETREC PARAM LIST FOR DMDALINK         
         L     RF,=A(DAREC)                                                     
         ST    RF,GRPL2                                                         
         MVC   GRPL4,VDADTF                                                     
         LA    R0,ISREC            BUILD DMCB FOR GETREC DATAMGR CALL           
         AH    R0,XXXDDA                                                        
         GOTO1 ,GRDMCB,=C'GETREC',XXXFIL,(R0),A(DAREC),DAWRK                    
                                                                                
ISRDHI   MVC   ISPL1,=F'1'         SET READ HIGH ROUTINE NUMBER                 
         XC    ISPL3(2),ISPL3      CLEAR ERROR RETURN                           
         XC    ISKEY,ISKEY         SET KEY FOR FIRST RECORD                     
         L     RF,VISDDS                                                        
         LA    R1,ISPL                                                          
         BASR  RE,RF                                                            
         OC    ISPL3(2),ISPL3      TEST FOR ERRORS                              
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
ISRDSEQ  MVC   ISPL1,=F'2'         SET READ SEQ ROUTINE NUMBER                  
         XC    ISPL3(2),ISPL3      CLEAR ERROR RETURN                           
         L     RF,VISDDS                                                        
         LA    R1,ISPL                                                          
         BASR  RE,RF                                                            
         OC    ISPL3(2),ISPL3      TEST FOR ERRORS                              
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
GETREC   LA    R2,GRDMCB           GETREC - R2=A(CALLERS DMCB)                  
         MVI   GRDMCB+8,0          CLEAR ERROR RETURN IN DMCB                   
         L     RF,VGETREC                                                       
         LA    R1,GRPL                                                          
         BASR  RE,RF                                                            
         CLI   GRDMCB+8,0          TEST ERRORS POSTED IN CALLERS DMCB           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         B     EXIT                BRANCH TO TEST LOOP OR EXIT                  
                                                                                
LOOP1    ICM   R5,15,LOOPCNT       SET LOOP COUNT                               
         BNP   LOOP1X                                                           
         ICM   R6,15,GETRCNT       SET GETREC GOUNT                             
         BNZ   *+14                                                             
         LR    R6,R5                                                            
         AHI   R6,1                                                             
         ST    R6,GETRCNT                                                       
         XC    ISKEY,ISKEY         SET KEY OF FIRST RECORD TO READ              
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',XXXDIR,ISKEY,ISREC                   
*                                                                               
LOOP1A   GOTO1 =V(DATAMGR),DMCB,=C'DMRSEQ',XXXDIR,ISKEY,ISREC                   
         CLI   DMCB+8,0                                                         
         BE    LOOP1B                                                           
         TM    DMCB+8,X'80'        TEST EOF                                     
         BO    LOOP1X                                                           
         DC    H'0'                                                             
*                                                                               
LOOP1B   BCT   R6,LOOP1C           TEST IF GETREC COUNTER ZERO                  
         L     R6,GETRCNT                                                       
         LA    R0,ISREC            BUILD DMCB FOR GETREC DATAMGR CALL           
         AH    R0,XXXDDA                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'GETREC',XXXFIL,(R0),A(DAREC),DAWRK           
         CLI   8(R1),0                                                          
         BNE   LOOP1C              IGNORE GETREC ERRORS                         
*                                                                               
LOOP1C   BCT   R5,LOOP1A                                                        
*                                                                               
LOOP1X   B     EXIT                                                             
                                                                                
LOOP2    ICM   R5,15,LOOPCNT       SET LOOP COUNT                               
         BNP   LOOP2X                                                           
         ICM   R6,15,GETRCNT       SET GETREC COUNT                             
         BNZ   *+14                                                             
         LR    R6,R5                                                            
         AHI   R6,1                                                             
         ST    R6,GETRCNT                                                       
         XC    ISKEY,ISKEY         SET KEY OF FIRST RECORD TO READ              
         MVC   ISPL1,=F'1'         SET READ HIGH CODE                           
         XC    ISPL3(2),ISPL3                                                   
         L     RF,VISDDS                                                        
         LA    R1,ISPL                                                          
         BASR  RE,RF                                                            
         OC    ISPL3(2),ISPL3      TEST FOR ERRORS READING FIRST REC            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOOP2A   MVC   ISPL1,=F'2'         SET READ SEQ CODE                            
         XC    ISPL3(2),ISPL3                                                   
         L     RF,VISDDS                                                        
         LA    R1,ISPL                                                          
         BASR  RE,RF                                                            
         OC    ISPL3(2),ISPL3      TEST FOR ERRORS                              
         BZ    LOOP2B                                                           
         TM    ISPL3+1,X'04'       TEST EOF                                     
         BO    LOOP2X                                                           
         DC    H'0'                                                             
*                                                                               
LOOP2B   BCT   R6,LOOP2C           TEST IF GETREC COUNTER ZERO                  
         L     R6,GETRCNT                                                       
         LA    R2,GRDMCB           R2=A(CALLERS DMCB)                           
         MVI   GRDMCB+8,0          CLEAR ERROR RETURN IN DMCB                   
         L     RF,VGETREC                                                       
         LA    R1,GRPL                                                          
         BASR  RE,RF                                                            
         CLI   GRDMCB+8,0                                                       
         BE    LOOP2C              IGNORE GETREC ERRORS                         
*                                                                               
LOOP2C   BCT   R5,LOOP2A                                                        
*                                                                               
LOOP2X   B     EXIT                                                             
                                                                                
EXIT     XBASE                                                                  
         EJECT                                                                  
DUB      DS    D                                                                
PLIST    DS    6F                                                               
*                                                                               
LOOPCNT  DC    F'1000000'          LOOP COUNTER FOR TIMIMG TEST                 
GETRCNT  DC    F'100'              GETREC COUNTER EVERY N'TH ISREC              
*                                                                               
XXXDIR   DC    C'ACCDIR  '         ISFILE NAME                                  
XXXFIL   DC    C'ACCMST  '         DAFILE NAME                                  
XXXDDA   DC    H'50'               DISPLACEMENT TO DISK ADDR IN KEY             
XXXSEN   DC    X'06'               SE NUM                                       
XXXSYS   DC    C'ACC'                                                           
XXXLST   DC    C'NACCDIR NACCMST X '                                            
*                                                                               
VISDTF   DS    A                   A(IS FILE) RETURNED FROM DMFAST              
VDADTF   DS    A                   A(DA FILE) RETURNED FROM DMFAST              
VISDDS   DS    A                   A(ISDDS)   RETURNED FROM DMFAST              
VGETREC  DS    A                   A(GETREC)  RETURNED FROM DMFAST              
*                                                                               
DMCB     DS    0XL24               DMCB FOR DATAMGR CALLS                       
DMCB1    DC    F'0'                                                             
DMCB2    DC    F'0'                                                             
DMCB3    DC    F'0'                                                             
DMCB4    DC    F'0'                                                             
DMCB5    DC    F'0'                                                             
DMCB6    DC    F'0'                                                             
*                                                                               
GRDMCB   DS    0XL24               GETREC DMCB FOR DATAMGR                      
GRDMCB1  DC    F'0'                                                             
GRDMCB2  DC    F'0'                                                             
GRDMCB3  DC    F'0'                                                             
GRDMCB4  DC    F'0'                                                             
GRDMCB5  DC    F'0'                                                             
GRDMCB6  DC    F'0'                                                             
*                                                                               
ISPL     DS    0XL32               ISDDS PARAM LIST                             
ISPL1    DC    F'0'                A(ROUTINE) OR F'I/S ROUTINE NUMBER'          
ISPL2    DC    F'0'                A(I/O AREA)                                  
ISPL3    DC    F'0'                XL2 ERROR FLAGS                              
ISPL4    DC    F'0'                XL1=FILENUM,AL3(DTFIS)                       
ISPL5    DC    F'0'                A(KEY)                                       
ISPL6    DC    F'0'                                                             
ISPL7    DC    F'0'                                                             
ISPL8    DC    F'0'                                                             
*                                                                               
*ISPL3+0 X'80' UNRECOVERABLE DISK ERROR                                         
*ISPL3+0 X'20' DUPLICATE KEY ON ADD                                             
*ISPL3+1 X'08' RECORD NOT FOUND                                                 
*ISPL3+1 X'04' EOF                                                              
*                                                                               
GRPL     DS    0XL32               GETREC PARAM LIST                            
GRPL1    DC    F'0'                                                             
GRPL2    DC    F'0'                A(I/O AREA)                                  
GRPL3    DC    F'0'                                                             
GRPL4    DC    F'0'                XL1=FILENUM,AL3(DTFPH)                       
GRPL5    DC    F'0'                                                             
GRPL6    DC    F'0'                                                             
GRPL7    DC    F'0'                                                             
GRPL8    DC    F'0'                                                             
*                                                                               
ISKEY    DC    XL42'00'                                                         
ISREC    DC    XL54'00'                                                         
DAWRK    DC    XL72'00'                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
DAREC    DC    1000D'0'                                                         
*                                                                               
         DC    C'*WRKWRK*'                                                      
WORK     DC    2000D'0'                                                         
*                                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',1024X'00'                          
UTL      DC    F'0',X'01',XL3'00',XL252'00'                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DMFAST    12/01/10'                                      
         END                                                                    
