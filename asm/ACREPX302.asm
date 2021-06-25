*          DATA SET ACREPX302  AT LEVEL 035 AS OF 08/16/00                      
*PHASE ACX302A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'FIX TIME TOTAL RECORDS'                                         
ACX302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACX3**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACX3D,RC                                                         
         EJECT                                                                  
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
EXIT     XMOD1                                                                  
         EJECT                                                                  
**********************************************************************          
* FIRST FOR REQ                                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING TTHRECD,R2                                                       
REQF     DS    0H                                                               
         ZAP   RECDEL,=P'0'                                                     
         ZAP   RECCHG,=P'0'                                                     
         ZAP   RECFIX,=P'0'                                                     
*                                                                               
REQF010  LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   TTHKTYP,TTHKTYPQ    X'3E'                                        
         MVI   TTHKSUB,TTHKSUBQ    X'0E'                                        
*                                                                               
         MVC   DKEY,IOKEY                                                       
         BAS   RE,DMHGH                                                         
*                                                                               
REQF050  CLC   DIR(TTHKCPY-TTHKEY),=X'3E0E'  STILL TIME TOTAL REC?              
         BNE   REQF900                                                          
         CLI   DIR+TTHKTIME-TTHKEY,TTHKTTOT                                     
         BNE   REQF400                                                          
         MVC   IOKEY,DIR                                                        
         CLI   DIR+TTHKEMOA+1-TTHKEY,X'12'                                      
         BNE   REQF400                                                          
         MVC   DKEY,DIR                                                         
         LA    R2,IO1              CHANGE MONTH IN RECORD                       
         BAS   RE,DMGETR                                                        
         CLI   TTHKEMOA+1,X'12'                                                 
         BE    REQF400                                                          
         CLI   QOPT1,C'D'                                                       
         BNE   REQF060                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'KEY',DIR,C'DUMP',64,=C'1D'                    
         GOTO1 =V(PRNTBL),DMCB,=C'CHG',(R2),C'DUMP',96,=C'1D'                   
REQF060  MVI   TTHKEMOA+1,X'12'                                                 
         BAS   RE,DMPUTR                                                        
         AP    RECFIX,=P'1'                                                     
         B     REQF400                                                          
*&&DO                                                                           
*--------------------------------------------------------------------           
         OC    TTHKEMOA,TTHKEMOA   ZERO, DELETE IT                              
         BE    REQF300                                                          
         CLI   TTHKEMOA+1,X'0C'    WRONG PACKED DECEMBER                        
         BNE   REQF400                                                          
         TM    TTHKSTAT,TTHSDELT   DELETED?                                     
         BO    REQF400             DON'T CHANGE IT                              
*                                                                               
         MVC   DKEY,DIR            SEE IF WE HAVE X'12' FOLLOWING               
         BAS   RE,DMSEQ                                                         
         CLC   IOKEY(TTHKEMOA+1-TTHKEY),DIR                                     
         BNE   REQF075                                                          
         CLI   DIR+TTHKEMOA+1-TTHKEY,X'12'                                      
         BE    REQF100                                                          
REQF075  MVC   DKEY,IOKEY                                                       
         BAS   RE,DMHGH                                                         
         B     REQF200                                                          
*                                                                               
REQF100  LA    R2,IO1              DELETE RECORD                                
         BAS   RE,DMGETR                                                        
         OI    TTHRSTAT,TTHSDELT                                                
         BAS   RE,DMPUTR                                                        
         LA    R2,DIR                                                           
         MVC   TTHKDA,IOKEY+TTHKDA-TTHKEY                                       
         BAS   RE,DMWRTR                                                        
*                                                                               
         MVC   DKEY,IOKEY          GET THE BAD X'0C' RECORD                     
         BAS   RE,DMHGH                                                         
         OI    TTHKSTAT,TTHSDELT                                                
         BAS   RE,DMWRTR                                                        
         AP    RECFIX,=P'1'                                                     
         B     REQF400                                                          
*                                                                               
REQF200  DS    0H                                                               
         LA    R2,IO1              CHANGE MONTH IN RECORD                       
         BAS   RE,DMGETR                                                        
         MVI   TTHKEMOA+1,X'12'                                                 
         BAS   RE,DMPUTR                                                        
         LA    R2,DIR              DELETE OLD KEY                               
         OI    TTHKSTAT,TTHSDELT                                                
         BAS   RE,DMWRTR                                                        
         MVC   DIR,IOKEY           ADD NEW KEY                                  
         MVI   TTHKEMOA+1,X'12'    WITH CORRECT MONTH                           
         BAS   RE,DMADDK                                                        
         MVC   DKEY,DIR                                                         
         BAS   RE,DMHGH                                                         
         MVC   DKEY,DIR                                                         
         AP    RECCHG,=P'1'                                                     
         B     REQF400                                                          
*                                                                               
REQF300  BAS   RE,DELRNK           DELETE RECORD AND KEY                        
         AP    RECDEL,=P'1'                                                     
*--------------------------------------------------------------------           
*&&                                                                             
REQF400  BAS   RE,DMSEQ                                                         
         B     REQF050                                                          
*                                                                               
REQF900  DS    0H                                                               
         MVC   P+2(20),=CL20'RECORDS DELETED: '                                 
         EDIT  RECDEL,(10,P+25),ZERO=NOBLANK                                    
         GOTO1 ACREPORT                                                         
         MVC   P+2(20),=CL20'RECORDS CHANGED: '                                 
         EDIT  RECCHG,(10,P+25),ZERO=NOBLANK                                    
         GOTO1 ACREPORT                                                         
         MVC   P+2(20),=CL20'RECORDS FIXED  : '                                 
         EDIT  RECFIX,(10,P+25),ZERO=NOBLANK                                    
         GOTO1 ACREPORT                                                         
*                                                                               
REQFX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
DELRNK   DS    0H                  DELETE RECORD AND KEY                        
         LR    R7,RE               SAVE RE TO RETURN                            
         LA    R2,IO1                                                           
         BAS   RE,DMGETR                                                        
         OI    TTHRSTAT,TTHSDELT    DELETE IT                                   
         BAS   RE,DMPUTR                                                        
         LA    R2,DIR                                                           
         OI    TTHKSTAT,TTHSDELT                                                
         BAS   RE,DMWRTR                                                        
         BR    R7                                                               
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
ACCDIR   DC    CL6'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL6'GETREC'                                                      
PUTREC   DC    CL6'PUTREC'                                                      
ADDREC   DC    CL6'ADDREC'                                                      
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
*                                                                               
PRNTBL   DC    V(PRNTBL)                                                        
VHELLO   DC    V(HELLO)                                                         
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
CALTAB   DS    0CL6                                                             
         DC    XL3'970105',XL3'970110'                                          
         DC    XL3'970112',XL3'970117'                                          
         DC    XL3'970119',XL3'970124'                                          
         DC    XL3'970126',XL3'970131'                                          
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R6,DISP2,ELCODE                                                  
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATA MANAGER ROUTINES                                              *          
**********************************************************************          
DMRD     LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGETR   LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),ACCMST,DA,(R2),DMWORK                
         B     DMERR                                                            
*                                                                               
DMWRTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         B     DMERR                                                            
*                                                                               
DMADDK   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMADD,ACCDIR,DIR,DIR                                
         B     DMERR                                                            
*                                                                               
DMADDR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R2),DMWORK                        
         B     DMERR                                                            
*                                                                               
DMPUTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R2),DMWORK                        
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* RECORDS IO AREA                                                    *          
**********************************************************************          
         SPACE 1                                                                
IOREC    DS    CL4                 RECORD LENGTH                                
IO1      DS    2000C               MASTER IO                                    
IO2      DS    2000C                                                            
         EJECT                                                                  
**********************************************************************          
* DSECT FOR PROGRAM WORK AREA                                        *          
**********************************************************************          
         SPACE 1                                                                
ACX3D    DSECT                                                                  
DMWRK    DS    12D                                                              
*                                                                               
DISP2    DS    H                                                                
DA1      DS    XL4                                                              
DA2      DS    XL4                                                              
*                                                                               
ELCODE   DS    XL1                                                              
SVSEQ    DS    XL1                                                              
MSG      DS    CL20                                                             
RECDEL   DS    PL8                 RECORDS DELETED                              
RECCHG   DS    PL8                 RECORDS CHANGED                              
RECFIX   DS    PL8                 RECORDS FIXED                                
*                                                                               
FLAG     DS    XL1                                                              
FLGDUP   EQU   X'80'               KEY IS STILL DUPLICATE                       
*                                                                               
SVPEDT   DS    PL3                                                              
*                                                                               
IOKEY    DS    CL64                                                             
DKEY     DS    CL64                                                             
SVIOKEY  DS    CL64                DIRECTORY IO                                 
DIR      DS    CL64                                                             
SVKEY    DS    CL49                                                             
DA       DS    F                                                                
ELEM     DS    CL255                                                            
*                                                                               
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
         EJECT                                                                  
**********************************************************************          
* PRINT LINE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL2                                                              
PKEY     DS    CL54                ORIGINAL 1R KEY                              
PRLNQ    EQU   *-PLINED                                                         
         EJECT                                                                  
**********************************************************************          
* INCLUDES                                                           *          
**********************************************************************          
         SPACE 1                                                                
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035ACREPX302 08/16/00'                                      
         END                                                                    
