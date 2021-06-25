*          DATA SET CTSFM31    AT LEVEL 002 AS OF 08/22/00                      
*PHASE TA0A31A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM31 -- USER PROFILE RECORD PURGE                 *         
*                                                                     *         
*  COMMENTS:     PURGES USER PROFILE RECORDS FOR A GIVEN              *         
*                SYSTEM, AGENCY, MEDIA, AND CLIENT.                   *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMBF (ALL)                                *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER POINTER                    *         
*                R3 -- WORK                                           *         
*                R4 -- CTFILE RECORD                                  *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL                                          *         
*                R7 -- SCREEN PLACE HOLDER                            *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- BASE                                           *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A31 - USER PROFILE RECORD PURGE'                             
TA0A31   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A31**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO             RELOCATION FACTOR                            
*                                                                               
         CLI   MODE,VALREC         DO EVERYTHING AT VALREC                      
         BNE   EXIT                                                             
         EJECT                                                                  
*                                                                               
*   VALIDATE SYSTEM                                                             
*                                                                               
         LA    R2,UPRSYSTH         SYSTEM NAME                                  
         GOTO1 ANY                 REQUIRED                                     
         CLI   UPRSYST,C'A'        PROGRAM INVALID FOR ACC                      
         BE    SYSERR                                                           
         GOTO1 GETFACT,DMCB,0      GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         L     R5,FASYSLST         A(SYSTEM LIST)                               
         DROP  R1                                                               
*                                                                               
         USING SYSLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
*                                                                               
VSYS10   CLC   SYSLUSLT,WORK       MATCH ON SYSTEM?                             
         BE    CLEAR                                                            
         BXLE  R5,R6,VSYS10        TRY NEXT TABLE ENTRY                         
SYSERR   MVC   GERROR,=AL2(INVSYS) SYSTEM NAME NOT IN TABLE                     
         B     VSFMERR                                                          
*                                                                               
*   CLEAR PROGRAM FIELDS                                                        
*                                                                               
CLEAR    LA    R7,UPRFRPGH                                                      
         LA    R1,UPRTAGH                                                       
         ST    R1,ENDPGMS                                                       
ERASE    MVC   8(3,R7),SPACES                                                   
         OI    6(R7),X'80'         TRANSMIT                                     
         LA    R7,11(R7)           BUMP TO NEXT HEADER                          
         C     R7,ENDPGMS                                                       
         BL    ERASE                                                            
         EJECT                                                                  
*                                                                               
*   BUILD FIRST KEY                                                             
*                                                                               
         XC    COUNTER,COUNTER                                                  
         XC    MATCHKEY,MATCHKEY                                                
         LA    R3,MATCHKEY                                                      
         USING CTUREC,R3                                                        
         MVI   CTUKTYP,C'U'        RECORD TYPE U                                
         MVC   CTUKSYS,UPRSYST     SYSTEM                                       
         DROP  R3                                                               
*                                                                               
         XC    HALFKEY,HALFKEY                                                  
         MVC   HALFKEY(2),AGENCY                                                
*                                                                               
         LA    R2,UPRMDIAH         MEDIA                                        
         GOTO1 ANY                 REQUIRED                                     
         CLI   WORK,C'*'                                                        
         BNE   *+14                                                             
         MVI   HALFKEY+2,X'00'                                                  
         B     *+10                                                             
         MVC   HALFKEY+2(1),WORK                                                
*                                                                               
         LA    R2,UPRCLNTH         CLIENT                                       
         GOTO1 ANY                 REQUIRED                                     
         MVC   CLI,SPACES                                                       
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLI(0),WORK                                                      
         MVC   HALFKEY+3(3),CLI                                                 
*                                                                               
         CLI   ACTNUM,8            TEST ACTION PURGE                            
         BE    PURGE                                                            
*                                                                               
         CLI   ACTNUM,9            TEST ACTION UNPURGE                          
         BE    UNPURGE                                                          
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*    PURGE                                                                      
*                                                                               
PURGE    XC    KEY,KEY                                                          
         LA    R7,UPRFRPGH         PLACE HOLDER FOR DISPLAYING PROG.            
         MVC   KEY(12),MATCHKEY                                                 
PURGE10  GOTO1 HIGH                                                             
         L     R4,AIO                                                           
         CLC   MATCHKEY,0(R4)      TEST SAME TYPE/SYSTEM                        
         BNE   PURGEXIT            NO MORE RECORDS                              
*                                                                               
         XC    KEY,KEY             CREATE COMPLETE KEY                          
         MVC   KEY(16),0(R4)                                                    
         MVC   KEY+16(6),HALFKEY                                                
         MVC   SAVEKEY,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),=C'CTFILE',KEY,AIO               
         CLI   DMCB+8,X'10'        RECORD NOT FOUND                             
         BE    PURGE20                                                          
*                                                                               
         OI    27(R4),X'80'        MARK FOR DELETION                            
         LH    R1,COUNTER          INCREMENT COUNTER                            
         LA    R1,1(R1)                                                         
         STH   R1,COUNTER                                                       
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,ENDPGMS                                                       
         SH    R1,=H'11'           LEAVE ROOM FOR MORE SYMBOLS (>>>)            
         ST    R1,ENDPGMS                                                       
         C     R7,ENDPGMS          ANY ROOM                                     
         BL    *+14                                                             
         MVC   UPRLSPG,=C'>>>'     NO                                           
         OI    UPRLSPGH+6,X'80'                                                 
         MVC   8(3,R7),12(R4)      DISPLAY PROGRAM                              
         OI    6(R7),X'80'         TRANSMIT                                     
         LA    R7,11(R7)           BUMP PLACE HOLDER                            
*                                                                               
         XC    KEY,KEY             CHECK                                        
         MVC   KEY(25),SAVEKEY                                                  
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 READ                                                             
         CLI   DMCB+8,X'02'        MAKE SURE RECORD WAS DELETED                 
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    DMINBTS,X'F7'                                                    
*                                                                               
PURGE20  MVC   NEWKEY,SAVEKEY      TO FIND NEXT PROGRAM TYPE                    
         ZIC   R1,NEWKEY+14                                                     
         AH    R1,=H'1'                                                         
         STC   R1,NEWKEY+14                                                     
         MVC   KEY(15),NEWKEY                                                   
         B     PURGE10                                                          
         EJECT                                                                  
*                                                                               
*   UNPURGE                                                                     
*                                                                               
UNPURGE  XC    KEY,KEY                                                          
         LA    R7,UPRFRPGH         LOACTION FOR SCREEN DISP. OF PGMS            
         MVC   KEY(12),MATCHKEY                                                 
UNPURG10 OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         L     R4,AIO                                                           
         CLC   MATCHKEY,KEY        TEST SAME TYPE/SYSTEM                        
         BNE   UNPRGXIT            NO MORE RECORDS                              
*                                                                               
         XC    KEY,KEY             CREATE COMPLETE KEY                          
         MVC   KEY(16),0(R4)                                                    
         MVC   KEY+16(6),HALFKEY                                                
         MVC   SAVEKEY,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMREAD'),=C'CTFILE',KEY,AIO               
         TM    27(R4),X'80'        TEST MARKED FOR DELETION                     
         BZ    UNPURG20            NO                                           
*                                                                               
         NI    27(R4),X'7F'        RESET DELETE BIT                             
         LH    R1,COUNTER          INCREMENT COUNTER                            
         LA    R1,1(R1)                                                         
         STH   R1,COUNTER                                                       
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,ENDPGMS                                                       
         SH    R1,=H'11'           LEAVE ROOM FOR MORE SYMBOLS (>>>)            
         ST    R1,ENDPGMS                                                       
         C     R7,ENDPGMS          ANY ROOM                                     
         BL    *+14                                                             
         MVC   UPRLSPG,=C'>>>'     NO                                           
         OI    UPRLSPGH+6,X'80'                                                 
         MVC   8(3,R7),12(R4)      DISPLAY PROGRAM                              
         OI    6(R7),X'80'         TRANSMIT                                     
         LA    R7,11(R7)           BUMP PLACE HOLDER                            
*                                                                               
         XC    KEY,KEY             TEST                                         
         MVC   KEY(25),SAVEKEY                                                  
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 READ                                                             
         NI    DMINBTS,X'F7'                                                    
         CLI   DMCB+8,0            MAKE SURE RECORD WAS RESTORED                
         BE    UNPURG20                                                         
         DC    H'0'                                                             
*                                                                               
UNPURG20 MVC   NEWKEY,SAVEKEY      TO FIND NEXT PROGRAM TYPE                    
         ZIC   R1,NEWKEY+14                                                     
         AH    R1,=H'1'                                                         
         STC   R1,NEWKEY+14                                                     
         MVC   KEY(15),NEWKEY                                                   
         B     UNPURG10                                                         
         EJECT                                                                  
*                                                                               
*   EXITS                                                                       
*                                                                               
PURGEXIT EDIT  (B2,COUNTER),(4,NUMBER),ALIGN=LEFT,ZERO=NOBLANK                  
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(4),NUMBER                                                
         LA    R5,CONHEAD                                                       
         AR    R5,R0                                                            
         MVC   1(28,R5),=C'USER PROFILE RECORDS DELETED'                        
         B     EXIT                                                             
         SPACE 2                                                                
UNPRGXIT EDIT  (B2,COUNTER),(4,NUMBER),ALIGN=LEFT,ZERO=NOBLANK                  
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(4),NUMBER                                                
         LA    R5,CONHEAD                                                       
         AR    R5,R0                                                            
         MVC   1(29,R5),=C'USER PROFILE RECORDS RESTORED'                       
         B     EXIT                                                             
         SPACE 5                                                                
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
         EJECT                                                                  
RELO     DS    F                   RELOCATION FACTOR                            
*                                                                               
         SPACE 3                                                                
EXIT     XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASYSLST                                                       
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE CTSFMFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
       ++INCLUDE CTSFMBFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTSFMWORKD                                                     
         PRINT ON                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
ENDPGMS  DS    F                                                                
NUMBER   DS    F                                                                
COUNTER  DS    H                                                                
NEWKEY   DS    CL15                                                             
SAVEKEY  DS    XL25                                                             
MATCHKEY DS    XL12                                                             
HALFKEY  DS    CL6                                                              
CLI      DS    CL3                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTSFM31   08/22/00'                                      
         END                                                                    
