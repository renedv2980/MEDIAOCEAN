*          DATA SET REREP7A02  AT LEVEL 075 AS OF 01/28/98                      
*PHASE RE7A02C,+0                                                               
***********************************************************************         
* HISTORY LOST                                                                  
*                                                                               
* 8/01/96 - CREATE A DEVELOPMENTAL SALESPERSON LIST  STAN PERLIN                
*                                                                   *           
* JAN28/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*               ***  END TOMBSTONE  ***                                         
***********************************************************************         
         TITLE 'DEVELOPMENTAL SALESMAN LISTING PROGRAM'                         
RE7A02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE7A02,RR=R5                                                 
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         B     MAIN30              ZERO IS GOOD RETURN                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     XIT                                                              
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(DSALPRNT) COMMENT PRINT                         
*                                  REQUIRES NO OTHER TABLE ENTRIES              
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
*        DC    AL1(PROCCONT),AL3(POST)    WOTV/WOOD FIX                         
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
*                                                                               
DSALPRNT NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   RDSPKTYP,X'3A'                                                   
         MVC   RDSPKREP,RCREPFL                                                 
         MVC   KEY,IOAREA                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         B     DSAL0040                                                         
DSAL0020 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY,0                 
DSAL0040 EQU   *                                                                
         CLC   KEY(24),KEYSAVE                                                  
         BNE   XIT                                                              
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK         
*                                                                               
         LA    R6,RDSPREC          LOAD ADDRESS OF DEV SAL REC                  
         SPACE 1                                                                
         CLC   QTEAM,SPACES        CHECK FOR TEAM REQUEST                       
         BE    DSAL0050                                                         
         CLC   RDSPTEAM+1(1),QTEAM                                              
         BNE   DSAL0020                                                         
         SPACE 1                                                                
DSAL0050 EQU   *                                                                
         CLC   QDIV,SPACES         CHECK FOR DIV REQUEST                        
         BE    DSAL0060                                                         
         CLC   RDSPTEAM(1),QDIV                                                 
         BNE   DSAL0020                                                         
         SPACE 1                                                                
DSAL0060 EQU   *                                                                
         CLC   QOFFICE,SPACES      CHECK FOR OFFICE REQUEST                     
         BE    PRTDETL                                                          
         CLC   QOFFICE,RDSPOFF                                                  
         BNE   DSAL0020                                                         
PRTDETL  EQU   *                   SET UP PRINT LINE                            
         MVC   P+3(3),RDSPKSAL                                                  
         MVC   P+16(20),RDSPNAME                                                
         MVC   P+38(12),RDSPTEL                                                 
         MVC   P+54(12),RDSPFAX                                                 
         OC    RDSPLEAV,RDSPLEAV   ANY LEAVE DATE?                              
         BZ    DSAL0090            NO, BYPASS DAT CONVERSION                    
         CLI   QOPTION1,C'B'       WERE INACTIVE SLSM TO BE INCLUDED            
         BNE   DSAL0097            IF NOT, BRANCH TO GET NEXT REC               
         GOTO1 DATCON,DMCB,(2,RDSPLEAV),(5,P+71)                                
DSAL0090 EQU   *                                                                
         CLI   RDSPMGR,C'Y'        IS THIS A MANAGER?                           
         BNE   DSAL0095            NO                                           
*********MVC   P+85(3),=C'YES'                                                  
DSAL0095 EQU   *                                                                
         GOTO1 REPORT                                                           
         B     DSAL0020                                                         
DSAL0097 EQU   *                                                                
         MVC   P,BLANKS            CLEAR PRINTLINE                              
         B     DSAL0020            GET NEXT REC                                 
XIT      XIT1                                                                   
END      XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
CNT      DS    F                                                                
RELO     DS    A                                                                
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
BLANKS   DS    132CL1' '                                                        
KEYMERG  DS    CL27                                                             
KEY2     DS    CL27                                                             
KEYSAVE2 DS    CL27                                                             
*                                                                               
IOAREA   DS    CL1000                                                           
         SPACE 3                                                                
         ORG   IOAREA                                                           
       ++INCLUDE REGENDSP                                                       
         ORG                                                                    
         PRINT ON                                                               
*                                                                               
                                                                                
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075REREP7A02 01/28/98'                                      
         END                                                                    
