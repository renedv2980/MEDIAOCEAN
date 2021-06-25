*          DATA SET NESPBTEST  AT LEVEL 039 AS OF 08/10/00                      
*PHASE T31E24A                                                                  
*INCLUDE NETSPB                                                                 
         TITLE 'T31E24 -EFFICIENCY REPORT'                                      
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*    NETWORK SCHEDULE EFFICIENCY ANALYSIS PRINT PROGRAM               *         
*                                                                     *         
*       INPUTS :  NETBLOCK SETUP BY EDIT                              *         
*                 CLIENT RECORD WILL BE RETURNED IN W/S1              *         
*       GLOBALS:  R5->CURRENT PRINT LINE                              *         
*                 R6->DEMOS                                           *         
*                 R7->WORKING STORAGE                                 *         
*                 R8->DSECT FOR SPOOL PRINTING                        *         
*                 R9->NETWORK SYSTEM DSECT                            *         
*                 RC->GEND                                            *         
*       LOCALS:   R4->BOXD                                            *         
***********************************************************************         
*                                                                               
T31E24   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SEAL**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS3                                                       
         USING WORKD,R7                                                         
         MVI   FRST,0                                                           
*                                                                               
         MVI   NBDATA,C'B'                                                      
         OI    NBSPLOPT,X'80'      HANDLE PRODUCT SPLITS                        
FFIB     GOTO1 NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    XIT                                                              
         CLI   NBMODE,NBPROCUN                                                  
         BNE   FFIB                                                             
         USING SPLTBLKD,R5                                                      
         LR    R5,R7                                                            
         LA    R1,NETBLOCK                                                      
         ST    R1,SPLANETB         SET NETBLOCK                                 
         LA    R2,HOOK                                                          
         ST    R2,SPLAHOOK                                                      
         MVI   SPLPRECI,X'02'                                                   
         MVC   SPLAMT,=F'3098600'  SET DOLLAR AMT TO BE SPLIT                   
         MVC   SPLPRD,=C'FLT'      SET PRODUCT                                  
         MVC   SPLIPRD,=C'CC '      SET PRODUCT                                 
         GOTO1 =V(NETSPB),DMCB,SPLTBLKD                                         
         GOTO1 =V(NETSPB),DMCB,SPLTBLKD                                         
         B     XIT                                                              
HOOK     NTR1                                                                   
         CLI   FRST,1                                                           
         BE    HK3                                                              
         MVI   FRST,1                                                           
         LA    R2,P                                                             
         LA    R1,SPLPLIST                                                      
HK2      MVC   0(3,R2),0(R1)                                                    
         LA    R2,3(R2)                                                         
         LA    R1,3(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   HK2                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
HK3      MVC   P(3),SPLOPRD                                                     
         LA    R2,SPLODOLS                                                      
         EDIT  (P8,0(R2)),(10,P+7),2                                            
HK7      GOTO1 SPOOL,DMCB,(R8)                                                  
XIT      XIT1                                                                   
         SPACE 3                                                                
*                                                                               
WORKD    DSECT                                                                  
FRST     DS    CL1                                                              
         EJECT                                                                  
DEMOS    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF4D                                                       
         PRINT ON                                                               
       ++INCLUDE SPLTBLKD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039NESPBTEST 08/10/00'                                      
         END                                                                    
