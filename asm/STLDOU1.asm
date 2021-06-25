*          DATA SET STLDOU1    AT LEVEL 075 AS OF 06/03/99                      
*PHASE STLDOU1                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*        TITLE 'STLDOU1 -STATION FILE LOAD/DUMP MODEL EXTERN'                   
         TITLE 'STLDOU1 -STATION FILE LOAD/DUMP MODEL EXTERN'                   
***********************************************************************         
*                                                                     *         
*        DROP C'N' PASSIVE POINTER                                    *         
*                                                                     *         
*        THIS MODULE IS PART OF A SET                                 *         
*                                                                     *         
*        STREPFXOU1 - BUILDS LIST OF NEW 'N' PASSIVES                 *         
*        STLDOU1    - DROPS 'N' PASSIVES                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
* PARAMETER LIST                                                      *         
*                                                                     *         
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                     *         
*                               X'01'= RECORD IN CORE                 *         
*                               X'FF'= END OF FILE                    *         
*               RETURN VALUE    X'00'= KEEP RECORD                    *         
*                               X'FF'= PURGE RECORD                   *         
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ        *         
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                     *         
*                               X'40'= TAPE OUTPUT                    *         
*                               X'20'= RECORD IS I/S FILE RECORD      *         
* P3=A(PARAM CARD)                                                    *         
* P4=A(FILE DEFN)                                                     *         
* P5=A(PRINTER)                                                       *         
* P6=A(CPRINT)                                                        *         
*                                                                     *         
***********************************************************************         
         TITLE 'STLDOU1 - RENUMBER CANADIAN STATIONS - INIT'                    
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKLQ,STLDOU1                                                   
*                                                                               
         ST    R1,APARM            SAVE A(PARAMETER LIST)                       
         MVC   PLIST,0(R1)         SAVE PARAMETER LIST                          
*                                                                               
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         L     R9,VLDDEFN          ESTABLISH LOAD CONTROLS                      
         USING LDDEFND,R9                                                       
*                                                                               
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
*                                                                               
         TITLE 'STLDOU1 - RENUMBER CANADIAN STATIONS - DMCTL'                   
***********************************************************************         
*                                                                     *         
*        CONTROL FLOW LOGIC                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXCTL   DS    0H                                                               
*                                                                               
         CLI   PRMMODE,PRMMINIQ                                                 
         BE    DMXINIT             INITIALIZE                                   
*                                                                               
         CLI   PRMMODE,PRMMRECQ    NEW RECORD IN CORE                           
         BE    DMXREC              PROCESS                                      
*                                                                               
         CLI   PRMMODE,PRMMEOFQ                                                 
         BE    DMXEOF              END-OF-FILE                                  
*                                                                               
         B     DMXIT                                                            
*                                                                               
*        EXITS                                                                  
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),PRMRKPQ                                                    
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),PRMRPRGQ                                                   
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),PRMREOJQ                                                   
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
*                                                                               
         TITLE 'STLDOU1 - RENUMBER CANADIAN STATIONS - DMXINIT'                 
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
*                                                                               
*                                                                               
*        PRINT TITLES                                                           
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID1+2(8),=CL8'OLD KEY'                                          
         MVC   MID1+40(8),=CL8'NEW KEY'                                         
         MVC   MID1+80(3),=C'OLD'                                               
         MVC   MID1+90(3),=C'NEW'                                               
*                                                                               
         B     DMXIT                                                            
*                                                                               
         TITLE 'STLDOU1 - RENUMBER CANADIAN STATIONS - DMXREC'                  
***********************************************************************         
*                                                                     *         
*        PROCESS NEXT RECORD TO BE ADDED TO FILE                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD FOR PROCESSING               
*                                                                               
*        DETERMINE RECORD TYPE                                                  
*                                                                               
         CLI   0(R3),C'N'          STATION PASSIVE                              
         BE    STN                                                              
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDOU1 - RENUMBER CANADIAN STATIONS - STN'                     
***********************************************************************         
*                                                                     *         
*        DROP 'N' PASSIVES FOR MEDIA C,N,T AND AGENCY OU              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STN      DS    0H                                                               
*                                                                               
         USING STAKEY,R3           ESTABLISH STATION PASSIVE                    
*                                                                               
         CLC   STNKAGY,=C'OU'      DROP IF AGENCY 'OU'                          
         BNE   STNX                                                             
*                                                                               
         CLI   STNKMED,C'C'        AND MEDIA C, N, OR T                         
         BE    *+8                                                              
         CLI   STNKMED,C'N'                                                     
         BE    *+8                                                              
         CLI   STNKMED,C'T'                                                     
         BNE   STNX                                                             
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+2,15,0,0  PRINT OLD KEY                      
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXPURGE                                                         
*                                                                               
STNX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - WORKD'                
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DS    0D                                                               
WORK     DS    CL128                                                            
DMCB     DS    6F                                                               
APARM    DS    A                   A(PARAMETER LIST)                            
*                                                                               
BYTE     DS    X                                                                
COUNT    DS    F                                                                
TEMP     DS    CL(CNVRECLQ)        CONVERSION FILE INPUT AREA                   
CDNRECC  DS    XL(CDNRECLQ)        TABLE ENTRY BUILD AREA                       
TABADDR  DS    A                   A(BINSRCH TABLE)                             
DUB      DS    D                                                                
KEY      DS    CL10                                                             
BAGY     DS    CL2                 AGENCY SAVEAREA                              
WAGY     DS    CL2                 AGENCY SAVEAREA                              
*                                                                               
*        PARAMETER LIST SAVE AREA                                               
*                                                                               
         DS    0D                  ALIGNMENT                                    
PLIST    DS    0CL24               PARAMETER LIST - SAVED                       
PRMMODE  DS    0XL1                CALLING MODE                                 
PRMMINIQ EQU   X'00'                 X'00'= INITIALISE                          
PRMMRECQ EQU   X'01'                 X'01'= RECORD IN CORE                      
PRMMEOFQ EQU   X'FF'                 X'FF'= END OF FILE                         
*                                                                               
PRMRTNCD DS    0XL1                RETURN CODE                                  
PRMRKPQ  EQU   X'00'               X'00'= KEEP RECORD                           
PRMRPRGQ EQU   X'FF'               X'FF'= PURGE RECORD                          
PRMREOJQ EQU   X'FF'               X'FF'/C'EOJ'=PURGE & CAUSE EOJ               
*                                                                               
AREC     DS    A                   A(CURRENT RECORD)                            
*                                                                               
VTAPEOUT DS    A                   V(TAPEOUT DCB)                               
APARAMC  DS    A                   A(PARAMETER CARD)                            
VLDDEFN  DS    A                   A(FILE DEFINITION)                           
VPRINTER DS    A                   V(PRINTER)                                   
VCPRINT  DS    A                   V(CPRINT)                                    
VHEXOUT  DS    A                   V(HEXOUT)                                    
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
         DS    0A                  ALIGNMENT                                    
       ++INCLUDE DDBSRPRMD                                                      
*                                                                               
WORKLQ   EQU   *-WORKD             LENGTH OF WORKING STORAGE                    
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - CDNRECD'              
***********************************************************************         
*                                                                     *         
*        DSECT FOR NUMBER CONVERSION TABLE - DIFFERENT ORDER THAN     *         
*              ORIGINAL FILE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CDNRECD  DSECT                                                                  
CDNREC   DS    0XL1                CONVERSION RECORD                            
CDNKEY   DS    0XL1                KEY FOR TABLE                                
CDNOLDNM DS    XL2                 OLD 2 BYTE NUMBER                            
CDNKEYLQ EQU   *-CDNKEY            KEY LENGTH                                   
*                                                                               
CDNAGYMD DS    CL1                 AGY/MEDIA                                    
CDNSTA   DS    CL5                 STATION CALL LETTERS WITH MEDIA              
CDNAGY   DS    CL2                 AGENCY CODE                                  
CDNNEWNM DS    XL2                 NEW 2 BYTE NUMBER                            
         DS    XL(80-(*-CDNRECD))  SPARE                                        
CDNRECLQ EQU   *-CDNREC            RECORD LENGTH                                
CDNRMAXQ EQU   5000                MACXIMUM NUMBER OF RECORDS IN FILE           
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - CNVRECD'              
***********************************************************************         
*                                                                     *         
*        DSECT FOR NUMBER CONVERSION FILE                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CNVRECD  DSECT                                                                  
CNVREC   DS    0XL1                CONVERSION RECORD                            
CNVAGY   DS    CL2                 AGENCY CODE                                  
CNVSTA   DS    CL5                 STATION CALL LETTERS WITH MEDIA              
CNVAGYMD DS    CL1                 AGY/MEDIA                                    
CNVOLDNM DS    XL2                 OLD 2 BYTE NUMBER                            
CNVNEWNM DS    XL2                 NEW 2 BYTE NUMBER                            
         DS    XL(80-(*-CNVRECD))  SPARE                                        
CNVRECLQ EQU   *-CNVREC            RECORD LENGTH                                
*                                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*SPSTAPACKD                                                                     
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
*SPGENSTA                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075STLDOU1   06/03/99'                                      
         END                                                                    
*                                                                               
