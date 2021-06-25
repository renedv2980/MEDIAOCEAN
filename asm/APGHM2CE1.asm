*          DATA SET APGHM2CE1  AT LEVEL 001 AS OF 08/27/93                      
*                                                                               
*PHASE ACHM2CE1,+0                                                              
         TITLE 'APG HOOK FOR STDHRS YTD ADJUSTMENT'                             
ACHM2CE1 CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R8                                                    
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT REC                             
*-----------------------------------------------------------------*             
*        CHECK TO SEE IF COLUMN IS UNDER OR OVER 10%              *             
*        IF UNDER THEN ELEMINATE.                                 *             
*        THIS IS BECAUSE KEYCOL HAPPENS BEFORE COLCOMPS           *             
*-----------------------------------------------------------------*             
HOOK01   CLI   HOOKNUM,1           HOOK CALL WHEN FORMING COLS                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING R1SORTD,R5                                                       
*        CP    S1MHR1,=P'0'        DON'T DIVIDE BY ZERO                         
*        BE    HOOK10                                                           
*        ZAP   MYDUB,S1MHR2                                                     
*        MP    MYDUB,=P'100'                                                    
*        DP    MYDUB,S1MHR1                                                     
*        SP    MYDUB(8),=PL8'10'                                                
*        BP    XITOK                                                            
*OOK10   EQU   *                                                                
         CP    S1YHR1,=P'0'        DON'T DIVIDE BY ZERO                         
         BE    XITNOT              LOSE THIS RECORD                             
         ZAP   MYDUB,S1YHR2                                                     
         MP    MYDUB,=P'100'                                                    
         DP    MYDUB,S1YHR1                                                     
         SP    MYDUB(8),=PL8'10'                                                
         BP    XITOK                                                            
         LTR   R5,R5                                                            
         B     XITNOT              DON'T KEEP THIS RECORD                       
         SPACE 4                                                                
XITOK    SR    R0,R0               THIS XIT SETS CC TO YES REC OK               
XITNOT   XIT1                      EXIT                                         
         EJECT                                                                  
*                                                                               
ONETIME  DC    AL1(NO)                                                          
MYDUB    DS    PL16                                                             
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER REPTAB                                            
*                                                                               
R1SORTD  DSECT COVERS SORT RECORD PASSED FROM ACAPGUTILS                        
S1REPORT DS    XL1                 REPORT NUMBER                                
         DS    XL1                 NUMBER OF COPIES                             
S1OFFICE DS    CL1                 ROW 1 OFFICE AC(1)                           
         DS    CL13                                                             
         DS    XL2                 REPORT NUMBER/NUMBER OF COPIES               
S1DEPT   DS    CL2                 ROW 2 DEPARTMENT AC+1(2)                     
         DS    CL12                                                             
         DS    XL2                 REPORT NUMBER/NUMBER OF COPIES               
S1SDEPT  DS    CL2                 ROW 3 SUB-DEPARTMENT AC+3(2)                 
         DS    CL12                                                             
         DS    XL2                 REPORT NUMBER/NUMBER OF COPIES               
S1SEMP   DS    CL7                 ROW 4 EMPLOYEE AC+5(7)                       
         DS    CL7                                                              
         DS    CL32                SPARE                                        
REPORT   EQU   *-R1SORTD           DISP TO REPORT NUMBER                        
         DS    XL1                 REPORT NUMBER                                
         DS    XL1                 NUMBER COPIES                                
         DS    XL2                 ZEROS                                        
         DS    CL36                OFFICE CODE NAME                             
         DS    CL36                DEPARTMENT                                   
         DS    CL36                SUB DEPARTMENT                               
         DS    CL36                EMPLOYEE                                     
         DS    CL36                SPARE                                        
         DS    CL36                SPARE                                        
BUKLOC   EQU   *-R1SORTD           BEGINNING OF BUCKETS (COLUMNS)               
S1MHR1   DS    PL8                 MONTH HRS 1C (HIDDEN)                        
         DS    PL8                 MONTH HRS 1C W/ CA+2(1)=9 OR X               
         DS    PL8                 COLCOMP                                      
S1MHR2   DS    PL8                 MONTH HRS 1C W/O CA+2(1)=9 OR X              
         DS    PL8                 COLCOMP                                      
         DS    PL8                 MONTH HRS 1C                                 
S1YHR1   DS    PL8                 PER HRS 1C (HIDDEN)                          
         DS    PL8                 PER HRS 1C W/ CA+2(1)=9 OR X                 
         DS    PL8                 COLCOMP                                      
S1YHR2   DS    PL8                 PER HRS 1C W/O CA+2(1)=9 OR X                
         DS    PL8                 COLCOMP                                      
         DS    PL8                 PER HRS 1C                                   
S1LEN    EQU   *-R1SORTD           LENGTH OF SORT RECORD                        
         EJECT                                                                  
*        INCLUDED HERE                                                          
*        ACREPWORKD                                                             
*        ACAPGWORKD                                                             
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACAPGWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001APGHM2CE1 08/27/93'                                      
         END                                                                    
