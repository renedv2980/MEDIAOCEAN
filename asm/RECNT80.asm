*          DATA SET RECNT80    AT LEVEL 008 AS OF 05/29/96                      
*PHASE T80280A,+0                                                               
*INCLUDE UNBOOK                                                                 
*INCLUDE UNOVRD                                                                 
*INCLUDE OVRD                                                                   
*INCLUDE RETEXT                                                                 
*INCLUDE UNTEXT                                                                 
*INCLUDE UNUPGR                                                                 
***********************************************************************         
*                                                                               
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                                         
*                                                                               
*                                                                               
***********************************************************************         
         TITLE 'COMMON ROUTINES - T80280'                                       
T80280   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80280,RR=R5                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         SPACE 1                                                                
         LA    R2,CORES                                                         
         LA    R4,CLIST                                                         
         SPACE 1                                                                
INIT     XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         MVC   DMCB+7(1),0(R4)     OVERLAY NUMBER                               
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(R2)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   INIT                                                             
         SPACE 1                                                                
         MVC   BOOKVAL(56),CORES   GET CORE RESIDENT PHASE                      
         MVC   UPVAL,CORES+56                                                   
         SPACE 1                                                                
         LA    RE,VLIST            RESOLVE ADDRESSES                            
         LA    RF,UNBOOK           POINTER TO WORKING STORAGE                   
         LA    R3,VLISTENT         COUNTER OF VLIST ENTRIES                     
         SPACE 1                                                                
COMMN    L     R4,0(RE)                                                         
         AR    R4,R5               RELOCATE ADDRESS                             
         ST    R4,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R3,COMMN                                                         
         B     EXXMOD                                                           
CLIST    DC    X'00'               BOOKVAL                                      
         DC    X'01'               CENTER                                       
         DC    X'02'               CHOPPER                                      
         DC    X'03'               DAYVAL                                       
         DC    X'E0'          ***DEMOCON***  (WAS DEMCON)                       
         DC    X'05'               DEMEX                                        
         DC    X'06'               DEMOTAB                                      
         DC    X'07'               DEMVAL                                       
         DC    X'08'               DEMUP                                        
*         DC    X'09'                                                           
*         DC    X'0A'                                                           
*         DC    X'0B'                                                           
         DC    X'0C'               SPOOL                                        
         DC    X'0D'               SQUASHER                                     
         DC    X'0E'               TIMVAL                                       
         DC    X'0F'               UNDAY                                        
         DC    X'10'               UNDERLIN                                     
*         DC    X'11'               UNTIME                                      
*         DC    X'12'               XSORT                                       
         DC    X'13'               UPVAL                                        
         DC    X'FF'                                                            
         SPACE 1                                                                
CORES    DS    0F                                                               
VBOOKVAL DS    V                                                                
VCENTER  DS    V                                                                
VCHOPPER DS    V                                                                
VDAYVAL  DS    V                                                                
VDEMCON  DS    V                                                                
VDEMEX   DS    V                                                                
VDEMOTAB DS    V                                                                
VDEMVAL  DS    V                                                                
VDEMUP   DS    V                                                                
*VINVEDIT DS    V                  NOT USED - FROM AVAILS...                    
*VPAVCOND DS    V                                                               
*VPAVEXPL DS    V                                                               
VSPOOL   DS    V                                                                
VSQUASH  DS    V                                                                
VTIMVAL  DS    V                                                                
VUNDAY   DS    V                                                                
VUNDERLN DS    V                                                                
VUNTIME  DS    V                                                                
VXSORT   DS    V                                                                
VUPVAL   DS    V                                                                
*                                                                               
* ORDER OF VCONS MUST MATCH ORDER USED IN WORKING STORAGE                       
*                                                                               
VLIST    DC    V(UNBOOK)                                                        
         DC    V(UNUPGR)                                                        
         DC    V(OVRD)                                                          
         DC    V(UNOVRD)                                                        
         DC    V(RETEXT)                                                        
         DC    V(UNTEXT)                                                        
VLISTENT EQU   (*-VLIST)/4                                                      
         PRINT OFF                                                              
*              RECNTWR2K                                                        
       ++INCLUDE RECNTWR2K                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008RECNT80   05/29/96'                                      
         END                                                                    
