*          DATA SET SPLDEXTAND AT LEVEL 104 AS OF 02/04/00                      
*PHASE SPEXTANN,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - FIX VKTO NETWORKS - CHANGED LOCAL STATIONS'           
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
*                                                                               
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
*        CHECK BUY RECORD                                                       
*                                                                               
         USING SPGENBUYD,R3                                                     
         CLI   0(R3),X'83'         VKTO NETWORK BUY                             
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   BDSTART,=X'630A12'   LOOK FOR BUYS AFTER OCT18/99                
         BL    DMXKEEP                                                          
         CLC   BDSTART,=X'640109'   AND BEFORE JAN09/00                         
         BH    DMXKEEP                                                          
*                                                                               
         MVI   CHANGED,C'N'                                                     
*                                                                               
         CLC   =X'0000',BUYMSTA    NETWORK LEVEL BUY                            
         BNE   DMX200              CHECK LOCAL STATION MATCH                    
*                                                                               
         LA    R4,CONVTAB          IS THIS TO BE CONVERTED                      
DMX20    CLC   0(3,R4),BUYMSTA+2   MATCH NETWORK STATION                        
         BE    DMX50                                                            
         LA    R4,9(R4)                                                         
         CLC   =X'FFFFFF',0(R4)    EOT                                          
         BNE   DMX20                                                            
         B     DMXKEEP                                                          
*                                                                               
DMX50    LA    R6,BDELEM           FIND 68 ELEMS TO CHANGE                      
DMX52    CLI   0(R6),0                                                          
         BE    DMXX                                                             
         CLI   0(R6),X'68'                                                      
         BNE   DMX56                                                            
         CLC   4(3,R6),3(R4)       MATCH LOCAL STATION                          
         BE    DMX60                                                            
DMX56    ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DMX52                                                            
*                                                                               
DMX60    MVC   4(3,R6),6(R4)       REPLACE STATION                              
         MVI   CHANGED,C'E'                                                     
         LA    R4,9(R4)            NEXT STATION                                 
         CLC   0(3,R4),BUYMSTA+2   SAME NETWORK STATION                         
         BNE   DMXX                                                             
         B     DMX56                                                            
*                                                                               
*                                                                               
*                                                                               
DMX200   LA    R4,CONVTAB          IS THIS TO BE CONVERTED                      
DMX220   CLC   3(3,R4),BUYMSTA+2   MATCH LOCAL STATION                          
         BE    DMX250                                                           
         LA    R4,9(R4)                                                         
         CLC   =X'FFFFFF',0(R4)    EOT                                          
         BNE   DMX220                                                           
         B     DMXKEEP                                                          
*                                                                               
DMX250   GOTO1 =V(HEXOUT),DMCB,(R3),P+1,13                                      
         MVC   BUYMSTA+2(3),6(R4)                                               
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,(R3),P+30,13                                     
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVI   CHANGED,C'K'                                                     
         B     DMXX                                                             
*                                                                               
*                                                                               
DMXX     CLI   CHANGED,C'N'                                                     
         BE    DMXKEEP                                                          
         BAS   RE,ELEMPRT                                                       
         AP    NUMBUYS,=P'1'                                                    
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
BUYPRNT  NTR1                                                                   
         LA    R4,=CL20'BUY RECORD'                                             
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
BPX      XIT1                                                                   
*                                                                               
*                                                                               
ELEMPRT  NTR1                                                                   
         CLI   CHANGED,C'K'        ONLY CHANGED KEY                             
         BE    EPXX                                                             
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,(R3),P+1,13                                      
*                                                                               
         LA    R6,BDELEM                                                        
EP10     CLI   0(R6),0                                                          
         BE    EPX                                                              
         CLI   0(R6),X'68'                                                      
         BE    EP20                                                             
EP15     ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     EP10                                                             
*                                                                               
EP20     GOTO1 =V(HEXOUT),DMCB,(R6),P+30,11                                     
         GOTO1 VPRINTER                                                         
         B     EP15                                                             
*                                                                               
EPX      GOTO1 VPRINTER                                                         
EPXX     XIT1                                                                   
*                                                                               
RECPRNT  NTR1                                                                   
         LA    R4,=CL20'HEADER REC'                                             
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(16),=C'NUMBER OF BUYS ='                                       
         EDIT  (P10,NUMBUYS),(15,P+25)                                          
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
**************************************************************                  
STATAB   DS    0C     = NETWORK STATION / OLD STATION / NEW STATION             
**** HIST                                                                       
CONVTAB  DC    X'6C6C03',X'6C512A',X'6CEB2A'    HEDM/HXED                       
         DC    X'6C6C03',X'6C642A',X'6CEC2A'    HVAN/HXVA                       
         DC    X'6C6C03',X'6C502A',X'6CEA2A'    HCAL/HXCA                       
         DC    X'6C6C03',X'6C6F2A',X'6CE72A'    HWPG/HXWP                       
         DC    X'6C6C03',X'6C5B2A',X'6CDF2A'    HLON/HXLO                       
         DC    X'6C6C03',X'6C5A2A',X'6CDE2A'    HKIT/HXKT                       
         DC    X'6C6C03',X'6C6E2A',X'6CDC2A'    HTON/HXTO                       
         DC    X'6C6C03',X'6C542A',X'6CDD2A'    HOTT/HXOT                       
         DC    X'6C6C03',X'6C532A',X'6CDB2A'    HMON/HXMO                       
**** LIFE                                                                       
         DC    X'6CB903',X'6CEF1E',X'6CED1E'    LSTJ/LSJN                       
**** SHOW                                                                       
         DC    X'76F103',X'6C5921',X'6CA421'    HSTJ/HSJN                       
         DC    X'76F103',X'6C9A21',X'6CA521'    HWDS/HWND                       
         DC    X'76F103',X'6C6521',X'6C6F21'    HWIN/HWPG                       
**** WTN                                                                        
         DC    X'7B3303',X'753D22',X'751322'    OSTJ/OSJN                       
         DC    X'7B3303',X'758022',X'757C22'    OTHU/OTHB                       
**** YTV                                                                        
         DC    X'7C7A03',X'7C0D1F',X'7C031F'    YSTJ/YSJN                       
         DC    X'7C7A03',X'7C501F',X'7C491F'    YTHU/YTHB                       
         DC    X'7C7A03',X'6D201F',X'D31D1F'    LWIN/YWPG                       
**** DISC                                                                       
         DC    X'650F03',X'65451D',X'65301D'    DSTJ/DSJN                       
         DC    X'650F03',X'65711D',X'65901D'    DWIN/DWPG                       
****                                                                            
         DC    X'FFFFFF'                                                        
**************************************************************                  
         EJECT                                                                  
NEXTEST  DC    F'0'                                                             
LASTEST  DC    F'0'                                                             
DATADISP DC    H'0024'                                                          
NUMBUYS  DC    PL10'0'                                                          
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
**                                                                              
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
CHANGED  DS    XL1                                                              
APBYTE   DS    XL1                                                              
ESTOWSDY DS    CL1                                                              
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORK2    DS    CL64                                                             
NUMRECS  DS    PL6                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*SPGENBUY                                                                       
SPGENBUYD      DSECT                                                            
       ++INCLUDE SPGENBUY                                                       
*SPGENEST                                                                       
SPGENESTD      DSECT                                                            
       ++INCLUDE SPGENEST                                                       
*SPGENCLT                                                                       
SPGENCLTD      DSECT                                                            
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'104SPLDEXTAND02/04/00'                                      
         END                                                                    
