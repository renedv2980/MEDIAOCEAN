*          DATA SET SPLDEXTAG  AT LEVEL 006 AS OF 05/10/02                      
*PHASE SPEXAGDS                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'TRANSFER PROGRAM INFO FRO AG TO DS'                             
*                                                                               
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         USING NPGRECD,R6                                                       
*                                                                               
         CLC   0(3,R6),=X'0D2013'                                               
         BNE   DMXPURGE                                                         
*                                                                               
         L     RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         ST    RE,COUNT                                                         
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'G',=C'SPTFILE '),(X'92',AREC),0,0              
         CLI   12(R1),0            SET CC ON EXIT                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         USING NPGEL92,RE                                                       
*  CLEAR DEMO INFO ON 92 ELEMENT                                                
         XC    NPGSHARE,NPGSHARE                                                
         XC    NPGVPHS,NPGVPHS                                                  
         DROP  RE                                                               
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'G',=C'SPTFILE '),(X'93',AREC),0,0              
         CLI   12(R1),0            SET CC ON EXIT                               
         BNE   DMX20                                                            
         L     RE,12(R1)                                                        
         USING NPGEL93,RE                                                       
*  CLEAR DEMO INFO ON 93 ELEMENT                                                
         XC    NPG2VPHS,NPG2VPHS                                                
         DROP  RE                                                               
                                                                                
DMX20    L     RE,COUNT                                                         
         C     RE,=F'30'                                                        
         BH    DMXKEEP                                                          
                                                                                
         GOTO1 =V(PRNTBL),DMCB,=C'REC',AREC,C'DUMP',300,=C'1D'                  
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(5),=C'COUNT'                                                   
         EDIT  (4,COUNT),(8,P+10),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
ELEMENT  DC    XL6'6106B2650100'                                                
COUNT    DS    F                   NUMBER OF RECORDS CHANGED                    
ELCODE   DS    X                                                                
PRCOUNT  DS    F                                                                
OELEMHLD DS    CL108                                                            
NELEMHLD DS    CL108                                                            
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL100                                                            
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VHEXOUT  DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
         EJECT                                                                  
OLDELEM  DSECT                                                                  
OLDCDLEN DS    XL2                                                              
OLD01X16 DS    XL32                                                             
OLD18X25 DS    XL16                                                             
OLD27X34 DS    XL16                                                             
OLD36X51 DS    XL32                                                             
OLD17    DS    XL2                                                              
OLD26    DS    XL2                                                              
OLD35    DS    XL2                                                              
OLD52    DS    XL2                                                              
*                                                                               
NEWELEM  DSECT                                                                  
NEWCDLEN DS    XL2                                                              
NEW01X16 DS    XL32                                                             
NEW17    DS    XL2                                                              
NEW18X25 DS    XL16                                                             
NEW26    DS    XL2                                                              
NEW27X34 DS    XL16                                                             
NEW35    DS    XL2                                                              
NEW36X51 DS    XL32                                                             
NEW52    DS    XL2                                                              
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGENPROG                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPLDEXTAG 05/10/02'                                      
         END                                                                    
