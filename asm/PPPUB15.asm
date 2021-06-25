*          DATA SET PPPUB15    AT LEVEL 023 AS OF 05/01/02                      
*PHASE T40615A,+0                                                               
*INCLUDE XSORT                                                                  
*                                                                               
*       ******  CHANGE LOG  ******                                              
*                                                                               
*  SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                   
*                ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                 
*                                                                               
         TITLE 'T40615 - PUBFILE TAX RATE MAINTENANCE'                          
T40615   CSECT                                                                  
         NMOD1 0,T40615,RR=R9                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         USING T406FFD,RA                                                       
         LA    R9,PUBIO                                                         
         USING PUBREC,R9                                                        
         LA    R4,PUBIO                                                         
         LA    R5,16                                                            
CLEARIO  XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         BCT   R5,CLEARIO                                                       
         LA    R3,53                                                            
         LA    R2,PBLPUBH                                                       
         OC    PUBADDR,PUBADDR         HAS PUBREC ADDRESS BEEN PASSED           
         BZ    ERROR                                                            
         SR    R3,R3                   CLEAR ERROR MSG                          
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
         MVI   PUBIND,0                                                         
         LA    R2,PUBREC+33                                                     
         USING PUBTAXEL,R2                                                      
         MVI   ELCODE,X'22'            LOOK FOR EXISTING TAX EL                 
         BAS   RE,NEXTEL                                                        
         BNE   CKACT3                                                           
         CLI   BACT,1                  CAN'T ADD IF TAX EL EXISTS               
         BNE   CKACT5                                                           
         BAS   RE,CLRSCRN                                                       
         LA    R3,12                                                            
         LA    R2,PBLACTH                                                       
         B     ERROR                                                            
*                                                                               
CKACT3   CLI   BACT,1             MUST BE ADD IF TAX EL DOESN'T EXIST           
         BE    CKACT15                                                          
         B     CKACT5                                                           
*                                                                               
CKACT5   CLI   BACT,4             IF DISPLAY                                    
         BNE   CKACT15                                                          
         BAS   RE,DSPTAX          DISPLAY DATA                                  
         LA    R2,PBLACTH         POSITION CURSOR TO ACTION FIELD               
         MVI   DSPSW,1            TURN ON DISPLAYED SWITCH                      
         B     EXIT                                                             
*                                                                               
CKACT15  CLI   DSPSW,1            IF ADD OR CHANGE AND SCREEN HAS BEEN          
         BE    CKFLDS             DISPL BRANCH TO CHECK ENTERED FLDS            
         BAS   RE,DSPTAX                                                        
         LA    R2,TAXRTE1H        ELSE POSITION CURSOR TO ACCEPT FLDS           
         MVI   DSPSW,1            TURN ON DISPLAYED SWITCH                      
         B     EXIT                                                             
*                                                                               
CKFLDS   TM    PBLMEDH+4,X'20'    IF MEDIA HAS BEEN CHANGED                     
         BO    CKFLDS5                                                          
         B     CKFLDS10                                                         
CKFLDS5  TM    PBLPUBH+4,X'20'    IF PUB HAS BEEN CHANGED                       
         BO    CKRAT5                                                           
*                                                                               
CKFLDS10 CLI   BACT,1             IF ADD BUT FIELDS HAVE BEEN ENTERED           
         BNE   CKFLDS15           BRANCH TO VALIDATE - ELSE DISPLAY             
         TM    TAXRTE1H,X'80'     AND POSITION CURSOR TO ACCEPT FLDS            
         BO    CKRAT5                                                           
*                                                                               
CKFLDS15 BAS   RE,DSPTAX                                                        
         LA    R2,TAXRTE1H                                                      
         B     EXIT                                                             
*                                                                               
CKRAT5   DS    0H                                                               
         SR    R4,R4                                                            
         LA    R6,TAXRTE1H             FIRST RATE ENTERED                       
         LA    R0,3                    DO THREE TIMES                           
CKRAT8   ZIC   R5,5(R6)                HAS AN ENTRY BEEN MADE                   
         LTR   R5,R5                                                            
         BZ    CKDAT5                                                           
         GOTO1 VCASHVAL,DMCB,(4,8(R6)),(R5)    VALIDATE RATE                    
         OI    DMCB,0                                                           
         BZ    CKRAT10                                                          
         LA    R3,FLDINV                                                        
         LR    R2,R6                                                            
         B     ERROR                                                            
CKRAT10  CLC   DMCB+5,=X'0F423F'       RATE CAN'T BE > 99.9999                  
         BNH   CKRAT15                                                          
         LA    R3,FLDINV                                                        
         LR    R2,R6                                                            
         B     ERROR                                                            
CKRAT15  LA    R6,31(R6)               BUMP TO NEXT RATE                        
         AH    R4,=H'1'                R4 STORES NUMBER OF ENTRIES              
         BCT   R0,CKRAT8                                                        
*                                                                               
CKDAT5   DS    0H                                                               
         LA    R6,TAXDTE1H             FIRST DATE ENTERED                       
         LTR   R0,R4                   WERE ANY RATES ENTERED                   
         BZ    DATSRT                                                           
CKDAT8   GOTO1 VDATVAL,DMCB,(0,8(R6)),WORK                                      
         OC    DMCB,DMCB                                                        
         BNZ   CKDAT10                                                          
         LA    R3,DATERR                                                        
         LR    R2,R6                                                            
         B     ERROR                                                            
CKDAT10  LA    R6,31(R6)               BUMP TO NEXT DATE                        
         BCT   R0,CKDAT8                                                        
*                                                                               
DATSRT   DS    0H                                                               
         MVI   DSPSW,0                 CLEAR DISPLAY SWITCH FOR NXT RUN         
         XC    SRTTAB,SRTTAB           CLEAR SORT TABLE                         
         LTR   R0,R4                   WERE ANY ENTRIES MADE                    
         BZ    UPDAT                                                            
*                                                                               
         LA    R6,TAXRTE1H                                                      
         LA    R7,TAXDTE1H                                                      
         LA    R8,SRTTAB                                                        
*                                                                               
DATSRT5  DS    0H                                                               
         GOTO1 VDATVAL,DMCB,(0,8(R7)),WORK                 CONVERT DATE         
         GOTO1 VDATCON,DMCB,(0,WORK),(3,0(R8))                TO BINARY         
*                                                                               
         ZIC   R5,5(R6)                                    CONVERT RATE         
         GOTO1 VCASHVAL,DMCB,(4,8(R6)),(R5)                   TO BINARY         
         MVC   3(3,R8),DMCB+5                                                   
*                                                                               
         LA    R6,31(R6)                                                        
         LA    R7,31(R7)                                                        
         LA    R8,6(R8)                                                         
         BCT   R0,DATSRT5                                                       
*                                                                               
DATSRT10 OC    SRTTAB(6),SRTTAB           IF NO ENTRIES SKIP SORT               
         BZ    UPDAT                                                            
*                                                                               
         GOTO1 =V(XSORT),DMCB,(0,SRTTAB),(R4),6,6,0,RR=RELO                     
*                                                                               
*                                                                               
UPDAT    DS    0H                                                               
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'22'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   UPDAT5                         IF OLD 22 EL EXISTS               
         GOTO1 VRECUP,DMCB,(1,PUBREC),(R2)    DELETE IT                         
*                                                                               
UPDAT5   OC    SRTTAB(6),SRTTAB               IF NO NEW ENTRIES SKIP            
         BZ    UPDAT15                        ADDING ELEMENT                    
         XC    WORK,WORK                                                        
         DROP  R2                                                               
         LA    R5,WORK                                                          
         USING PUBTAXEL,R5                                                      
         LA    R8,SRTTAB                                                        
*                                                                               
         MVC   PUBTAXEL(2),=X'221E'           BUILD NEW 22 EL FROM              
         MVC   PUBTAX1,3(R8)                  SORT TABLE                        
         MVC   PUBTAX1D,0(R8)                                                   
         MVC   PUBTAX2,9(R8)                                                    
         MVC   PUBTAX2D,6(R8)                                                   
         MVC   PUBTAX3,15(R8)                                                   
         MVC   PUBTAX3D,12(R8)                                                  
         DROP  R5                                                               
         GOTO1 VRECUP,DMCB,(1,PUBREC),WORK,(R2)   ADD NEW TAX EL                
UPDAT15  DS    0H                                                               
         MVC   KEY+27(4),PUBADDR          WRITE UPDATED RECORD BACK             
         BAS   RE,PUTPUB                  TO FILE                               
         MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*                                                                               
DSPTAX   NTR1                                                                   
         CLI   SAVSCRN,X'E5'                   WAS OVERLAY CALLED               
         BE    DSPT5                           PREVIOUSLY                       
         LA    R4,PBLLAST                                                       
         GOTO1 VCALLOV,DMCB,(R4),X'D90406E5'   ELSE CALL OVERLAY                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SAVSCRN,X'E5'                                                    
*                                                                               
DSPT5    BAS   RE,CLRSCRN                      CLEAR SCREEN                     
         CLI   BACT,1                          IF ACTION IS VALID ADD           
         BE    DSPTXIT                         EXIT                             
*                                                                               
         LA    R2,PUBREC+33                                                     
         USING PUBTAXEL,R2                                                      
         CLI   0(R2),X'22'                                                      
         BE    DSPT10                                                           
         MVI   ELCODE,X'22'                                                     
         BAS   RE,NEXTEL                       GET 22 EL                        
         BE    DSPT10                                                           
         B     DSPTXIT                                                          
*                                                                               
DSPT10   DS    0H                                                               
         OC    PUBTAX1(6),PUBTAX1                                               
         BZ    DSPT20                                                           
         EDIT  PUBTAX1,(7,TAXRTE1),4                                            
         FOUT  TAXRTE1H                                                         
         GOTO1 VDATCON,DMCB,(3,PUBTAX1D),(5,TAXDTE1)                            
         FOUT  TAXDTE1H                                                         
*                                                                               
DSPT20   OC    PUBTAX2(6),PUBTAX2                                               
         BZ    DSPT30                                                           
         EDIT  PUBTAX2,(7,TAXRTE2),4                                            
         FOUT  TAXRTE2H                                                         
         GOTO1 VDATCON,DMCB,(3,PUBTAX2D),(5,TAXDTE2)                            
         FOUT  TAXDTE2H                                                         
*                                                                               
DSPT30   OC    PUBTAX3(6),PUBTAX3                                               
         BZ    DSPTXIT                                                          
         EDIT  PUBTAX3,(7,TAXRTE3),4                                            
         FOUT  TAXRTE3H                                                         
         GOTO1 VDATCON,DMCB,(3,PUBTAX3D),(5,TAXDTE3)                            
         FOUT  TAXDTE3H                                                         
*                                                                               
*                                                                               
DSPTXIT  XIT1                                                                   
         EJECT                                                                  
*                                                                               
CLRSCRN  NTR1                                                                   
         CLI   SAVSCRN,X'E5'                  FIRST SEE IF TAX SCREEN           
         BNE   CLRSCX                         HAS BEEN LOADED                   
         LA    R5,3                           CLEAR ALL RATE AND                
         LA    R6,TAXRTE1H                    DATE FIELDS                       
CLRSC1   MVC   8(7,R6),=8X'00'                                                  
         MVC   23(8,R6),=8X'00'                                                 
         OI    6(R6),X'80'                                                      
         OI    21(R6),X'80'                                                     
         LA    R6,31(R6)                                                        
         BCT   R5,CLRSC1                                                        
CLRSCX   XIT1                                                                   
*                                                                               
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
NEXT2    IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF REC                                   
         BE    NEXTELX                                                          
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         BL    NEXT2                                                            
         CLI   0(R2),X'51'                                                      
         BE    NEXT2                                                            
NEXTELX  LTR   R2,R2                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
PUBIND   DS    CL1                                                              
ELCODE   DS    CL1                                                              
DATERR   EQU   20                                                               
FLDINV   EQU   2                                                                
SRTTAB   DS    CL18                                                             
ELEAREA  DS    630C                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE PUGENEROL                                                      
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE PUGENOLD                                                       
PUBIO    DS    4000C                                                            
         EJECT                                                                  
         ORG   PUBIO                                                            
       ++INCLUDE PUBREC                                                         
         EJECT                                                                  
TAXEL    DSECT                                                                  
       ++INCLUDE PUBTAXEL                                                       
         EJECT                                                                  
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
       ++INCLUDE PPPUBFFD                                                       
         ORG   PBLLAST                                                          
       ++INCLUDE PPPUBE5D                                                       
         ORG   T406FFD                                                          
         DS    CL16                                                             
BMED     DS    CL1                                                              
BACT     DS    CL1                                                              
BSCR     DS    CL1                                                              
OLNUM    DS    CL1                                                              
PUBADDR  DS    F                                                                
LTLADDR  DS    F                                                                
BPUB     DS    CL6                                                              
BCLT     DS    CL3                                                              
BDIV     DS    CL3                                                              
BDATE    DS    CL3                                                              
APROF    DS    CL1                                                              
SAVSCRN  DS    CL1                                                              
DSPSW    DS    CL1                                                              
*LBACT   DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023PPPUB15   05/01/02'                                      
         END                                                                    
