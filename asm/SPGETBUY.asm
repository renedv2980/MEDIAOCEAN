*          DATA SET SPGETBUY   AT LEVEL 008 AS OF 03/12/19                      
*PHASE T00A2BC                                                                  
                                                                                
*=================================================================              
* THIS PROGRAM ENABLES PROGRAMS TO BE CONVERTED TO USE 2-BYTE LINE              
* NUMBERS BEFORE THE FILE IS ACTUALLY CONVERTED.                                
*                                                                               
* A GBINIT CALL PASSING THE 2-CHAR AGENCY CODE IS REQUIRED TO                   
* DETERMINE WHETHER 1 OR 2 BYTE LINE NUMBERS ARE USED (GBY1OR2)                 
*                                                                               
* THAT FLAG IS THEN RETURNED BY THE CALLER ON EACH SUBSEQUENT CALL.             
* THE BUY KEYS AND RECORDS WILL THEN ALWAYS BE IN 2-BYTE FORMAT                 
*                                                                               
* THIS PROGRAM MUST BE CALLED FOR ALL FILE I/O FOR BUY RECORDS!                 
*=================================================================              
                                                                                
GETBUY   TITLE 'EMULATE 2-BYTE LINE NUMBERS FOR BUY RECORDS'                    
                                                                                
GETBUY   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 WORKX-WORKD,SPGETBUY,CLEAR=YES                                   
         USING WORKD,RC                                                         
*                                                                               
         LR    RA,R1                                                            
         USING GETBUYD,RA                                                       
*                                                                               
         L     RF,GBYCOMF                                                       
         USING COMFACSD,RF                                                      
         MVC   VDATAMGR,CDATAMGR                                                
*                                                                               
         L     R4,GBYKEYIN         KEYARG ADDRESS                               
*                                                                               
         CLI   GBYACT,GBYHIGH                                                   
         BE    GBHI                                                             
         CLI   GBYACT,GBYSEQ                                                    
         BE    GBSEQ                                                            
         CLI   GBYACT,GBYREAD                                                   
         BE    GBHI                                                             
         CLI   GBYACT,GBYGET                                                    
         BE    GBGET                                                            
         CLI   GBYACT,GBYPUT                                                    
         BE    GBPUT                                                            
         CLI   GBYACT,GBYADD                                                    
         BE    GBADD                                                            
         CLI   GBYACT,GBYWRT       SPTDIR WRITE                                 
         BE    GBHI                                                             
         CLI   GBYACT,GBYINIT                                                   
         BE    GBINIT                                                           
         CLI   GBYACT,GBYCONV                                                   
         BE    GBCONV                                                           
         DC    H'0'                                                             
*                                                                               
GBHI     MVC   KEY(13),0(R4)       MOVE A-M/CL/PR/MK/ST/ES/FL/LIN               
         CLI   GBY1OR2,2           TEST FILE HAS 2-BYTE LINENUM                 
         BE    GBHI2                                                            
*                                                                               
         CLI   KEY+10,0            TEST ACTIVE POINTER                          
         BNE   GBHI2               NO  - LINE IS IN KEY+12                      
         MVC   KEY+11(1),12(R4)    MOVE LINE NUMBER                             
         MVI   KEY+12,1            AND MAKE IT A 1                              
*                                                                               
GBHI2    CLI   GBYACT,GBYWRT                                                    
         BE    GBWRT                                                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(GBYDMIN,=C'DMRDHI'),=C'SPTDIR',KEY,KEY            
*                                                                               
         CLI   GBYACT,C'R'         TEST READ                                    
         BNE   GBDIR                                                            
         CLC   KEY(12),KEYSAVE                                                  
         BNE   GBNF                                                             
         CLI   GBY1OR2,2                                                        
         BNE   GBDIR                                                            
         CLC   KEY(13),KEYSAVE                                                  
         BE    GBDIR                                                            
GBNF     MVI   8(R1),X'10'         SET RECORD NOT FOUND                         
         B     GBDIR                                                            
*                                                                               
GBSEQ    GOTO1 VDATAMGR,DMCB,(GBYDMIN,=C'DMRSEQ'),=C'SPTDIR',KEY,KEY            
                                                                                
GBDIR    MVC   GBYERR,8(R1)        MOVE RESULT FLAGS                            
         NC    GBYERR,GBYDMOUT     MASK UNWANTED BITS                           
*                                                                               
         ICM   R4,15,GBYKEYOT                                                   
         BNZ   *+8                                                              
         L     R4,GBYKEYIN                                                      
         MVC   0(18,R4),KEY        RETURN KEY                                   
*                                                                               
         CLI   GBY1OR2,2                                                        
         BE    GBX                                                              
*                                                                               
         LLC   R0,KEY+11                                                        
         CLI   KEY+10,0                                                         
         BE    *+10                                                             
         LLC   R0,KEY+12                                                        
*                                                                               
         STCM  R0,3,11(R4)         RETURN AS 2-BYTE LINENUM                     
         B     GBX                                                              
*                                                                               
GBWRT    MVC   KEY(18),0(R4)       MOVE KEY/STATUS/DA                           
         CLI   GBY1OR2,2           TEST FILE HAS 2-BYTE LINENUM                 
         BE    GBWRT2                                                           
*                                                                               
         CLI   KEY+10,0            TEST ACTIVE POINTER                          
         BNE   GBHI2               NO  - LINE IS IN KEY+12                      
         MVC   KEY+11(1),12(R4)    MOVE LINE NUMBER                             
         MVI   KEY+12,1            AND MAKE IT A 1                              
*                                                                               
GBWRT2   GOTO1 VDATAMGR,DMCB,(GBYDMIN,=C'DMWRT'),=C'SPTDIR',KEY,KEY             
         B     GBX                                                              
         EJECT                                                                  
GBGET    LA    RE,=C'GETREC'                                                    
         ST    RE,DMCB                                                          
         MVC   DMCB(1),GBYDMIN                                                  
         LA    RE,=C'SPTFILE'                                                   
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(4),GBYDA                                                  
         MVC   DMCB+12(4),GBYIOA                                                
         MVC   DMCB+16(4),GBYDMWRK                                              
         GOTO1 VDATAMGR,DMCB                                                    
*                                                                               
GBCONV   CLI   GBY1OR2,2                                                        
         BE    GBADDX                                                           
*                                                                               
         L     R6,GBYIOA                                                        
         LLC   R0,10(R6)           GET 1-BYTE LINE NUMBER                       
         STCM  R0,3,10(R6)         AND SET AS 2-BYTE LINE NUMBMER               
         B     GBADDX                                                           
*                                                                               
GBPUT    LA    RE,=C'PUTREC'                                                    
         B     GBADD2                                                           
*                                                                               
GBADD    LA    RE,=C'ADDREC'                                                    
*                                                                               
GBADD2   L     R6,GBYIOA           GET A(REC)                                   
         MVC   KEY(13),0(R6)       SAVE KEY                                     
*                                                                               
         CLI   GBY1OR2,2                                                        
         BNE   GBADD4                                                           
         OI    15(R6),BUYRLN2      SET 2-BYTE LINENUM FLAG                      
         B     GBADD6                                                           
*                                                                               
GBADD4   ICM   R0,3,10(R6)         GET 2-BYTE LINENUM                           
         SLL   R0,8                MAKE IT 1-BYTE/00                            
         AHI   R0,1                AND NOW MAKE IT 1-BYTE/01                    
         STCM  R0,3,10(R6)                                                      
         NI    15(R6),X'FF'-BUYRLN2   TURN OFF 2-BYTE LINENUM FLAG              
*                                                                               
GBADD6   ST    RE,DMCB                                                          
         MVC   DMCB(1),GBYDMIN                                                  
         LA    RE,=C'SPTFILE'                                                   
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(4),GBYDA                                                  
         MVC   DMCB+12(4),GBYIOA                                                
         MVC   DMCB+16(4),GBYDMWRK                                              
         MVC   DMCB+20(4),GBYPRDL                                               
         GOTO1 VDATAMGR,DMCB                                                    
*                                                                               
         MVC   0(13,R6),KEY        RESTORE ORIGINAL DATA FOR CALLER             
*                                                                               
GBADDX   MVC   GBYERR,8(R1)        MOVE RESULT FLAGS                            
         NC    GBYERR,GBYDMOUT     MASK UNWANTED BITS                           
*                                                                               
GBX      XIT1                                                                   
         EJECT                                                                  
*===================================================================            
* LOOK UP AGENCY IN SPGETBTAB (T00A2D) AND SET GBY1OR2                          
*===================================================================            
                                                                                
GBINIT   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=CL6'SSBAD'                                        
         ICM   RE,15,4(R1)         GET SSB                                      
         JZ    *+2                                                              
         USING SSBD,RE                                                          
         LLC   R2,SSBDSPAC         GET ONLINE DSPACE                            
         CLC   =X'0000',SSBCNTL    ONLINE?                                      
         BNE   GBINI10             YES                                          
         IC    R2,SSODSPAC         GET OFFLINE DSPACE                           
         DROP  RE                                                               
*                                                                               
GBINI10  CHI   R2,C'C'             IF CSC                                       
         BNE   GBINI20                                                          
         LHI   R2,C'A'             CHANGE TO MATCH ADV SYS ID                   
*                                                                               
GBINI20  L     RF,GBYCOMF                                                       
         L     RF,CCALLOV-COMFACSD(RF)                                          
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A2D'                                           
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
*                                                                               
         MVI   GBY1OR2,2           PRESET FOR ENTRY IN TABLE                    
*                                                                               
         L     R4,0(R1)            GET PHASE ADDRESS                            
         LM    RE,RF,0(R4)         ENTRY LENGTH/TABLE LENGTH                    
         LA    R4,8(R4)            FIRST ENTRY                                  
         AR    RF,R4               TABLELEN + START = EOT ADDRESS               
*                                                                               
         USING GETBTABD,R4                                                      
GBINI30  CLC   GBYAGY,GETBAGY      MATCH ON AGENCY ALPHA?                       
         BNE   GBINI40                                                          
         CLM   R2,1,GETBDSPC       MATCH ON DSPACE?                             
         BE    GBX                                                              
*                                                                               
GBINI40  BXLE  R4,RE,GBINI30                                                    
*                                                                               
         MVI   GBY1OR2,1                                                        
         B     GBX                                                              
         LTORG                                                                  
*                                                                               
WORKD    DSECT                                                                  
KEY      DS    XL24                                                             
KEYSAVE  DS    XL24                                                             
VDATAMGR DS    A                                                                
*                                                                               
         DS    0D                                                               
DMCB     DS    6F                                                               
WORKX    EQU   *                                                                
*                                                                               
GETBTABD DSECT                                                                  
GETBAGY  DS   CL2                                                               
GETBDSPC DS   C                    DSPACE                                       
GETBTABL EQU   *-GETBTABD                                                       
*                                                                               
       ++INCLUDE SPGETBUYD                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASSB                                                          
         ORG     SSBD                                                           
       ++INCLUDE FASSBOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPGETBUY  03/12/19'                                      
         END                                                                    
