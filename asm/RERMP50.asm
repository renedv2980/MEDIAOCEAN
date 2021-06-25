*          DATA SET RERMP50    AT LEVEL 009 AS OF 03/16/05                      
*          DATA SET RERMP1C    AT LEVEL 152 AS OF 07/16/97                      
*PHASE T81050A,*                                                                
*INCLUDE UPOUT                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T81050 - RERMP1C - DEMO TAPE CREATE'                            
*                                                                               
***********************************************************************         
*                                                                     *         
*- RERMP1C -- DEMO TAPE CREATE                                        *         
*                                                                     *         
*  MOD LOG:                                                           *         
*  --------                                                           *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         SPACE 2                                                                
T81050   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81050**,RR=RE                                                 
         USING T81050,RB,R8                                                     
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
*                                                                               
         L     RC,0(R1)            ESTABLISH GENCON WORKAREA                    
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA             ESTABLISH SCREEN                             
         USING CONHEADH-64,RA                                                   
*                                                                               
*        L     R8,ASPOOLD          ESTABLISH SPOOL WORKAREA                     
*        USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD            ESTABLISH SYSTEM WORKAREA                    
         USING SYSD,R9                                                          
*                                                                               
         L     R7,ACOMFACS         ESTABLLISH COMMON SUBROUTINES                
         USING COMFACSD,R7                                                      
*                                                                               
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         MVI   BLANKS,X'40'                                                     
         MVC   BLANKS+1(131),BLANKS                                             
*                                                                               
*        L     R1,XSORT                                                         
*        A     R1,RELO                                                          
*        ST    R1,ATAPEDCB                                                      
*                                                                               
*                                                                               
         L     R1,TWADCONS                                                      
         A     R1,TSPFUSER-TWADCOND(R1)                                         
         ST    R1,ATAPEDCB                                                      
*                                                                               
RP6      CLI   MODE,VALKEY                                                      
         BNE   RP8                                                              
         BAS   RE,VALREQ                                                        
         BAS   RE,GTINV                                                         
         B     EXXMOD                                                           
         SPACE 1                                                                
RP8      B     EXXMOD                                                           
*                                                                               
EXXMOD   XIT1                                                                   
         EJECT                                                                  
*                                                                               
*-- EDIT THE REQUEST SCREEN                                                     
*                                                                               
VALREQ   NTR1                                                                   
         B     EXXMOD                                                           
*                                                                               
*--READ AND FILTER INVENTORY RECORDS                                            
*                                                                               
GTINV    NTR1                                                                   
         LA    R4,KEY                                                           
         USING REINVREC,R4                                                      
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         B     GTINV300                                                         
*                                                                               
*                                                                               
* GET NEXT RECORD IF A HEADER CHECK IF IT FITS THE REQUEST                      
*                                                                               
GTINV200 OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
GTINV300 CLC   KEY(1),KEYSAVE                                                   
         BNE   EXXMOD                                                           
         CLI   RINVKSRC,0                                                       
         BNE   GTINV200                                                         
         CLI   KEY+27,X'80'                                                     
         BNE   GTINV200                                                         
         MVC   KEYHLD(27),KEY                                                   
         MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         GOTO1 GETREC                                                           
DEL200   GOTO1 INVPTR,DMCB,0(R4),WORK2                                          
         GOTO1 DELPT,DMCB,WORK2                                                 
         MVC   KEY,KEYHLD                                                       
         GOTO1 HIGH                                                             
         B     GTINV200                                                         
*                                                                               
PRINTIT  GOTO1 =V(PRNTBL),DMCB,=C'REC',KEY,C'DUMP',30,=C'1D'                    
         B     GTINV200                                                         
         EJECT                                                                  
*              ROUTINE TO DELETE POINTERS                                       
         SPACE 1                                                                
DELPT    NTR1                                                                   
         L     R2,0(R1)                                                         
DELPT1   CLI   0(R2),0                                                          
         BE    EXXMOD                                                           
         MVC   KEY(27),0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DELPT4                                                           
         GOTO1 =V(PRNTBL),DMCB,=C'REC',KEY,C'DUMP',30,=C'1D'                    
         OI    KEY+27,X'80'                                                     
*        BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
DELPT4   LA    R2,32(R2)                                                        
         B     DELPT1                                                           
         EJECT                                                                  
*              CREATE NEW PASSIVE POINTER                                       
         SPACE 1                                                                
*              PARAM 1   BYTES 1-3 A(INVENTORY RECORD)                          
*              PARAM 2   BYTES 1-3 A(200 BYTE OUTPUT AREA)                      
         SPACE 1                                                                
         DROP  R4                                                               
         USING RINVREC,R2                                                       
         USING RIDPKEY,R4                                                       
INVPTR   NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R4,4(R1)                                                         
         XC    0(200,R4),0(R4)                                                  
         LA    R6,6                                                             
         LA    R3,RINVDP                                                        
         SPACE 1                                                                
INVPTR1  MVI   RIDPKTYP,X'92'                                                   
         MVC   RIDPKREP,RINVKREP                                                
         MVC   RIDPKSTA,RINVKSTA                                                
         MVC   RIDPKDPT,0(R3)                                                   
         MVC   RIDPKINV,RINVKINV                                                
         MVC   RIDPKSTD,RINVKSTD                                                
         SPACE 1                                                                
*                                                                               
*  IF SELF ASSIGNED GET NEXT DAYPART                                            
*  ONLY COMPUTER GENERATED NUMBERS GET THE DAY,QTR HOUR                         
*  AND THE LENGTH FILLED IN.                                                    
*                                                                               
         TM    RINVSTAT,X'80'                                                   
         BO    INVPTR20            BIT ON SELF ASSIGNED                         
*                                                                               
         MVC   RIDPKDAY,RINVOINV+1   MOVE DAY CODE,                             
         MVC   RIDPKQTR,RINVOINV     QUARTER HOUR,                              
         MVC   RIDPKLEN,RINVOINV+2   AND PROGRAM LENGTH TO KEY                  
*                                                                               
         LA    RE,EFFDAT           SPECIAL DAYPARTS                             
INVPTR10 CLI   0(RE),X'FF'                                                      
         BE    INVPTR20                                                         
         CLC   0(1,R3),0(RE)                                                    
         BE    INVPTR15                                                         
         LA    RE,1(RE)                                                         
         B     INVPTR10                                                         
*                                                                               
INVPTR15 XC    RIDPKDAY,RIDPKDAY                                                
         MVC   RIDPKDTE,RINVPEFF                                                
         SPACE                                                                  
INVPTR20 LA    R3,1(R3)            NEXT DAYPART CODE                            
         CLI   0(R3),X'40'                                                      
         BNH   INVPTX                                                           
         LA    R4,32(R4)                                                        
         BCT   R6,INVPTR1          DO NEXT POINTER                              
         SPACE 1                                                                
INVPTX   B     EXXMOD                                                           
         SPACE 1                                                                
*  THESE DAYPARTS GET A DAY CODE, QUARTER HOUR, AND PROGRAM LENGTH              
DAYCOD   DC    C'MDKNPOUXYWZ',X'FF'                                             
         SPACE 1                                                                
*  THESE DAYPARTS GET EFFECTIVE DATE, QUARTER HOUR, AND PROGRAM LENGTH          
EFFDAT   DC    C'VSJ',X'FF'                                                     
         SPACE 1                                                                
         DROP  R2,R4                                                            
         SPACE 1                                                                
         EJECT                                                                  
RELO     DS    A                                                                
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPFFD                                                                      
* DDGENTWA                                                                      
* RERMPWTWA                                                                     
* RERMPD7D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* RERMPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPECD                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
       ++INCLUDE REGENRDP                                                       
         EJECT                                                                  
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*              WORK AREA                                                        
*                                                                               
*  ALL FIELDS DEFINED ABOVE THE DOUBLE LINE OF ASTERIKS                         
*  MUST ALSO BE DEFINED IN THE RERMP30 PHASE.                                   
*                                                                               
TAPECNT  DS    F                   NUMBER OF RECORDS                            
ATAPEDCB DS    F                   ADDRESS OF DCB                               
*                                                                               
STRTOPT  DS    CL3                 START DATE                                   
STRTOPTC DS    CL2                 START DATE COMPRESSED                        
ENDOPT   DS    CL3                 END DATE                                     
ENDOPTC  DS    CL2                                                              
SAVPROG  DS    CL27                PROGRAM NAME                                 
COMPSTA  DS    CL1                 COMPETITIVE STATION (Y,N)                    
*                                                                               
DBEXTRA1 DS    CL128               DBLOCK EXTRA LENGTH                          
RECLEN   DS    H                   RECORD LENGTH                                
*                                                                               
BLANKS   DS    CL132               SPACE FILLED FIELD                           
*                                                                               
DAYTMHD  DS    XL40                HEADER DAY TIME HOLE                         
*                                                                               
KEYHLD   DS    CL27                                                             
WORK2    DS    CL500                                                            
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T810FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T810FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009RERMP50   03/16/05'                                      
         END                                                                    
