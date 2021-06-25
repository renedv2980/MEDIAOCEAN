*          DATA SET STREPFXMVC AT LEVEL 041 AS OF 06/20/00                      
*PHASE SPFX02C                                                                  
*INCLUDE BINSRCH2                                                               
*        TITLE 'STREPFXMST - BUILD AGY/MED/MKT/STA DATASET'                     
         TITLE 'STREPFXMST - BUILD AGY/MED/MKT/STA DATASET'                     
***********************************************************************         
*                                                                     *         
*        STREPFXMST - BUILD AGY/MED/MKT/STA DATASET                   *         
*                                                                     *         
*              READ STATION FILE FOR AL STATIONS                      *         
*              BUILD DATASET OF STATIONS AND THEIR MARKETS            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   SPFX02                                                           
         NMOD1 0,SPFX02,R8,CLEAR=YES                                            
*                                                                               
         L     RA,0(R1)            ESTABLISH SPWORKD                            
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST        ONLY INTERESTED IN 1ST TIME CALL             
         BE    FX                                                               
*                                                                               
         CLI   MODE,RUNLAST        CLOSE FILES AT END OF REQUEST                
         BE    FXRUNLST                                                         
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'STREPFXMST - BUILD AGY/MED/MKT/STA DATASET- FX'                 
***********************************************************************         
*                                                                     *         
*        OUTPUT A FILE CONTAINING ALL STATIONS ON FILE                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FX       DS    0H                                                               
*                                                                               
*        BUILD STARTING STATION KEY                                             
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))  OPEN OUTFILE                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING STARECD,R5                                                       
*                                                                               
         MVI   STAKTYPE,C'S'       STANDARD KEY                                 
         MVC   STAKMED,QMED        STARTING MEDIA                               
*                                                                               
         MVC   SAVEKEY,KEY         SAVE KEY                                     
*                                                                               
         LA    R6,MSTRECC          ESTABLISH RECORD FOR CONVERSION FILE         
         USING MSTREC,R6                                                        
*                                                                               
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
*                                                                               
FXSTALP  DS    0H                                                               
*                                                                               
         L     R5,ADSTAT               ADSTAT-->RECORD FOUND.                   
*                                                                               
         CLC   STAKTYPE(STAKCALL-STAKEY),SAVEKEY DONE ON BREAK IN               
         BNE   FXSTADN             STARTING KEY                                 
*                                                                               
         CLI   STAKMED,C'R'        ONLY INTERESTED IN RADIO                     
         BE    *+12                                                             
         CLI   STAKMED,C'T'        OR TV                                        
         BNE   FXSTACN                                                          
*                                                                               
         CLC   STAKAGY,AGENCY      KEEP IF FOR AGENCY                           
         BNE   FXSTACN                                                          
*                                                                               
         CLI   STAKCALL,C'0'       SKIP IF CABLE                                
         BNL   FXSTACN                                                          
*                                                                               
****     CLC   STAKCLT,=C'000'     KEEP IF DEFAULT STATION RECORD               
****     BNE   FXSTACN                                                          
*                                                                               
*        BUILD CONVERSION FILE RECORD                                           
*                                                                               
         MVC   MSTREC(MSTRECLQ),SPACES INIT CONVERSION FILE RECORD              
*                                                                               
         MVC   MSTKEY,STAKEY       SAVE KEY                                     
         MVC   MSTMKT,SMKT         MARKET                                       
*                                                                               
         GOTO1 MSPACK,DMCB,MSTMKT,STAKCALL,MSTMKSTA  MKT/STA                    
*                                                                               
         MVC   KEY,MSTKEY                                                       
         GOTO1 HIGHSTA             RE-POINT FILE                                
*                                                                               
         MVC   MSTAGYMD,BAGYMD     AGENCY/MEDIA                                 
*                                                                               
         NI    MSTAGYMD,X'F0'      KILL MEDIA                                   
*                                                                               
         CLI   STAKMED,C'T'        IF 'TV'                                      
         BNE   *+8                                                              
         OI    MSTAGYMD,X'01'         MEDIA                                     
*                                                                               
         CLI   STAKMED,C'R' RADIO                                               
         BNE   *+8                                                              
         OI    MSTAGYMD,X'02'      SET MEDIA                                    
*                                                                               
         TM    MSTAGYMD,X'0F'      MUST HAVE A MEDIA                            
         BZ    FXSTACN                                                          
*                                                                               
         PUT   FILEOUT,MSTREC      ADD TO CONVERSION FILE                       
*                                                                               
         LA    R1,P1               ESTABLISH PRINT LINE                         
         USING PLINED,R1                                                        
*                                                                               
         MVC   PKEY,STAKEY         KEY                                          
         UNPK  WORK(3),MSTAGYMD(2)  PRINT AGY/MED                               
         TR    WORK(2),HEXTAB-C'0'                                              
         MVC   PAGYMED,WORK                                                     
         MVC   PMKT,MSTMKT         MARKET                                       
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
FXSTACN DS     0H                                                               
*                                                                               
         GOTO1 SEQSTA              READ NEXT STATION ON FILE                    
*                                                                               
         B     FXSTALP                                                          
*                                                                               
FXSTADN  DS    0H                                                               
*                                                                               
         GOTO1 AENDREQ             GOTO NEXT REQUEST                            
*                                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'STREPFXMST - RENUMBER CANADIAN STATIONS - FXRUNLST'             
***********************************************************************         
*                                                                     *         
*        END OF RUN - CLOSE FILES AND STOP RUN                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FXRUNLST DS    0H                                                               
*                                                                               
         CLOSE FILEOUT             CLOSE CONVERSION FILE                        
*                                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'STREPFXMST - RENUMBER CANADIAN STATIONS - FILEOUT'              
***********************************************************************         
*                                                                     *         
*        DCB FOR INPUT FILE CONTAINING STATIONS WITH OLD AND NEW      *         
*        NUMBERS                                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FILEOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM               
*                                                                               
HEXTAB   DC    C'0123456789ABCDEF' HEX CONVERSION TABLE                         
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STREPFXMST - RENUMBER CANADIAN STATIONS - WORKAREAS'            
***********************************************************************         
*                                                                     *         
*        WORKAREAS                                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SAVEKEY  DS    XL32                                                             
MSTRECC  DS    XL(MSTRECLQ)        CONVERSION RECORD BUILDAREA                  
*                                                                               
         TITLE 'STREPFXMST - RENUMBER CANADIAN STATIONS - MSTRECD'              
**********************************************************************          
*                                                                               
*        DSECT FOR NUMBER CONVERSION FILE                                       
*                                                                               
**********************************************************************          
         SPACE 2                                                                
MSTRECD  DSECT                                                                  
MSTREC   DS    0XL1                CONVERSION RECORD                            
MSTKEY   DS    XL(L'STAKEY)        KEY FOR STATION RECORD                       
MSTMKT   DS    CL4                 MARKET CODE                                  
MSTMKSTA DS    XL5                 MKT/STA PACKED                               
MSTAGYMD DS    XL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-MSTRECD))  SPARE                                        
MSTRECLQ EQU   *-MSTREC            RECORD LENGTH                                
*                                                                               
         TITLE 'STREPFXMST - RENUMBER CANADIAN STATIONS - PLINED'               
***********************************************************************         
*                                                                     *         
*        DSECT FOR PRINT LINE                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PLINED   DSECT                                                                  
PKEY     DS    XL15                KEY                                          
         DS    XL2                                                              
PAGYMED  DS    XL2                 AGENCY MEDIA BYTE                            
         DS    XL2                                                              
PMKT     DS    CL4                 MARKET ID                                    
         DS    XL2                                                              
*                                                                               
         TITLE 'STREPFXMST - RENUMBER CANADIAN STATIONS - STARECD'              
**********************************************************************          
*                                                                               
*        FIXED-RECORDS DSECT                                                    
*                                                                               
**********************************************************************          
         SPACE 2                                                                
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041STREPFXMVC06/20/00'                                      
         END                                                                    
