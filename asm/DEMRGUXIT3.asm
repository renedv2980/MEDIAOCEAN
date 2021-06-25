*          DATA SET DEMRGUXIT3 AT LEVEL 005 AS OF 02/28/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEMRGU3A                                                                 
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DEMRGICEG, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.          *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* THE INPUT TO THE SORT IS A CONCATENTATED DATASET CONSISTING OF:     *         
*  1. THE MERGED SIGNAL RECORDS CONTAINING THE MINOR KEY REPLACEMENT  *         
*      RANGES FOR GIVEN MAJOR KEYS.                                   *         
*  2. A DATASET CONTAINING ALL DEMO RECORDS READ FROM THE EXISTING    *         
*      DEMO FILES WHICH ARE CANDIDATES TO BE UPDATED AS A RESULT OF   *         
*      THIS MERGE.                                                    *         
*                                                                     *         
* THESE TWO DATASETS ARE USED TO CREATE A NEW DATASET OF DEMO RECORDS *         
* WHICH DOES *NOT* CONTAIN ANY MINOR KEYS THAT FALL WITHIN THE        *         
* REPLACEMENT RANGE(S) FOR THE MAJOR KEY. IN ADDITION, IF THE         *         
* REPLACEMENT RANGE(S) CAUSE *ALL* RECORDS FOR THAT MAJOR KEY TO BE   *         
* PURGED FROM THE EXISTING FILE, WE GENERATE A MAJOR KEY THAT IS      *         
* MARKED FOR DELETION. EACH RECORD IS THEN PREFIXED BY THE CHARACTERS *         
* "VV", SO THAT THE RECORDS SORT CORRECTLY DURING THE SPLICE          *         
* OPERATION WHICH FOLLOWS. THE RECORD IS THEN RETURNED TO THE SORT.   *         
*                                                                     *         
***********************************************************************         
DEMRGU3  CSECT                                                                  
*                                                                               
         ENTRY E35                 MUST BE "E35" (FOR DFSORT)                   
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E35,RC              RC = PROGRAM BASE REGISTER                   
E35      STM   RE,RC,12(RD)        SAVE ALL REGS EXCEPT RD                      
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         ICM   R3,15,0(R2)         A(RECORD)                                    
         BZ    EOF                 ON EOF, DO NOT RETURN                        
*                                                                               
         LA    R3,4(R3)            POINT PAST RDW                               
         USING SMAJKEY,R3                                                       
*                                                                               
         CLC   =C'*END OF MAJOR KEY*',18(R3)                                    
         BE    ENDMAJOR            NO MORE FOR THIS MAJOR KEY                   
*                                                                               
         CLC   =C'*REPLACEMENT RANGES*',SEYECAT    "SIGNAL" RECORD?             
         BNE   DEMOREC             NO                                           
*                                                                               
         MVC   NUMVALS,SNUMENTS    NUMBER OF TABLE ENTRIES                      
         L     R0,NUMVALS                                                       
         LA    R5,REPLTABL         COPY REPLACEMENT RANGE TABLE...              
         LA    RF,SREPTABL         ...TO MY STATIC STORAGE                      
NXTVALUE DS    0H                                                               
         MVC   0(REPTABLQ,R5),0(RF)                                             
         LA    R5,REPTABLQ(R5)                                                  
         LA    RF,REPTABLQ(RF)                                                  
         BCT   R0,NXTVALUE                                                      
         B     DELREC              DON'T KEEP THIS RECORD                       
*                                                                               
DEMOREC  DS    0H                                                               
         MVI   RECFOUND,C'Y'       A RECORD EXISTS FOR THIS MAJOR KEY           
         ICM   R0,15,NUMVALS       ANY DATA TO REPLACE?                         
         BNZ   *+12                                                             
         MVI   KEEPFLAG,C'Y'       NO: KEEP THE RECORD                          
         B     KEEPREC                                                          
         LA    R5,REPLTABL         REPLACEMENT KEY TABLE                        
         USING REPTABLD,R5                                                      
CHKNEXT  DS    0H                                                               
         CLC   18(2,R3),MINORKEY   LOWER THAN LOW VALUE?                        
         BNL   *+12                                                             
         MVI   KEEPFLAG,C'Y'       YES: IT'S OKAY                               
         B     KEEPREC                                                          
*                                                                               
         LA    R5,REPTABLQ(R5)     BUMP TO NEXT TABLE ENTRY                     
         BCTR  R0,0                                                             
         CLC   18(2,R3),MINORKEY   HIGHER THAN HIGH VALUE?                      
         BNH   DELREC              NO: SKIP IT!                                 
         LA    R5,REPTABLQ(R5)     BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,CHKNEXT          CHECK AGAINST ALL RANGE PAIRS                
         MVI   KEEPFLAG,C'Y'                                                    
         B     KEEPREC             IT'S OKAY                                    
         DROP  R5                                                               
         EJECT                                                                  
ENDMAJOR DS    0H                                                               
         XC    NUMVALS,NUMVALS     GET SET FOR NEXT MAJOR KEY                   
*                                                                               
* IF ANY RECORDS FOR THE MAJOR KEY WERE FOUND ON THE FILE, AND IF ALL           
* OF THEM WERE PURGED, THEN WE NEED TO GENERATE A DELETE POINTER.               
* WE DO THIS BY OVERWRITING THE "END OF MAJOR KEY" RECORD WITH A                
* LEGITIMATE POINTER (MARKED FOR DELETION), AND RETURNING IT TO                 
* DFSORT.                                                                       
*                                                                               
         CLI   KEEPFLAG,C'Y'       ANY RECORDS KEPT FOR THIS MAJOR KEY?         
         BE    ENDMJ20             YES: DON'T GENERATE DELETED POINTER          
         CLI   RECFOUND,C'N'       ANY RECORDS FOUND FOR MAJOR KEY?             
         BE    ENDMJ20             NO: NO NEED TO GENERATE DELETED PTR          
*                                                                               
         L     R3,0(R2)            A("END OF MAJOR KEY") RECORD                 
         MVC   0(2,R3),=AL2((SEYECAT-SSIGNAL)+4) SET RDW TO BE...               
*                                                ...L'POINTER + L'RDW           
         LA    R3,4(R3)            BUMP PAST RDW                                
         XC    SMINKEY,SMINKEY     CLEAR MINOR KEY                              
         MVC   SRECLEN,=X'FFFF'    SO DELDXMOD TREATS IT AS A POINTER           
         MVI   SSTATUS,X'80'       TURN ON DELETE BIT                           
*                                                                               
         MVI   RECFOUND,C'N'                                                    
         MVI   KEEPFLAG,C'N'                                                    
         B     KEEPREC             KEEP THE MAJOR KEY MARKED FOR DELETE         
*                                                                               
ENDMJ20  DS    0H                                                               
         MVI   RECFOUND,C'N'                                                    
         MVI   KEEPFLAG,C'N'                                                    
         B     DELREC              DON'T KEEP THIS RECORD                       
*                                                                               
KEEPREC  DS    0H                                                               
         L     RE,0(R2)            A(ORIGINAL RECORD)                           
         SR    R6,R6                                                            
         ICM   R6,3,0(RE)          VARIABLE RECORD LENGTH                       
         LA    R4,L'OUT_FLGS(,R6)      PLUS FLAG BYTES                          
         STCM  R4,3,OUT_RDW            EQUALS NEW RECORD LENGTH                 
*                                                                               
         SHI   R6,4                L'RECORD MINUS L'RDW                         
         LA    R0,OUT_REC          A(RECONSTRUCTED RECORD)                      
         LR    R1,R6               L'TARGET AREA                                
         LR    RE,R3               A(ORIGINAL RECORD W/OUT RDW)                 
         LR    RF,R6               L'SOURCE AREA                                
         MVCL  R0,RE               COPY SOURCE TO TARGET                        
         BNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
*                                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         SGR   GR1,GR1                                                          
         LA    R1,OUT_RDW          RESTORE RECORD POINTER                       
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4: DELETE RECORD                      
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORT_HIGH_HALVES                                       
         LMH   GR2,GRE,DFSORT_HIGH_HALVES+8                                     
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE REGS                                 
         BSM   0,RE                RETURN                                       
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
         ORG   DEMRGU3+(((*-DEMRGU3)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
DFSORT_HIGH_HALVES DS 16F                                                       
         SPACE 3                                                                
RECFOUND DC    C'N'                'Y' = AT LEAST ONE RECORD EXISTS...          
*                                  ...FOR THIS MAJOR KEY                        
KEEPFLAG DC    C'N'                'Y' = WE SAVED AT LEAST 1 RECORD...          
*                                  ...FOR THIS MAJOR KEY                        
NUMVALS  DC    F'0'                CURRENT NUMBER OF VALUES IN TABLE            
REPLTABL DS    2000X               TABLE OF REPLACEMENT RANGES                  
*                                                                               
         DS    0L                                                               
         DC    C'*OUTREC*'                                                      
OUT_RDW  DC    F'0'                                                             
OUT_FLGS DC    C'VV'               FOR THE SPLICE OPERATOR LATER ON             
*                                   FLAG THESE AS OVERLAY RECORDS               
OUT_REC  DS    2000X                                                            
         EJECT                                                                  
       ++INCLUDE DEMRGUXITD                                                     
         EJECT                                                                  
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DEMRGUXIT302/28/14'                                      
         END                                                                    
