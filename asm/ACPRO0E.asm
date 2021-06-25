*          DATA SET ACPRO0E    AT LEVEL 052 AS OF 03/21/12                      
*PHASE T60B0EA                                                                  
         TITLE 'T60B0E - WORK MAINT'                                            
T60B0E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B0E**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         GOTO1 VMODPTRS,DMCB,(X'80',POINTERS)                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC                                                          
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY                                                          
         B     XIT                                                              
*                                                                               
MODE6    CLI   MODE,DISPREC                                                     
         BNE   MODE8                                                            
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE8    GOTO1 CANWEDEL                                                         
         CLI   MODE,XRECADD                                                     
         BE    SRCHADD                                                          
         CLI   MODE,RECPUT                                                      
         BE    SRCHCHA                                                          
         CLI   MODE,XRECDEL                                                     
         BE    SRCHDEL                                                          
         CLI   MODE,XRECREST                                                    
         BE    SRCHRES                                                          
         B     XIT                                                              
*                                                                               
*              CHANGE SEARCH POINTERS                                           
SRCHADD  MVI   DMCB,C'A'                                                        
         B     SRCH                                                             
SRCHCHA  MVI   DMCB,C'C'                                                        
         B     SRCH                                                             
SRCHDEL  MVI   DMCB,C'D'                                                        
         B     SRCH                                                             
SRCHRES  MVI   DMCB,C'R'                                                        
*                                                                               
SRCH     MVC   DMCB+1(3),=C'TF '                                                
         GOTO1 VACSRCHP,DMCB,,AIO,0,0,ACOMFACS,AACCFACS                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
VKEY     NTR1                      WORK CODE RECORD                             
         LA    R2,PROWRKH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLC   WORK(2),=C'99'      BILLING WORKCODE ?                           
         BE    ERREND              YES                                          
         CLC   WORK(2),=C'**'      ORDER WORKCODE ?                             
         BE    ERREND              YES                                          
         CLI   WORK,C' '           CAN'T START WITH A SPACE EITHER              
         BE    ERREND                                                           
         TRT   WORK(2),TRTTAB                                                   
         LA    R2,PROWRKH          RESTORE REGISTER 2                           
         BNE   ERREND                                                           
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         BNE   VKEY04                                                           
                                                                                
         SR    R3,R3                                                            
         MVC   KEY,SPACES          READ AND COUNT WORKCODES                     
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CUL                                                     
                                                                                
         GOTO1 HIGH                                                             
         B     VKEY04                                                           
                                                                                
VKEY02   GOTO1 SEQ                                                              
                                                                                
VKEY04   CLC   KEYSAVE(4),KEY                                                   
         BNE   VKEY06                                                           
         LA    R3,1(R3)                                                         
         CHI   R3,MAXWC                                                         
         BL    VKEY02                                                           
         MVI   ERROR,MAXWCODE                                                   
         B     ERREND                                                           
                                                                                
VKEY06   MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CUL                                                     
         MVC   KEY+4(2),WORK                                                    
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
*                                                                               
VREC     NTR1                                                                   
         L     R3,AIO                                                           
         LA    R2,PRODESCH                                                      
         GOTO1 ANY                                                              
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELCODE,WCOELQ       CHECK IF EXISTING ELEMENT                    
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT                                                       
         USING WCOELD,R6                                                        
         MVI   WCOEL,WCOELQ                                                     
         MVI   WCOLN,WCOLNQ                                                     
         MVC   WCOCODE,PROWRK                                                   
         MVC   WCODESC,WORK                                                     
*                                                                               
         MVI   WCOGRP,0                                                         
         LA    R2,PROWGRH          WORK GROUP IS OPTIONAL                       
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY1,KEY                                                     
         MVI   OPTION,C'Y'                                                      
         GOTO1 VALWG                                                            
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
         MVI   OPTION,0                                                         
         MVC   WCOGRP,PROWGR                                                    
*                                                                               
VREC4    NI    WCOSTAT,X'FD'       TURN OFF NON-COMMISSIONABLE BIT              
         CLI   PROCOM,C'N'         IF SET TO N                                  
         BNE   VREC6                                                            
         OI    WCOSTAT,WCOSNONC    TURN IT ON                                   
*                                                                               
VREC6    MVI   WCOTYPE,C' '                                                     
         NI    WCOSTAT,X'F7'       TURN OFF HOURS BIT                           
         LA    R2,PROTYPH          TYPE IS OPTIONAL                             
         CLI   5(R2),0                                                          
         BE    VREC8                                                            
         MVC   WCOTYPE,8(R2)                                                    
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'O'          OUT-OF-POCKET                                
         BE    VREC8                                                            
         CLI   8(R2),C'P'          PRE-BILL                                     
         BE    VREC8                                                            
         CLI   8(R2),C'M'          MEDIA TRANSFER                               
         BE    VREC8                                                            
         CLI   8(R2),C'R'          RETAINER                                     
         BE    VREC8                                                            
         CLI   8(R2),C'T'          TIME                                         
         BNE   ERREND                                                           
         OI    WCOSTAT,WCOSHCOE    TURN ON WCOSHCOE FOR TIME                    
*                                                                               
VREC8    NI    WCOSTAT,X'FE'       TURN OFF MEDIA TRANSFER BIT                  
         LA    R2,PROTRANH                                                      
         CLI   8(R2),C'Y'          IF SET TO YES, TURN IT ON                    
         BNE   VREC10                                                           
         OI    WCOSTAT,WCOSMEDT                                                 
*                                                                               
VREC10   LA    R2,PROSTATH                                                      
         CLI   5(R2),0             STATUS IS OPTIONAL                           
         BE    VREC12                                                           
         MVI   ERROR,INVALID                                                    
         NI    WCOSTAT2,X'FF'-WCOSINA                                           
         CLI   8(R2),C'A'          IF ACTIVE, LEAVE STATUS AS ACTIVE            
         BE    VREC12                                                           
         CLI   8(R2),C'I'          IF NOT, CHANGE STATUS                        
         BNE   ERREND                                                           
         OI    WCOSTAT2,WCOSINA                                                 
*                                                                               
VREC12   NI    WCOSTAT2,X'FF'-WCOSART                                           
         LA    R2,PROARATH                                                      
         CLI   8(R2),C'Y'          IF SET TO YES, TURN IT ON                    
         BNE   VREC14                                                           
         OI    WCOSTAT2,WCOSART                                                 
*                                                                               
VREC14   NI    WCOSTAT2,X'FF'-WCOSINC                                           
         LA    R2,PROINCH                                                       
         CLI   8(R2),C'I'          IF SET TO 'I', TURN IT ON                    
         BNE   VREC16                                                           
         OI    WCOSTAT2,WCOSINC                                                 
*                                                                               
VREC16   NI    WCOSTAT2,X'FF'-WCOSTHRD                                          
         LA    R2,PROTHRH                                                       
         CLI   8(R2),C'Y'          IF SET TO YES, TURN IT ON                    
         BNE   VREC18                                                           
         OI    WCOSTAT2,WCOSTHRD                                                
*                                                                               
VREC18   GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 PERSIN                                                           
*                                                                               
         MVI   ELCODE,NAMELQ       CHECK IF EXISTING ELEMENT                    
         GOTO1 REMELEM                                                          
         LA    R2,PROLNMH          SAVE LONG NAME                               
         CLI   5(R2),0             IS LENGTH ZERO                               
         BE    VRECX               YES, DON'T ADD NAME EL                       
         GOTO1 NAMEIN                                                           
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
*              DISPLAY KEY                                                      
*                                                                               
DKEY     NTR1                                                                   
         L     R4,AIO                                                           
         MVC   PROWRK,4(R4)                                                     
         LA    R2,PROWRKH                                                       
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
*                                                                               
DREC     NTR1                                                                   
         MVI   ELCODE,WCOELQ                                                    
         BAS   RE,GETELIO                                                       
         USING WCOELD,R6                                                        
         LA    R2,PRODESCH         SHOW DESCRIPTION                             
         MVC   8(15,R2),WCODESC                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 PERSOUT             SHOW PERSON                                  
*                                                                               
         MVC   PROLACT,SPACES      SHOW ACTIVITY DATE                           
         MVC   PROLACT(14),=C'LAST ACTIVITY:'                                   
         MVC   PROLACT+15(20),WORK+20                                           
         OI    PROLACTH+6,X'80'                                                 
*                                                                               
         MVC   PROLNM,SPACES                                                    
         LA    R2,PROLNMH          SHOW LONG NAME                               
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         GOTO1 NAMEOUT                                                          
*                                                                               
         MVC   PROWGR,SPACES       SHOW WORK GROUP AND NAME                     
         MVC   PROWGRN,SPACES                                                   
         OI    PROWGRH+6,X'80'                                                  
         OI    PROWGRNH+6,X'80'                                                 
         LA    R2,PROWGRH                                                       
         CLI   WCOGRP,X'41'                                                     
         BL    DREC2                                                            
*                                                                               
         MVC   8(1,R2),WCOGRP                                                   
         MVC   SAVEKEY1,KEY                                                     
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING WGRRECD,R4                                                       
         MVI   WGRKTYP,WGRKTYPQ                                                 
         MVI   WGRKSUB,WGRKSUBQ                                                 
         MVC   WGRKCPY(3),CUL                                                   
         MVC   WGRKCODE,WCOGRP                                                  
         GOTO1 READ                                                             
         GOTO1 SETNAME,DMCB,AIO,PROWGRN                                         
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
*                                                                               
DREC2    LA    R2,PROCOMH          SHOW IF COMMISSIONABLE                       
         MVI   8(R2),C'Y'                                                       
         OI    6(R2),X'80'                                                      
         TM    WCOSTAT,WCOSNONC                                                 
         BNO   *+8                                                              
         MVI   8(R2),C'N'                                                       
*                                                                               
         LA    R2,PROTYPH          SHOW TYPE                                    
         MVC   8(1,R2),WCOTYPE                                                  
         CLI   8(R2),C' '                                                       
         BH    *+8                                                              
         MVI   8(R2),C'O'                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,PROTRANH         SHOW IF MEDIA TRANSFER                       
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'                                                      
         TM    WCOSTAT,WCOSMEDT                                                 
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
         LA    R2,PROSTATH         SHOW STATUS                                  
         MVI   8(R2),C'A'                                                       
         OI    6(R2),X'80'                                                      
         TM    WCOSTAT2,WCOSINA                                                 
         BZ    *+8                                                              
         MVI   8(R2),C'I'                                                       
*                                                                               
         LA    R2,PROARATH         SHOW IF ADJUSTMENT NEEDED                    
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'                                                      
         TM    WCOSTAT2,WCOSART                                                 
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
         LA    R2,PROINCH          SHOW IF INTERNAL INCOME                      
         CLI   DDS,C'Y'            IS THIS A DDS TERMINAL ?                     
         BE    *+8                 YES, SKIP OVER                               
         OI    1(R2),X'20'         NO, PROTECT THIS FIELD                       
         MVI   8(R2),C' '                                                       
         OI    6(R2),X'80'                                                      
         TM    WCOSTAT2,WCOSINC                                                 
         BZ    *+8                                                              
         MVI   8(R2),C'I'                                                       
*                                                                               
         LA    R2,PROTHRH          SHOW 3RD PARTY FLAG                          
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'                                                      
         TM    WCOSTAT2,WCOSTHRD                                                
         BNO   *+8                                                              
         MVI   8(R2),C'Y'                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
                                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
                                                                                
MAXWC    EQU   1400                                                             
                                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
                                                                                
ERREND   GOTO1 VERRCUR                                                          
                                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
TRTTAB   DC    256X'00'                                                         
         ORG   TRTTAB+C'^'                                                      
         DC    X'FF'                                                            
         ORG                                                                    
*                                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
*                                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROFED                                                       
*                                                                               
POINTERS DS    CL(8*54+1)          PASSIVE POINTER BLOCK                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052ACPRO0E   03/21/12'                                      
         END                                                                    
