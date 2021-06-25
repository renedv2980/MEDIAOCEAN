*          DATA SET ACNV05YR   AT LEVEL 054 AS OF 09/19/00                      
*PHASE ACNV5YRA ACNV05YR                                                        
         TITLE 'YRTSEC CONVERSION HOOK'                                         
*                                                                               
ACNV05YR CSECT                                                                  
         PRINT NOGEN                                                            
         USING ACNVD,R9                                                         
         NMOD1 0,*HOOK*,RA                                                      
         EJECT                                                                  
***********************************************************************         
* CONVERSION INITIALIZATION ROUTINE                                   *         
***********************************************************************         
*                                                                               
CNVIN    CLI   MODE,INITTAB                                                     
         BNE   ACCHG                                                            
         L     R1,=A(ACTABL)                                                    
         ST    R1,AACTABL                                                       
         BAS   RE,DYNTAB           BUILD DYNAMIC TABLE                          
         BAS   RE,BLDN2P           BUILD NEW 2P ACCOUNTS IN TABLE               
*                                                                               
XIT      XIT1  ,                   RETURN TO CONVERSION TO COMPLETE             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* BUILD DYNAMIC TABLE                                                 *         
***********************************************************************         
*                                                                               
DYNTAB   NTR1  ,                                                                
         XC    ACXNUM,ACXNUM          CLEAR NUMBER IN TABLE                     
         L     R2,AACTABL                                                       
         USING TABD,R2                                                          
         LA    R3,DKEY                                                          
         USING ACTRECD,R3                                                       
         MVC   DKEY,SPACE             READ IN 2P ACCOUNTS                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA(2),=C'2P'                                                
*                                                                               
DYN3     GOTO1 ADMGR,DMHI                                                       
         CLC   DIR(3),DKEY                                                      
         BNE   XIT                                                              
         LA    R3,DIR                                                           
         MVC   TABOLD,ACTKULA         COPY OLD ACCOUNT                          
         MVC   TABNEW,SPACE                                                     
*                                                                               
         LA    R2,TABLNQ(R2)                                                    
         MVI   0(R2),X'FF'                                                      
         L     RF,ACXNUM              INCREMENT # OF ACCOUNTS                   
         LA    RF,1(RF)               STORED IN TABLE                           
         ST    RF,ACXNUM                                                        
*                                                                               
DYN7     MVC   DKEY,SPACE             SKIP TO NEXT ACCOUNT                      
         MVC   DKEY(L'ACTKCULA),DIR                                             
         LA    R3,DKEY                                                          
         LA    R1,ACTKACT+(L'ACTKACT-1)                                         
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         AHI   RF,1                                                             
         STC   RF,0(R1)                                                         
         B     DYN3                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD NEW 2P KEYS BASED ON PERSON RECORDS                           *         
***********************************************************************         
*                                                                               
BLDN2P   NTR1  ,                                                                
         USING TABD,R3                                                          
         L     R3,AACTABL                                                       
                                                                                
         USING PERRECD,R2                                                       
BLD05    CLC   TABOUL,=C'2P'          END OF ACCOUNT TABLE                      
         BNE   XIT                                                              
         LA    R2,DKEY                                                          
         MVC   PERKEY,SPACE                                                     
         MVI   PERKTYP,PERKTYPQ       READ PERSON RECORDS X'0F'                 
         MVC   PERKCPY,COMPANY                                                  
         MVC   PERKCODE,TABOPER       TAKE PERSON CODE FROM 2P                  
*                                                                               
         GOTO1 ADMGR,DMHI             LOOK UP PERSON RECORD                     
         CLC   DKEY,DIR                                                         
         BE    BLD10                                                            
*                                                                               
         MVC   TABNUL,TABOUL                                                    
         MVC   TABNOFF,DFLTOFF        DEFAULT OFFICE                            
         MVC   TABNDEP,=C'MA'         DEFAULT DEPTARTMENT                       
         MVC   TABNPER,TABOPER        PERSON                                    
         B     BLD50                  NEXT TABLE ENTRY                          
*                                                                               
BLD10    L     R2,AIO3                                                          
         GOTO1 ADMGR,DMGET                                                      
         LA    R4,PERRFST                                                       
*                                                                               
BLD20    CLI   0(R4),0                END OF RECORD?                            
         BE    BLD50                   YES                                      
         CLI   0(R4),LOCELQ           LOCATION ELEMENT X'83'?                   
         BE    BLD30                   YES                                      
BLD25    SR    R0,R0                                                            
         IC    R0,1(R4)               BUMP TO NEXT ELEMENT                      
         AR    R4,R0                                                            
         B     BLD20                                                            
*                                                                               
         USING LOCELD,R4                                                        
BLD30    MVC   TABNEW,SPACE           CLEAR                                     
         MVC   TABNUL,TABOUL          UNIT/LEDGER                               
         MVC   TABNOFF,LOCOFF         OFFICE                                    
         MVC   TABNDEP,LOCDEPT        DEPTARTMENT                               
         MVC   TABNPER,TABOPER        PERSON                                    
         B     BLD25                                                            
*                                                                               
BLD50    LA    R3,TABLNQ(R3)                                                    
         B     BLD05                  NEXT ENTRY IN THE ACCOUNT TABLE           
*                                                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ACCOUNT CHANGES                                                     *         
***********************************************************************         
*                                                                               
ACCHG    CLI   MODE,CHNGACC                                                     
         BNE   ACCHGX                                                           
*      --------------------------------------------                             
         L     R2,HKNEWA                                                        
         CLC   SRCHARG(2),=C'1P'   TEST 1P ACCOUNT                              
         BNE   ACCHG05                                                          
         MVC   0(14,R2),=CL14'1P999999999999'                                   
         B     XIT                                                              
*      --------------------------------------------                             
ACCHG05  CLC   SRCHARG(2),=C'2P'                                                
         BNE   ACCHGX                                                           
*                                                                               
         USING TABD,R3                                                          
         L     R3,AACTABL                                                       
*                                                                               
         L     RF,ACXNUM           SET NUMBER IN 2P TABLE                       
         CHI   RF,MXACC            TEST MAX                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACCH05   CLC   TABOLD,SRCHARG                                                   
         BNE   ACCH50                                                           
         BAS   RE,CHGOFF              OFFICE                                    
         MVC   0(L'ACCN,R2),TABNEW    MOVE NEW ACCOUNT INTO NEW HOOK            
         B     ACCHGX                                                           
*                                                                               
ACCH50   LA    R3,TABLNQ(R3)                                                    
         BCT   RF,ACCH05                                                        
*        DC    H'0'                                                             
*      --------------------------------------------                             
ACCHGX   B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT THE OFFICE   -  WHEN CALLED R3 POINTS TABLE ENTRY           *         
***********************************************************************         
         USING TABD,R3                                                          
CHGOFF   NTR1                                                                   
*                                                                               
         CLC   TABNOFF,DFLTOFF           DEFAULT OFFICE                         
         BE    XIT                                                              
*                                                                               
         L     R2,OFFTAB                 START OF OFFICE TABLE                  
         L     R4,OFFNUM                 NUMBER OF RECORDS TABLE                
CHOFF05  CLC   TABNOFF(1),OFFO-OFFS(R2)  FIND OLD OFFICE IN TABLE               
         BNE   CHOFF10                                                          
         MVC   TABNOFF,OFFN-OFFS(R2)     BRING IN NEW OFFICE                    
         B     XIT                                                              
CHOFF10  LA    R2,OFFLNQ(R2)             NEXT TABLE ENTRY                       
         BCT   R4,CHOFF05                                                       
         DC    H'0'                      DIDN'T FIND OFFICE, SHOULD             
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND WORK AREA                                             *         
***********************************************************************         
*                                                                               
AACTABL  DS    A                   ADDRESS OF ACCOUNT CONVERSION TABLE          
ACXNUM   DS    F                   EXPANDED TABLE ENTRY NUMBER                  
*                                                                               
FOUT     DCB   DDNAME=FOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=FB,LRECL=80,BLKSIZE=80                                     
         LTORG                                                                  
*                                                                               
MXACC    EQU   8000                MAXIMUM NUMBER OF ACCOUNT                    
ACXTAB   DS    (MXACC)CL(ACCLNQ)      EXPANDED TABLE FOR CONVERSION             
         EJECT                                                                  
***********************************************************************         
* ACCOUNT CONVERSION TABLE                                            *         
***********************************************************************         
*                                                                               
ACTABL   DS    0CL28                  OLD/NEW                                   
         DS    (MXACC)CL(L'ACTABL)                                              
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
*                                                                               
TABD     DSECT                                                                  
TABOLD   DS    0CL14               OLD ACCOUNT                                  
TABOUL   DS    CL2                                                              
TABOPER  DS    CL12                PERSON                                       
TABNEW   DS    0CL14               NEW ACCOUNT                                  
TABNUL   DS    CL2                                                              
TABNOFF  DS    CL2                 OFFICE / DEPT                                
TABNDEP  DS    CL2                 OFFICE / DEPT                                
TABNPER  DS    CL8                 PERSON                                       
TABLNQ   EQU   *-TABD                                                           
*                                                                               
ACNVD    DSECT                                                                  
* ACNVWORK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACNVWORK                                                       
         PRINT ON                                                               
* ACNVDSECT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACNVDSECT                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACNV05YR  09/19/00'                                      
         END                                                                    
